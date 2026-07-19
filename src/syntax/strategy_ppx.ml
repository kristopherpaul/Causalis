open Ppxlib
open Causalis_syntax.Strategy_parser

let quote value = Printf.sprintf "%S" value

let module_name_is_valid name =
  String.length name > 0 && name.[0] >= 'A' && name.[0] <= 'Z'

let value_name_is_valid name =
  String.length name > 0
  && ((name.[0] >= 'a' && name.[0] <= 'z') || name.[0] = '_')
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       name

let validate_name loc name =
  if not (value_name_is_valid name) then
    Location.raise_errorf ~loc "strategy port and binding names must be OCaml value identifiers"

let weight_expression weight =
  Printf.sprintf
    "(match Causalis_strategy.Domain.Weight.of_decimal Decimal.(Decimal.of_string %s / of_int 100) with | Ok value -> value | Error message -> invalid_arg message)"
    (quote weight)

let rec lower_expression environment = function
  | Variable name ->
      (match Hashtbl.find_opt environment name with
      | Some generated_name -> generated_name
      | None -> failwith (Printf.sprintf "unknown strategy binding %S" name))
  | Price value ->
      Printf.sprintf
        "Causalis_core.Signal.const ~clock:Causalis_core.Signal.Clock.logical (Causalis_core.Value.Price.of_decimal (Decimal.of_string %s))"
        (quote value)
  | Weight value ->
      Printf.sprintf
        "Causalis_core.Signal.const ~clock:Causalis_core.Signal.Clock.logical %s"
        (weight_expression value)
  | Add (left, right) -> lower_price_binary "+" environment left right
  | Subtract (left, right) -> lower_price_binary "-" environment left right
  | Multiply (left, right) -> lower_price_binary "*" environment left right
  | Divide (left, right) -> lower_price_binary "/" environment left right
  | Greater (left, right) -> lower_price_comparison ">" environment left right
  | Greater_equal (left, right) -> lower_price_comparison ">=" environment left right
  | Less (left, right) -> lower_price_comparison "<" environment left right
  | Less_equal (left, right) -> lower_price_comparison "<=" environment left right
  | Pre expression ->
      Printf.sprintf "Causalis_core.Signal.pre (%s)"
        (lower_expression environment expression)
  | Sma (period, expression) ->
      Printf.sprintf "Causalis_core.Signal.sma %d (%s)" period
        (lower_expression environment expression)
  | Ema (period, expression) ->
      Printf.sprintf "Causalis_core.Signal.ema %d (%s)" period
        (lower_expression environment expression)
  | If (condition, if_true, if_false) ->
      Printf.sprintf "Causalis_core.Signal.select (%s) (%s) (%s)"
        (lower_expression environment condition)
        (lower_expression environment if_true)
        (lower_expression environment if_false)
and lower_price_binary operator environment left right =
  Printf.sprintf
    "Causalis_core.Signal.map2 (fun left right -> Causalis_core.Value.Price.(left %s right)) (%s) (%s)"
    operator (lower_expression environment left) (lower_expression environment right)

and lower_price_comparison operator environment left right =
  Printf.sprintf
    "Causalis_core.Signal.map2 (fun left right -> Decimal.(Causalis_core.Value.Price.to_decimal left %s Causalis_core.Value.Price.to_decimal right)) (%s) (%s)"
    operator (lower_expression environment left) (lower_expression environment right)

let lower_strategy strategy =
  if not (module_name_is_valid strategy.name) then
    failwith "strategy names must be valid OCaml module identifiers";
  let environment = Hashtbl.create 16 in
  let declared_names = Hashtbl.create 16 in
  let bindings = ref [] in
  let inputs = ref [] in
  let outputs = ref [] in
  let next_signal = ref 0 in
  let add_name name =
    validate_name Location.none name;
    if Hashtbl.mem declared_names name then
      failwith (Printf.sprintf "duplicate strategy binding %S" name);
    Hashtbl.add declared_names name ()
  in
  List.iter
    (function
      | Input name ->
          add_name name;
          let port_name = "input_" ^ name in
          let signal_name = Printf.sprintf "__signal_%d" !next_signal in
          incr next_signal;
          Hashtbl.add environment name signal_name;
          inputs :=
            (Printf.sprintf "let %s : Causalis_core.Value.Price.t Causalis_core.Signal.Input.t = Causalis_core.Signal.Input.create ~name:%s ~clock:Causalis_core.Signal.Clock.logical ()\nlet %s = Causalis_core.Signal.input %s"
               port_name (quote name) signal_name port_name,
             name, port_name)
            :: !inputs
      | Let (name, expression) ->
          add_name name;
          let signal_name = Printf.sprintf "__signal_%d" !next_signal in
          incr next_signal;
          let lowered = lower_expression environment expression in
          Hashtbl.add environment name signal_name;
          bindings :=
            Printf.sprintf "let %s = %s" signal_name lowered :: !bindings
      | Output (name, expression) ->
          add_name name;
          let index = List.length !outputs in
          let signal_name = Printf.sprintf "__output_signal_%d" index in
          let output_name = "output_" ^ name in
          let lowered = lower_expression environment expression in
          outputs :=
            (Printf.sprintf "let %s = %s\nlet %s : Causalis_strategy.Domain.Weight.t Causalis_core.Compiler.Output.t = Causalis_core.Compiler.Output.create ~name:%s %s"
               signal_name lowered output_name (quote name) signal_name,
             name, output_name)
            :: !outputs)
    strategy.declarations;
  let inputs = List.rev !inputs in
  let bindings = List.rev !bindings in
  let outputs = List.rev !outputs in
  let module_body =
    List.map (fun (declaration, _, _) -> declaration) inputs
    @ bindings
    @ List.map (fun (declaration, _, _) -> declaration) outputs
    @ [ Printf.sprintf "let input_ports = [%s]"
          (inputs
           |> List.map (fun (_, name, port_name) ->
                  Printf.sprintf "(%s, Causalis_core.Signal.Input.id %s)"
                    (quote name) port_name)
           |> String.concat "; ");
        Printf.sprintf "let outputs = [%s]"
          (outputs
           |> List.map (fun (_, _, output_name) ->
                  Printf.sprintf "Causalis_core.Compiler.Output.pack %s" output_name)
           |> String.concat "; ");
        Printf.sprintf "let output_names = [%s]"
          (outputs |> List.map (fun (_, name, _) -> quote name) |> String.concat "; ") ]
  in
  Printf.sprintf "module %s = struct\n%s\nend"
    strategy.name (String.concat "\n" module_body)

let string_constant expression =
  match expression.pexp_desc with
  | Pexp_constant (Pconst_string (value, _, _)) -> Some value
  | _ -> None

let extension_payload loc expression =
  match expression.pexp_desc with
  | Pexp_tuple [ path_expression; source_expression ] ->
      (match string_constant path_expression, string_constant source_expression with
      | Some path, Some source -> path, source
      | _ -> Location.raise_errorf ~loc "strategy extension expects (source_path, source_text)")
  | _ -> Location.raise_errorf ~loc "strategy extension expects (source_path, source_text)"

let strategy_extension =
  Extension.V2.declare_inline "strategy" Extension.Context.structure_item
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ payload ->
      let source_path, source = extension_payload loc payload in
      try
        let strategy = Causalis_syntax.Strategy_parser.parse source in
        let generated = lower_strategy strategy in
        let lexbuf = Lexing.from_string generated in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = source_path };
        Ppxlib.Parse.implementation lexbuf
      with
      | Parse_error error ->
          let pos =
            { Lexing.pos_fname = source_path;
              pos_lnum = error.line;
              pos_bol = 0;
              pos_cnum = error.column - 1 }
          in
          let source_loc =
            { Location.loc_start = pos; loc_end = pos; loc_ghost = false }
          in
          Location.raise_errorf ~loc:source_loc "%s" error.message
      | Failure message -> Location.raise_errorf ~loc "%s" message)

let () =
  Driver.register_transformation "causalis_strategy"
    ~rules:[ Context_free.Rule.extension strategy_extension ]