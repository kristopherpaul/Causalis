type expression =
  | Variable of string
  | Price of string
  | Weight of string
  | Add of expression * expression
  | Subtract of expression * expression
  | Multiply of expression * expression
  | Divide of expression * expression
  | Greater of expression * expression
  | Greater_equal of expression * expression
  | Less of expression * expression
  | Less_equal of expression * expression
  | Pre of expression
  | Sma of int * expression
  | Ema of int * expression
  | If of expression * expression * expression

type declaration =
  | Input of string
  | Let of string * expression
  | Output of string * expression

type strategy = { name : string; declarations : declaration list }

type error = { line : int; column : int; message : string }

exception Parse_error of error

type token_kind =
  | Identifier of string
  | Number of string
  | Symbol of string
  | End

type token = { kind : token_kind; line : int; column : int }

type lexer = {
  source : string;
  mutable offset : int;
  mutable line : int;
  mutable column : int;
}

let fail_at line column message = raise (Parse_error { line; column; message })

let current lexer =
  if lexer.offset >= String.length lexer.source then '\000'
  else lexer.source.[lexer.offset]

let advance lexer =
  let character = current lexer in
  if character <> '\000' then begin
    lexer.offset <- lexer.offset + 1;
    if character = '\n' then begin
      lexer.line <- lexer.line + 1;
      lexer.column <- 1
    end else
      lexer.column <- lexer.column + 1
  end;
  character

let is_digit character = character >= '0' && character <= '9'

let is_identifier_start character =
  (character >= 'a' && character <= 'z')
  || (character >= 'A' && character <= 'Z')
  || character = '_'

let is_identifier_character character =
  is_identifier_start character || is_digit character

let read_while lexer predicate =
  let start = lexer.offset in
  while current lexer <> '\000' && predicate (current lexer) do
    ignore (advance lexer)
  done;
  String.sub lexer.source start (lexer.offset - start)

let rec skip_space lexer =
  match current lexer with
  | ' ' | '\t' | '\r' | '\n' -> ignore (advance lexer); skip_space lexer
  | '/' when lexer.offset + 1 < String.length lexer.source
             && lexer.source.[lexer.offset + 1] = '/' ->
      while current lexer <> '\000' && current lexer <> '\n' do
        ignore (advance lexer)
      done;
      skip_space lexer
  | _ -> ()

let next_token lexer =
  skip_space lexer;
  let line = lexer.line in
  let column = lexer.column in
  let make kind = { kind; line; column } in
  match current lexer with
  | '\000' -> make End
  | character when is_identifier_start character ->
      make (Identifier (read_while lexer is_identifier_character))
  | character when is_digit character ->
      let start = lexer.offset in
      ignore (read_while lexer is_digit);
      if current lexer = '.' && lexer.offset + 1 < String.length lexer.source
         && is_digit lexer.source.[lexer.offset + 1] then begin
        ignore (advance lexer);
        ignore (read_while lexer is_digit)
      end;
      let number = String.sub lexer.source start (lexer.offset - start) in
      make (Number number)
  | ('=' | '>' | '<') as character ->
      ignore (advance lexer);
      let symbol =
        if current lexer = '=' then begin
          ignore (advance lexer);
          String.make 1 character ^ "="
        end else
          String.make 1 character
      in
      make (Symbol symbol)
  | ('+' | '-' | '*' | '/' | '{' | '}' | '(' | ')' | ':' | ';' | ',' | '%') as character ->
      ignore (advance lexer);
      make (Symbol (String.make 1 character))
  | character ->
      fail_at line column (Printf.sprintf "unexpected character %C" character)

type parser = { tokens : token array; mutable index : int }

let token parser = parser.tokens.(parser.index)

let consume parser =
  let current = token parser in
  if parser.index < Array.length parser.tokens - 1 then parser.index <- parser.index + 1;
  current

let fail parser message =
  let current = token parser in
  fail_at current.line current.column message

let accept_symbol parser symbol =
  match (token parser).kind with
  | Symbol actual when String.equal actual symbol -> ignore (consume parser); true
  | _ -> false

let expect_symbol parser symbol =
  if not (accept_symbol parser symbol) then
    fail parser (Printf.sprintf "expected %S" symbol)

let accept_keyword parser keyword =
  match (token parser).kind with
  | Identifier actual when String.equal actual keyword -> ignore (consume parser); true
  | _ -> false

let expect_keyword parser keyword =
  if not (accept_keyword parser keyword) then
    fail parser (Printf.sprintf "expected %S" keyword)

let expect_identifier parser =
  match consume parser with
  | { kind = Identifier name; _ } -> name
  | current -> fail_at current.line current.column "expected an identifier"

let expect_number parser =
  match consume parser with
  | { kind = Number number; _ } -> number
  | current -> fail_at current.line current.column "expected a number"

let rec parse_expression parser = parse_conditional parser

and parse_conditional parser =
  if accept_keyword parser "if" then begin
    let condition = parse_comparison parser in
    expect_keyword parser "then";
    let if_true = parse_expression parser in
    expect_keyword parser "else";
    let if_false = parse_expression parser in
    If (condition, if_true, if_false)
  end else
    parse_comparison parser

and parse_comparison parser =
  let left = parse_addition parser in
  match (token parser).kind with
  | Symbol ">" -> ignore (consume parser); Greater (left, parse_addition parser)
  | Symbol ">=" -> ignore (consume parser); Greater_equal (left, parse_addition parser)
  | Symbol "<" -> ignore (consume parser); Less (left, parse_addition parser)
  | Symbol "<=" -> ignore (consume parser); Less_equal (left, parse_addition parser)
  | _ -> left

and parse_addition parser =
  let rec loop expression =
    match (token parser).kind with
    | Symbol "+" ->
        ignore (consume parser);
        loop (Add (expression, parse_multiplication parser))
    | Symbol "-" ->
        ignore (consume parser);
        loop (Subtract (expression, parse_multiplication parser))
    | _ -> expression
  in
  loop (parse_multiplication parser)

and parse_multiplication parser =
  let rec loop expression =
    match (token parser).kind with
    | Symbol "*" ->
        ignore (consume parser);
        loop (Multiply (expression, parse_primary parser))
    | Symbol "/" ->
        ignore (consume parser);
        loop (Divide (expression, parse_primary parser))
    | _ -> expression
  in
  loop (parse_primary parser)

and parse_primary parser =
  match consume parser with
  | { kind = Number number; _ } ->
      if accept_symbol parser "%" then Weight number else Price number
  | { kind = Symbol "-"; line; column } ->
      (match consume parser with
      | { kind = Number number; _ } when accept_symbol parser "%" -> Weight ("-" ^ number)
      | _ -> fail_at line column "negative literals are supported only for percentage weights")
  | { kind = Identifier "price"; _ } ->
      expect_symbol parser "(";
      let value = expect_number parser in
      expect_symbol parser ")";
      Price value
  | { kind = Identifier "pre"; _ } ->
      expect_symbol parser "(";
      let expression = parse_expression parser in
      expect_symbol parser ")";
      Pre expression
  | { kind = Identifier ("sma" as operator); _ }
  | { kind = Identifier ("ema" as operator); _ } ->
      expect_symbol parser "(";
      let period_token = token parser in
      let period =
        try int_of_string (expect_number parser)
        with Failure _ ->
          fail_at period_token.line period_token.column "period must be a positive integer"
      in
      if period <= 0 then
        fail_at period_token.line period_token.column "period must be a positive integer";
      expect_symbol parser ",";
      let expression = parse_expression parser in
      expect_symbol parser ")";
      if operator = "sma" then Sma (period, expression) else Ema (period, expression)
  | { kind = Identifier name; _ } -> Variable name
  | { kind = Symbol "("; _ } ->
      let expression = parse_expression parser in
      expect_symbol parser ")";
      expression
  | current -> fail_at current.line current.column "expected a strategy expression"

let parse source =
  let lexer = { source; offset = 0; line = 1; column = 1 } in
  let rec lex collected =
    let next = next_token lexer in
    match next.kind with
    | End -> Array.of_list (List.rev (next :: collected))
    | _ -> lex (next :: collected)
  in
  let parser = { tokens = lex []; index = 0 } in
  expect_keyword parser "strategy";
  let name = expect_identifier parser in
  if name = "" || name.[0] < 'A' || name.[0] > 'Z' then
    fail parser "strategy names must start with an uppercase letter";
  expect_symbol parser "{";
  let rec declarations collected =
    if accept_symbol parser "}" then List.rev collected
    else
      let declaration =
        if accept_keyword parser "input" then begin
          let name = expect_identifier parser in
          expect_symbol parser ":";
          expect_keyword parser "price";
          expect_symbol parser ";";
          Input name
        end else if accept_keyword parser "let" then begin
          let name = expect_identifier parser in
          expect_symbol parser "=";
          let expression = parse_expression parser in
          expect_symbol parser ";";
          Let (name, expression)
        end else if accept_keyword parser "output" then begin
          let name = expect_identifier parser in
          expect_symbol parser ":";
          expect_keyword parser "weight";
          expect_symbol parser "=";
          let expression = parse_expression parser in
          expect_symbol parser ";";
          Output (name, expression)
        end else
          fail parser "expected input, let, output, or the end of the strategy"
      in
      declarations (declaration :: collected)
  in
  let declarations = declarations [] in
  (match (token parser).kind with
  | End -> ()
  | _ -> fail parser "unexpected content after strategy declaration");
  if not (List.exists (function Output _ -> true | _ -> false) declarations) then
    fail parser "a strategy must declare at least one output";
  { name; declarations }