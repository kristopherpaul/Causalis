type bar = {
  source_timestamp : string;
  timestamp : Ptime.t;
  logical_instant : int;
  market : Causalis_strategy.Domain.market;
  open_price : Causalis_core.Value.Price.t;
  high_price : Causalis_core.Value.Price.t;
  low_price : Causalis_core.Value.Price.t;
  close_price : Causalis_core.Value.Price.t;
  volume : Decimal.t option;
  open_interest : Decimal.t option;
}

let source_timestamp bar = bar.source_timestamp
let timestamp bar = bar.timestamp
let logical_instant bar = bar.logical_instant
let market bar = bar.market
let open_price bar = bar.open_price
let high_price bar = bar.high_price
let low_price bar = bar.low_price
let close_price bar = bar.close_price
let volume bar = bar.volume
let open_interest bar = bar.open_interest

type t = { bars : bar array; mutable cursor : int }

type error = {
  path : string;
  record : int option;
  message : string;
}

let expected_header =
  [ "timestamp"; "open"; "high"; "low"; "close"; "volume"; "open_interest" ]

let error path record message = Error { path; record; message }

let parse_timestamp value =
  match Ptime.of_rfc3339 value with
  | Ok (timestamp, Some _, _) -> Ok timestamp
  | Ok (_, None, _) -> Error "timestamp must include a UTC offset"
  | Error _ -> Error "invalid RFC3339 timestamp"

let parse_decimal field value =
  let value = String.trim value in
  if String.equal value "" then Ok None
  else
    try Ok (Some (Decimal.of_string value)) with
    | Invalid_argument _ -> Error ("invalid numeric value for " ^ field)

let parse_price field value =
  match parse_decimal field value with
  | Error message -> Error message
  | Ok None -> Ok None
  | Ok (Some decimal) ->
      (try Ok (Some (Causalis_core.Value.Price.of_decimal decimal)) with
      | Invalid_argument _ -> Error (field ^ " must be non-negative"))

let column row index =
  match List.nth_opt row index with
  | Some value -> value
  | None -> ""

let parse_row ~path ~record ~instrument row =
  if List.length row > List.length expected_header then
    error path (Some record) "unexpected extra columns"
  else
    let source_timestamp = String.trim (column row 0) in
    match parse_timestamp source_timestamp with
    | Error message -> error path (Some record) message
    | Ok timestamp ->
        let parsed_prices =
          List.map
            (fun (field, index) -> (field, parse_price field (column row index)))
            [ ("open", 1); ("high", 2); ("low", 3); ("close", 4) ]
        in
        let first_price_error =
          List.find_map
            (fun (_, result) -> match result with Error message -> Some message | Ok _ -> None)
            parsed_prices
        in
        (match first_price_error with
        | Some message -> error path (Some record) message
        | None ->
            let prices =
              List.map
                (fun (_, result) -> match result with Ok price -> price | Error _ -> assert false)
                parsed_prices
            in
            match prices with
            | [ Some open_price; Some high_price; Some low_price; Some close_price ] ->
                let volume_result = parse_decimal "volume" (column row 5) in
                let open_interest_result =
                  parse_decimal "open_interest" (column row 6)
                in
                (match (volume_result, open_interest_result) with
                | Error message, _ | _, Error message -> error path (Some record) message
                | Ok volume, Ok open_interest ->
                    if
                      Option.fold ~none:false ~some:(fun value -> Decimal.(value < zero)) volume
                      || Option.fold ~none:false
                           ~some:(fun value -> Decimal.(value < zero)) open_interest
                    then error path (Some record) "volume and open_interest must be non-negative"
                    else
                      let market =
                        Causalis_strategy.Domain.market
                          ~prices:[ (instrument, close_price) ]
                      in
                      Ok
                        (Some
                           { source_timestamp;
                             timestamp;
                             logical_instant = -1;
                             market;
                             open_price;
                             high_price;
                             low_price;
                             close_price;
                             volume;
                             open_interest }))
            | _ -> Ok None)

let read_file ~instrument path =
  match
    try Ok (open_in_bin path) with
    | Sys_error message -> Error { path; record = None; message }
  with
  | Error error -> Error error
  | Ok channel ->
      let csv = Csv.of_channel ~strip:false channel in
      let close () = Csv.close_in csv in
      (try
         let header = Csv.next csv |> List.map String.trim in
         if header <> expected_header then begin
           close ();
           error path (Some 1) "unexpected CSV header"
         end
         else
           let rec read record previous_timestamp reversed_bars =
             match Csv.next csv with
             | row ->
                 (match parse_row ~path ~record ~instrument row with
                 | Error error -> Error error
                 | Ok parsed ->
                     let timestamp =
                       match parsed with
                       | Some bar -> Some bar.timestamp
                       | None ->
                           (match parse_timestamp (String.trim (column row 0)) with
                           | Ok timestamp -> Some timestamp
                           | Error _ -> None)
                     in
                     (match (previous_timestamp, timestamp) with
                     | Some previous, Some current
                       when Ptime.compare current previous <= 0 ->
                         error path (Some record)
                           "timestamps must be strictly increasing"
                     | _ ->
                         let previous_timestamp =
                           match timestamp with
                           | Some value -> Some value
                           | None -> previous_timestamp
                         in
                         let reversed_bars =
                           match parsed with
                           | Some bar -> bar :: reversed_bars
                           | None -> reversed_bars
                         in
                         read (record + 1) previous_timestamp reversed_bars))
             | exception End_of_file -> Ok (List.rev reversed_bars)
             | exception (Csv.Failure (csv_record, field, message)) ->
                 error path (Some csv_record)
                   (Printf.sprintf "CSV parse error in field %d: %s" field message)
           in
           let result = read 2 None [] in
           close ();
           result
       with
      | End_of_file ->
          close ();
          error path (Some 1) "CSV file is empty"
      | Sys_error message ->
          close ();
          error path None message
      | Csv.Failure (record, field, message) ->
          close ();
          error path (Some record)
            (Printf.sprintf "CSV parse error in field %d: %s" field message))

let load_files ?from_timestamp ?until_timestamp ~instrument paths =
  match (from_timestamp, until_timestamp) with
  | Some first, Some last when Ptime.compare first last > 0 ->
      Error { path = "<range>"; record = None; message = "range start must not follow range end" }
  | _ ->
      let rec load_paths previous_timestamp logical_instant reversed_bars = function
        | [] -> Ok { bars = Array.of_list (List.rev reversed_bars); cursor = 0 }
        | path :: rest ->
            (match read_file ~instrument path with
            | Error error -> Error error
            | Ok file_bars ->
                let rec add_file previous_timestamp logical_instant reversed_bars = function
                  | [] -> load_paths previous_timestamp logical_instant reversed_bars rest
                  | bar :: remaining ->
                      (match previous_timestamp with
                      | Some previous when Ptime.compare bar.timestamp previous <= 0 ->
                          Error
                            { path;
                              record = None;
                              message = "timestamps must be strictly increasing across files" }
                      | _ ->
                          let within_range =
                            (match from_timestamp with
                            | None -> true
                            | Some first -> Ptime.compare bar.timestamp first >= 0)
                            &&
                            (match until_timestamp with
                            | None -> true
                            | Some last -> Ptime.compare bar.timestamp last < 0)
                          in
                          let previous_timestamp = Some bar.timestamp in
                          if within_range then
                            let bar = { bar with logical_instant } in
                            add_file previous_timestamp (logical_instant + 1)
                              (bar :: reversed_bars) remaining
                          else
                            add_file previous_timestamp logical_instant reversed_bars remaining)
                in
                add_file previous_timestamp logical_instant reversed_bars file_bars)
      in
      load_paths None 0 [] paths

let bars source = Array.to_list source.bars

let next source =
  if source.cursor >= Array.length source.bars then None
  else
    let bar = source.bars.(source.cursor) in
    source.cursor <- source.cursor + 1;
    Some bar

let reset source = source.cursor <- 0
let exhausted source = source.cursor >= Array.length source.bars