open Causalis_data_source

let with_csv content callback =
  let path = Filename.temp_file "causalis-historical" ".csv" in
  let channel = open_out_bin path in
  output_string channel content;
  close_out channel;
  Fun.protect
    ~finally:(fun () -> Sys.remove path)
    (fun () -> callback path)

let timestamp value =
  match Ptime.of_rfc3339 value with
  | Ok (timestamp, _, _) -> timestamp
  | Error _ -> Alcotest.fail "test timestamp did not parse"

let check_ok (result : (Historical_csv.t, Historical_csv.error) result) =
  match result with
  | Ok source -> source
  | Error error -> Alcotest.failf "%s: %s" error.path error.message

let test_range_skip_and_reset () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:15:00+05:30,10,12,9,11,100,4\n"
    ^ "2025-01-01T09:16:00+05:30,11,13,10,12,,\n"
    ^ "2025-01-01T09:17:00+05:30,,13,10,12,120,5\n"
    ^ "2025-01-01T09:18:00+05:30,12,14,11,13,130,6\n"
  in
  with_csv csv (fun path ->
      let source =
        Historical_csv.load_files
          ~from_timestamp:(timestamp "2025-01-01T09:16:00+05:30")
          ~until_timestamp:(timestamp "2025-01-01T09:18:00+05:30")
          ~instrument:"ETF" [ path ]
        |> check_ok
      in
      let bars = Historical_csv.bars source in
      Alcotest.(check int) "one accepted bar in half-open range" 1
        (List.length bars);
      let bar = List.hd bars in
      Alcotest.(check string) "original offset timestamp"
        "2025-01-01T09:16:00+05:30"
        (Historical_csv.source_timestamp bar);
      Alcotest.(check int) "logical instant after filtering and skipping" 0
        (Historical_csv.logical_instant bar);
      Alcotest.(check bool) "missing volume remains absent" true
        (Historical_csv.volume bar = None);
      Alcotest.(check bool) "missing open interest remains absent" true
        (Historical_csv.open_interest bar = None);
      Alcotest.(check bool) "initially not exhausted" false
        (Historical_csv.exhausted source);
      Alcotest.(check bool) "one next result" true
        (Historical_csv.next source <> None);
      Alcotest.(check bool) "exhausted after consuming row" true
        (Historical_csv.exhausted source);
      Historical_csv.reset source;
      Alcotest.(check bool) "reset restores replay" true
        (Historical_csv.next source <> None))

let test_incomplete_rows_are_skipped_before_indexing () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:15:00+05:30,10,12,9,11,100,4\n"
    ^ "2025-01-01T09:16:00+05:30,,13,10,12,120,5\n"
    ^ "2025-01-01T09:17:00+05:30,12,14,11,13,130,6\n"
  in
  with_csv csv (fun path ->
      let source =
        Historical_csv.load_files ~instrument:"ETF" [ path ] |> check_ok
      in
      let bars = Historical_csv.bars source in
      Alcotest.(check int) "incomplete OHLC row skipped" 2 (List.length bars);
      Alcotest.(check int) "accepted rows get contiguous logical instants" 1
        (Historical_csv.logical_instant (List.nth bars 1)))

let test_duplicate_timestamp_is_rejected_with_record () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:15:00+05:30,10,12,9,11,100,4\n"
    ^ "2025-01-01T09:15:00+05:30,11,13,10,12,120,5\n"
  in
  with_csv csv (fun path ->
      match Historical_csv.load_files ~instrument:"ETF" [ path ] with
      | Ok _ -> Alcotest.fail "duplicate timestamp should fail"
      | Error error ->
          Alcotest.(check bool) "error identifies source file" true
            (String.equal error.path path);
          Alcotest.(check (option int)) "error identifies CSV record" (Some 3)
            error.record)

let test_out_of_order_timestamp_is_rejected () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:16:00+05:30,10,12,9,11,100,4\n"
    ^ "2025-01-01T09:15:00+05:30,11,13,10,12,120,5\n"
  in
  with_csv csv (fun path ->
      match Historical_csv.load_files ~instrument:"ETF" [ path ] with
      | Ok _ -> Alcotest.fail "out-of-order timestamp should fail"
      | Error error ->
          Alcotest.(check (option int)) "error identifies CSV record" (Some 3)
            error.record)

let test_malformed_timestamp_is_rejected () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "not-a-time,10,12,9,11,100,4\n"
  in
  with_csv csv (fun path ->
      match Historical_csv.load_files ~instrument:"ETF" [ path ] with
      | Ok _ -> Alcotest.fail "malformed timestamp should fail"
      | Error error ->
          Alcotest.(check bool) "error identifies source file" true
            (String.equal error.path path);
          Alcotest.(check (option int)) "error identifies CSV record" (Some 2)
            error.record)

let test_timezone_offset_is_required () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:15:00,10,12,9,11,100,4\n"
  in
  with_csv csv (fun path ->
      match Historical_csv.load_files ~instrument:"ETF" [ path ] with
      | Ok _ -> Alcotest.fail "timezone-less timestamp should fail"
      | Error error ->
          Alcotest.(check string) "timezone offset is required"
            "invalid RFC3339 timestamp" error.message)

let test_malformed_number_is_rejected () =
  let csv =
    "timestamp,open,high,low,close,volume,open_interest\n"
    ^ "2025-01-01T09:15:00+05:30,10,12,9,bad,100,4\n"
  in
  with_csv csv (fun path ->
      match Historical_csv.load_files ~instrument:"ETF" [ path ] with
      | Ok _ -> Alcotest.fail "malformed numeric value should fail"
      | Error error ->
          Alcotest.(check bool) "malformed field identified" true
            (String.equal error.message "invalid numeric value for close");
          Alcotest.(check (option int)) "error identifies CSV record" (Some 2)
            error.record)

let test_missing_file_is_reported () =
  let path = Filename.temp_file "causalis-missing" ".csv" in
  Sys.remove path;
  match Historical_csv.load_files ~instrument:"ETF" [ path ] with
  | Ok _ -> Alcotest.fail "missing file should fail"
  | Error error ->
      Alcotest.(check bool) "error identifies source file" true
        (String.equal error.path path);
      Alcotest.(check bool) "error has no record number" true
        (error.record = None)

let () =
  Alcotest.run "historical_csv"
    [ ( "historical source",
        [ Alcotest.test_case "range, optional fields, and reset" `Quick
            test_range_skip_and_reset;
          Alcotest.test_case "incomplete row indexing" `Quick
            test_incomplete_rows_are_skipped_before_indexing;
          Alcotest.test_case "duplicate timestamp" `Quick
            test_duplicate_timestamp_is_rejected_with_record;
          Alcotest.test_case "out-of-order timestamp" `Quick
            test_out_of_order_timestamp_is_rejected;
          Alcotest.test_case "malformed timestamp" `Quick
            test_malformed_timestamp_is_rejected;
          Alcotest.test_case "explicit timezone offset" `Quick
            test_timezone_offset_is_required;
          Alcotest.test_case "malformed number" `Quick
            test_malformed_number_is_rejected;
          Alcotest.test_case "missing file" `Quick test_missing_file_is_reported ] ) ]