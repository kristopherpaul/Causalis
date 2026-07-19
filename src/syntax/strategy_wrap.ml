let read_file path =
  In_channel.with_open_text path In_channel.input_all

let () =
  if Array.length Sys.argv <> 2 then begin
    prerr_endline "usage: causalis-strategy-wrap FILE.strategy";
    exit 2
  end;
  let path = Sys.argv.(1) in
  let source = read_file path in
  Printf.printf "[%%%%strategy (%S, %S)]\n" path source