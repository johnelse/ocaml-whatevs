open Cmdliner
open Markov_names_libs

let file_arg =
  let doc = "The path to the file to process" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let process_term = Term.(pure Process.process $ file_arg)

let info =
  let doc = "Process a file" in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports somewhere I guess";
  ]
  in
  Term.info "process" ~version:"1.0" ~doc ~exits:Term.default_exits ~man

let () =
  Random.self_init ();
  Printexc.record_backtrace true;
  Term.exit @@ Term.eval (process_term, info)
