let finally f cleanup =
  try
    let result = f () in
    cleanup ();
    result
  with e ->
    cleanup ();
    raise e

type item = char option

module CharMap = Map.Make(Char)
module ItemMap = Map.Make(struct type t = item let compare = compare end)

let new_table () = CharMap.empty

let insert char next table =
  match CharMap.find_opt char table with
  | Some item_map -> begin
    let new_item_map = match ItemMap.find_opt next item_map with
    | Some count -> ItemMap.add next (count + 1) item_map
    | None       -> ItemMap.add next 1 item_map
    in
    CharMap.add char new_item_map table
  end
  | None ->
    CharMap.add char (ItemMap.singleton next 1) table

let process_name name table =
  Seq.fold_left
    (fun table (index, char) ->
      let next =
        if index >= (String.length name - 1)
        then None
        else Some (name.[index + 1])
      in
      insert char next table)
    table (String.to_seqi name)

let generate_table file =
  let channel = open_in file in
  finally
    (fun () ->
      let rec process_all channel table =
        match (try Some (input_line channel) with End_of_file -> None) with
        | Some line -> process_all channel (process_name line table)
        | None      -> table
      in
      new_table () |> process_all channel)
    (fun () -> close_in channel)

let pick_start table =
  let cardinal = CharMap.cardinal table in
  let index = Random.int cardinal in
  List.nth (CharMap.bindings table) index |> fst

let pick_item item_map =
  let total, cumulative_bindings =
    ItemMap.fold
      (fun item count (total, cumulative_bindings) ->
        let new_total = total + count in
        new_total, (item, new_total) :: cumulative_bindings)
      item_map (0, [])
    |> (fun (a, b) -> a, List.rev b)
  in
  let index = Random.int total in
  let rec pick_item index = function
    | []              -> None
    | (item, cumulative_count) :: rest -> begin
      if cumulative_count > index
      then item
      else pick_item index rest
    end
  in
  pick_item index cumulative_bindings

let string_of_chars chars =
  let length = List.length chars in
  let bytes = Bytes.create length in
  List.iteri (fun index char -> Bytes.set bytes index char) chars;
  Bytes.to_string bytes

let generate_name table =
  let start = pick_start table in
  let rec generate_chars acc start table =
    match CharMap.find_opt start table with
    | Some item_map -> begin
      match pick_item item_map with
      | Some character -> generate_chars (character :: acc) character table
      | None           -> acc
    end
    | None -> acc
  in
  let chars =
    generate_chars [] start table
    |> List.rev
  in
  string_of_chars (start :: chars)

let process file =
  Printf.printf "%s\n" "Processing names list...";
  let table = generate_table file in
  Printf.printf "%s" "Press return to generate a name";
  while true do
    ignore (read_line ());
    Printf.printf "%s" (generate_name table)
  done
