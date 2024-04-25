let read_file file =
  In_channel.with_open_bin file In_channel.input_all

let read_lines file =
  let ic = open_in file in
  let rec read_lines_aux acc =
    try
      let line = input_line ic in
      read_lines_aux (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines_aux []

let read_dictionary file_path =
  let lines = read_lines file_path in
  let rec parse_lines acc = function
    | [] -> acc
    | line :: rest ->
        match String.split_on_char ':' line with
        | [word; defs] ->
            let defs = String.split_on_char ';' defs |> List.map String.trim in
            let updated_acc = List.fold_left (fun acc defn -> Dict.add_def acc ~word ~def:defn) acc defs in
            parse_lines updated_acc rest
        | _ -> parse_lines acc rest (* Skip malformed lines *)
  in
  parse_lines Dict.empty lines

let () =
  let dictionary = read_dictionary Sys.argv.(1) in
  Printf.printf "entries:\n";
  let rec handle_input () =
    try
      let input = input_line stdin in
      match String.split_on_char ' ' input with
      | ["entries"] ->
          Dict.get_entries dictionary
          |> List.sort String.compare
          |> List.iter (Printf.printf "- %s\n");
          handle_input ()
      | ["definition"; word] ->
          Printf.printf "definition %s:\n" word;
          Dict.get_defs dictionary word
          |> List.sort String.compare
          |> List.iter (Printf.printf "- %s\n");
          handle_input ()
      | ["synonyms"; word] ->
          Printf.printf "synonyms %s:\n" word;
          Dict.get_synonyms dictionary word
          |> List.sort String.compare
          |> List.iter (Printf.printf "- %s\n");
          handle_input ()
      | _ -> handle_input ()
    with
    | End_of_file -> ()  (* End of input *)
  in
  handle_input ()
