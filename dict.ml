type t = (string * string list) list

let empty = []

let rec add_def (dict : t) ~word:(word : string) ~def:(def : string) : t =
  match dict with
  | [] -> [(word, [def])]
  | (w, defs) :: tl ->
      if w = word then (word, def :: defs) :: tl
      else (w, defs) :: add_def tl ~word ~def

let rec get_defs dict word =
  match dict with
  | [] -> []
  | (w, defs) :: tl ->
      if w = word then defs else get_defs tl word

let rec get_synonyms dict word =
  let rec synonyms_helper acc dict word =
    match dict with
    | [] -> acc
    | (w, defs) :: tl ->
        if List.mem word defs && word <> w then
          synonyms_helper (w :: acc) tl word
        else synonyms_helper acc tl word
  in
  synonyms_helper [] dict word

let get_entries dict = List.map (fun (w, _) -> w) dict
