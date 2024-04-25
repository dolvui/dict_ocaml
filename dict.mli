type t

val empty : t

val add_def : t -> word:string -> def:string -> t
val get_defs : t -> string -> string list
val get_synonyms : t -> string -> string list
val get_entries : t -> string list
