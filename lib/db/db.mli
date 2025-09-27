type handle

val init_datastore : string -> unit
(* initialize new database directory *)

val get_handle : string -> handle
(* opens database handle *)

val get : handle -> bytes -> bytes option
(* retrieves value by key *)

val put : handle -> bytes -> bytes -> unit
(* insert/update a key-value pair *)

val delete : handle -> bytes -> unit
(* delete key by tombstoning *)

val list_keys : handle -> bytes list
(* list active keys *)

val fold : handle -> (bytes -> Key_dir.entry -> 'c -> 'c) -> 'c -> 'c
(* folds over all key-value pairs *)

val merge : string -> unit
val sync : handle -> unit
(* force write to disk *)

val close : handle -> unit
(* close db *)
