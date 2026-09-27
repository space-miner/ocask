type entry
type t

val make : unit -> t
val make_entry : int -> int32 -> int32 -> int64 -> entry
val clear : t -> unit
val add : t -> bytes -> entry -> unit
val find : t -> bytes -> entry
val fold : (bytes -> entry -> 'c -> 'c) -> t -> 'c -> 'c
val is_tombstone : entry -> bool
val file_id : entry -> int
val value_pos : entry -> int32
val value_size : entry -> int32
val timestamp : entry -> int64
