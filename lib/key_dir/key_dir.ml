type entry = {
  file_id : int;
  value_size : int32;
  value_pos : int32;
  timestamp : int64;
}
[@@deriving show, eq, fields]

type t = (bytes, entry) Hashtbl.t

let make () = Hashtbl.create 100

let make_entry file_id value_size value_pos timestamp =
  { file_id; value_size; value_pos; timestamp }

let clear t = Hashtbl.reset t
let add = Hashtbl.replace
let find = Hashtbl.find
let fold = Hashtbl.fold
let is_tombstone entry = entry.timestamp = 0L

(* no longer used *)
(* let list_keys t = t |> Hashtbl.to_seq_keys |> List.of_seq *)
