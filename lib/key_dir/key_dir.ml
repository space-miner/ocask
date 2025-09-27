type entry = {
  file_id : int;
  value_size : int32;
  value_pos : int32;
  timestamp : int64;
}
[@@deriving show, eq]

type t = (bytes, entry) Hashtbl.t

let make_entry file_id value_size value_pos timestamp =
  { file_id; value_size; value_pos; timestamp }

let make () = Hashtbl.create 100
let clear t = Hashtbl.reset t
let list_keys t = Hashtbl.to_seq_keys t |> List.of_seq
let fold = Hashtbl.fold
