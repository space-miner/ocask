external clock_gettime_ns : unit -> int64 = "clock_gettime_ocaml"

(* ignore crc for now *)
type entry = {
  timestamp : int64;
  key_size : int32;
  value_size : int32;
  key : bytes;
  value : bytes;
}
[@@deriving show, eq]

let make_entry key value =
  {
    timestamp = clock_gettime_ns ();
    key_size = Bytes.length key |> Int32.of_int;
    value_size = Bytes.length value |> Int32.of_int;
    key;
    value;
  }

let int32_to_bytes_le i32 =
  let bytes = Bytes.create 4 in
  Bytes.set_int32_le bytes 0 i32;
  bytes

let int64_to_bytes_le i64 =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 i64;
  bytes

let bytes_of_entry entry =
  Bytes.concat Bytes.empty
    [
      int64_to_bytes_le entry.timestamp;
      int32_to_bytes_le entry.key_size;
      int32_to_bytes_le entry.value_size;
      entry.key;
      entry.value;
    ]

let write_entry filename entry =
  let fd = Unix.openfile filename [ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
  let bytes = bytes_of_entry entry in
  ignore (Unix.single_write fd bytes 0 (Bytes.length bytes));
  Unix.fsync fd;
  Unix.close fd

let to_bytes filename =
  let file_size = (Unix.stat filename).st_size in
  let fd = Unix.openfile filename [ O_RDONLY ] 0o644 in
  let bytes = Bytes.create file_size in
  ignore (Unix.read fd bytes 0 file_size);
  Unix.close fd;
  bytes

let bytes_of_file filename =
  let file_size = (Unix.stat filename).st_size in
  let fd = Unix.openfile filename [ O_RDONLY ] 0o644 in
  let bytes = Bytes.create file_size in
  ignore (Unix.read fd bytes 0 file_size);
  Unix.close fd;
  bytes

let entries_of_bytes bytes =
  let rec aux bytes ix acc =
    try
      let timestamp_offset = 0 in
      let key_size_offset = 8 in
      let value_size_offset = 12 in
      let key_offset = 16 in
      let timestamp = Bytes.get_int64_le bytes (ix + timestamp_offset) in
      let key_size = Bytes.get_int32_le bytes (ix + key_size_offset) in
      let value_size = Bytes.get_int32_le bytes (ix + value_size_offset) in
      let key = Bytes.sub bytes (ix + key_offset) (Int32.to_int key_size) in
      let value_offset = key_offset + Int32.to_int key_size in
      let value =
        Bytes.sub bytes (ix + value_offset) (Int32.to_int value_size)
      in
      let next_ix = ix + value_offset + Int32.to_int value_size in
      aux bytes next_ix ({ timestamp; key_size; value_size; key; value } :: acc)
    with _ -> acc
  in
  aux bytes 0 []

let entries_of_file filename = bytes_of_file filename |> entries_of_bytes

let print_entry entry =
  Printf.printf
    "{\n\
     \ttimestamp: %Ld,\n\
     \tkey_size: %ld,\n\
     \tvalue_size: %ld,\n\
     \tkey: %s,\n\
     \tvalue: %s,\n\
     },\n"
    entry.timestamp entry.key_size entry.value_size
    (String.of_bytes entry.key)
    (String.of_bytes entry.value)

let equal e1 e2 =
  Int64.equal e1.timestamp e2.timestamp
  && Int32.equal e1.key_size e2.key_size
  && Int32.equal e1.value_size e2.value_size
  && Bytes.equal e1.key e2.key
  && Bytes.equal e1.value e2.value
