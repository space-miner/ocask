(* ignore crc for now *)
type entry = {
  timestamp : int64;
  key_size : int32;
  value_size : int32;
  key : bytes;
  value : bytes;
}
[@@deriving show, eq, fields]

let make_entry key value timestamp =
  {
    timestamp;
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

(* write may not write all bytes, so loop till the whole buffer is written *)
let write_all fd buf =
  let len = Bytes.length buf in
  let rec aux off =
    if off < len then (
      let written = Unix.single_write fd buf off (len - off) in
      if written = 0 then failwith "write returned 0 bytes";
      aux (off + written))
  in
  aux 0

(* same as write, read may not read everything. loop till everything is read *)
(* and return number of bytes read *)
let read_all fd buf =
  let len = Bytes.length buf in
  let rec aux off =
    if off >= len then off
    else
      let read = Unix.read fd buf off (len - off) in
      if read = 0 then off else aux (off + read)
  in
  aux 0

let write_entry filename entry =
  let fd = Unix.openfile filename [ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
  let bytes = bytes_of_entry entry in
  write_all fd bytes;
  Unix.fsync fd;
  Unix.close fd

let file_as_bytes filename =
  let file_size = (Unix.stat filename).st_size in
  let fd = Unix.openfile filename [ O_RDONLY ] 0o644 in
  let bytes = Bytes.create file_size in
  let read = read_all fd bytes in
  Unix.close fd;
  Bytes.sub bytes 0 read

let offset_entries_of_bytes bytes =
  let rec aux bytes ix acc =
    if ix + 16 > Bytes.length bytes then acc
    else
      let timestamp = Bytes.get_int64_le bytes ix in
      let key_size = Bytes.get_int32_le bytes (ix + 8) in
      let value_size = Bytes.get_int32_le bytes (ix + 12) in
      let entry_len = 16 + Int32.to_int key_size + Int32.to_int value_size in
      if ix + entry_len > Bytes.length bytes then acc
      else
        let key = Bytes.sub bytes (ix + 16) (Int32.to_int key_size) in
        let value =
          Bytes.sub bytes
            (ix + 16 + Int32.to_int key_size)
            (Int32.to_int value_size)
        in
        aux bytes (ix + entry_len) ((ix, make_entry key value timestamp) :: acc)
  in
  aux bytes 0 [] |> List.rev

let entries_of_bytes bytes = bytes |> offset_entries_of_bytes |> List.map snd
let entries_of_file filename = file_as_bytes filename |> entries_of_bytes

let%test "serialization roundtrip preserves data" =
  let timestamp = C_utils.clock_gettime_ns () in
  let key = Bytes.of_string "hello" in
  let value = Bytes.of_string "world" in
  let entry = make_entry key value timestamp in
  let serialized = bytes_of_entry entry in
  let deserialized = List.hd (entries_of_bytes serialized) in
  equal_entry entry deserialized
