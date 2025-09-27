type entry = {
  timestamp : int64;
  key_size : int32;
  value_size : int32;
  value_pos : int32;
  key : bytes;
}
[@@deriving show, eq]

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
      int32_to_bytes_le entry.value_pos;
      entry.key;
    ]
