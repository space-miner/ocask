module DataFile = struct
  external clock_gettime_ns : unit -> int64 = "clock_gettime_ocaml"

  (* ignore crc for now *)
  type entry = {
    timestamp : int64;
    key_size : int32;
    value_size : int32;
    key : bytes;
    value : bytes;
  }

  let make key value =
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
    let rec aux bytes ix =
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
        let next_ix = value_offset + Int32.to_int value_size in
        { timestamp; key_size; value_size; key; value } :: aux bytes next_ix
      with _ -> []
    in
    aux bytes 0

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
end

module DB = struct
  let open_datastore dir_name = failwith "todo"
  let get handle key = failwith "todo"
  let put handle key value = failwith "todo"
  let delete handle key = failwith "todo"
  let list_keys handle = failwith "todo"
  let fold handle func acc = failwith "todo"
  let merge dir_name = failwith "todo"
  let sync handle = failwith "todo"
  let close handle = failwith "todo"
end

let () =
  let open DataFile in
  (* make entry1 *)
  let key_bytes = "hello" |> Bytes.of_string in
  let value_bytes = "world" |> Bytes.of_string in
  let entry1 () = make key_bytes value_bytes in
  (* make entry2 *)
  let key_bytes = "horse" |> Bytes.of_string in
  let value_bytes = "shoecrab" |> Bytes.of_string in
  let entry2 () = make key_bytes value_bytes in
  (* make entry3 *)
  let key_bytes = "catfish" |> Bytes.of_string in
  let value_bytes = "shoe" |> Bytes.of_string in
  let entry3 () = make key_bytes value_bytes in
  (* make a file -- e.g. active.log *)
  let filename = "active.log" in
  (* write entries to the file in some order*)
  let entries = [ entry1 (); entry3 (); entry1 (); entry2 (); entry2 () ] in
  List.iter (fun entry -> write_entry filename entry) entries;
  (* decode file and check if you get the correct entries back*)
  let entries = entries_of_file filename in
  List.iter (fun entry -> print_entry entry) entries
