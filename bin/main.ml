module DataFile = struct
  (* ignore crc for now *)
  type entry = {
    (* crc : string; *)
    timestamp : int32;
    key_size : int32;
    value_size : int32;
    key : bytes;
    value : bytes;
  }

  let make key value =
    {
      timestamp = Unix.time () |> Int32.of_float;
      key_size = Bytes.length key |> Int32.of_int;
      value_size = Bytes.length value |> Int32.of_int;
      key;
      value;
    }

  let bytes_of_i32_le i32 =
    let bytes = Bytes.create 4 in
    Bytes.set_int32_le bytes 0 i32;
    bytes

  let bytes_of_entry entry =
    Bytes.concat (Bytes.create 0)
      [
        bytes_of_i32_le entry.timestamp;
        bytes_of_i32_le entry.key_size;
        bytes_of_i32_le entry.value_size;
        entry.key;
        entry.value;
      ]

  let write_to_file file entry =
    let bytes = bytes_of_entry entry in
    let oc = Out_channel.open_bin file in
    Out_channel.output_bytes oc bytes;
    Out_channel.flush oc
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
  let entry1 = make key_bytes value_bytes in
  (* make entry2 *)
  let key_bytes = "horse" |> Bytes.of_string in
  let value_bytes = "shoecrab" |> Bytes.of_string in
  let entry2 = make key_bytes value_bytes in
  (* make entry3 *)
  let key_bytes = "catfish" |> Bytes.of_string in
  let value_bytes = "shoe" |> Bytes.of_string in
  let entry3 = make key_bytes value_bytes in
  (* make a file -- e.g. active.log *)
  (* write entries to the file in some order*)
  (* decode file and check if you get the correct entries back*)
  failwith "todo"
