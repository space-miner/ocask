module DB = struct
  (* lets start with this for now *)
  type handle = { keydir : Key_dir.t; dir_name : string }

  let make_datastore dir_name =
    (* if directory doesn't exist *)
    (* create directory and create active.log file *)
    (* otherwise reconstruct keydir by reading the *)
    (* datafiles in order and ending with active.lgo *)
    (* create and return handle *)
    failwith "todo"

  let get handle key =
    (* look up the key in keydir and *)
    (* use the value to figure out which datafile to read from *)
    (* get data from the datafile *)
    failwith "todo"

  let put handle key value =
    (* check size of active.log *)
    (* if there's room create a new datafile entry and append to active.log *)
    (* otherwise figure out the correct id to rename/copy active.log to *)
    failwith "todo"

  let delete handle key = failwith "todo"
  let list_keys handle = failwith "todo"
  let fold handle func acc = failwith "todo"
  let merge dir_name = failwith "todo"
  let sync handle = failwith "todo"
  let close handle = failwith "todo"
end

let () =
  let open Data_file in
  (* make entry1 *)
  let key_bytes = "hello" |> Bytes.of_string in
  let value_bytes = "world" |> Bytes.of_string in
  let entry1 () = make_entry key_bytes value_bytes in
  (* make entry2 *)
  let key_bytes = "horse" |> Bytes.of_string in
  let value_bytes = "shoecrab" |> Bytes.of_string in
  let entry2 () = make_entry key_bytes value_bytes in
  (* make entry3 *)
  let key_bytes = "catfish" |> Bytes.of_string in
  let value_bytes = "shoe" |> Bytes.of_string in
  let entry3 () = make_entry key_bytes value_bytes in
  (* make a file -- e.g. active.log *)
  let filename = "active.log" in
  (* write entries to the file in some order*)
  let entries_in = [ entry1 (); entry3 (); entry1 (); entry2 (); entry2 () ] in
  List.iter (fun entry -> write_entry filename entry) entries_in;
  (* decode file and check if you get the correct entries back*)
  let entries_out = entries_of_file filename |> List.rev in
  List.iter (fun entry -> Printf.printf "%s\n" (show_entry entry)) entries_out;
  assert (List.for_all2 (fun e1 e2 -> equal_entry e1 e2) entries_in entries_out);
  Sys.remove filename
