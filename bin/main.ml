module DB = struct
  type handle = {
    dir_name : string;
    keydir : Key_dir.t;
    mutable active_file_id : int;
    mutable current_pos : int32;
    mutable active_fd : Unix.file_descr option;
  }

  let log_filename id = Printf.sprintf "%04d.log" id
  let max_file_size = 10_000_000

  let init_datastore dir_name =
    if not (Sys.file_exists dir_name) then (
      (* need execute permissions otherwise it can't create active.log for some reason ?? *)
      Unix.mkdir dir_name 0o755;
      let active_log = Filename.concat dir_name "active.log" in
      let fd = Unix.openfile active_log [ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
      Unix.close fd)
    else Unix.chmod dir_name 0o755

  let rotate_active_log handle =
    (* rename active.log to the appropriate archive name *)
    let archive_active_log handle =
      let active_log = Filename.concat handle.dir_name "active.log" in
      let archive_log =
        Filename.concat handle.dir_name (log_filename handle.active_file_id)
      in
      if Sys.file_exists active_log then Sys.rename active_log archive_log;
      (* close current fd *)
      match handle.active_fd with
      | Some fd ->
          Unix.close fd;
          handle.active_fd <- None
      | None -> ()
    in
    (* create new active.log file *)
    let create_active_log handle =
      let active_log = Filename.concat handle.dir_name "active.log" in
      let fd = Unix.openfile active_log [ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
      handle.active_fd <- Some fd;
      handle.active_file_id <- handle.active_file_id + 1;
      handle.current_pos <- 0l
    in
    archive_active_log handle;
    create_active_log handle

  (* create handle from dir_name *)
  let get_handle dir_name =
    let log_files =
      if Sys.file_exists dir_name then
        Sys.readdir dir_name |> Array.to_list
        |> List.filter (fun file -> Filename.extension file = ".log")
      else []
    in
    let active_file_id = List.length log_files in
    let keydir = Key_dir.make () in
    (* TODO: rebuild from hints *)
    let active_log = Filename.concat dir_name "active.log" in
    try
      let current_pos = (Unix.stat active_log).st_size |> Int32.of_int in
      let active_fd =
        Some (Unix.openfile active_log [ O_WRONLY; O_APPEND ] 0o644)
      in
      { keydir; dir_name; active_file_id; current_pos; active_fd }
    with _ -> failwith "failed to open active.log"

  let get handle key =
    try
      let keydir_entry = Hashtbl.find handle.keydir key in
      let datafile =
        if keydir_entry.file_id = handle.active_file_id then
          Filename.concat handle.dir_name "active.log"
        else Filename.concat handle.dir_name (log_filename keydir_entry.file_id)
      in
      (* read the value from the data file at the specified position *)
      let fd = Unix.openfile datafile [ O_RDONLY ] 0o644 in
      let _ = Unix.lseek fd (Int32.to_int keydir_entry.value_pos) SEEK_SET in
      let value = Bytes.create (Int32.to_int keydir_entry.value_size) in
      let _ = Unix.read fd value 0 (Int32.to_int keydir_entry.value_size) in
      Unix.close fd;
      Some value
    with Not_found -> None

  let append_entry_to_log handle (entry : Data_file.entry) =
    let entry_size =
      8 + 4 + 4 + Bytes.length entry.key + Bytes.length entry.value
    in
    (* check if we need to rotate the active file *)
    let active_log = Filename.concat handle.dir_name "active.log" in
    let current_file_size =
      if Sys.file_exists active_log then (Unix.stat active_log).st_size else 0
    in
    (* archive active log by renaming the current active log and creating a new one *)
    if current_file_size + entry_size > max_file_size then
      rotate_active_log handle;
    (* write to active log *)
    Data_file.write_entry active_log entry;
    (* update keydir *)
    let value_pos =
      8 + 4 + 4 + Bytes.length entry.key + Int32.to_int handle.current_pos
      |> Int32.of_int
    in
    let keydir_entry =
      Key_dir.make_entry handle.active_file_id entry.value_size value_pos
        entry.timestamp
    in
    Hashtbl.replace handle.keydir entry.key keydir_entry;
    (* update position in file *)
    handle.current_pos <- Int32.add handle.current_pos (Int32.of_int entry_size)

  let put handle key value =
    let entry = Data_file.make_entry key value in
    append_entry_to_log handle entry

  let delete handle key =
    let tombstone_entry = Data_file.make_tombstone_entry key in
    append_entry_to_log handle tombstone_entry

  let list_keys handle = Key_dir.list_keys handle.keydir
  let fold handle func acc = Key_dir.fold func handle.keydir acc
  let merge dir_name = failwith "todo"

  let sync handle =
    match handle.active_fd with Some fd -> Unix.fsync fd | None -> ()

  let close handle =
    let _ =
      match handle.active_fd with
      | Some fd ->
          sync handle;
          Unix.close fd
      | None -> ()
    in
    Key_dir.clear handle.keydir
end

let () =
  let test_dir = "test_db" in
  DB.init_datastore test_dir;
  let handle = DB.get_handle test_dir in

  let key1 = Bytes.of_string "hello" in
  let value1 = Bytes.of_string "world" in
  DB.put handle key1 value1;

  let key2 = Bytes.of_string "foo" in
  let value2 = Bytes.of_string "bar" in
  DB.put handle key2 value2;

  (* create large value that won't fit in current active.log *)
  (* should rotate files -- archive to 0001.log and create new active.log *)
  let key3 = Bytes.of_string "garbage" in
  let value3 = Bytes.create 10_000_000 in
  Bytes.fill value3 0 (Bytes.length value3) '@';
  DB.put handle key3 value3;

  let key4 = Bytes.of_string "tom" in
  let value4 = Bytes.of_string "atos" in
  DB.put handle key4 value4
