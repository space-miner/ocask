type handle = {
  dir_name : string;
  keydir : Key_dir.t;
  mutable active_file_id : int;
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
    handle.active_file_id <- handle.active_file_id + 1
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
  let active_fd =
    if Sys.file_exists active_log then
      Some (Unix.openfile active_log [ O_WRONLY; O_APPEND ] 0o644)
    else None
  in
  { keydir; dir_name; active_file_id; active_fd }

let get handle key =
  try
    let keydir_entry = Key_dir.find handle.keydir key in
    let datafile =
      if keydir_entry.file_id = handle.active_file_id then
        Filename.concat handle.dir_name "active.log"
      else Filename.concat handle.dir_name (log_filename keydir_entry.file_id)
    in
    (* read the value from the data file at the specified position *)
    let fd = Unix.openfile datafile [ O_RDONLY ] 0o644 in
    ignore (Unix.lseek fd (Int32.to_int keydir_entry.value_pos) SEEK_SET);
    let value = Bytes.create (Int32.to_int keydir_entry.value_size) in
    ignore (Unix.read fd value 0 (Int32.to_int keydir_entry.value_size));
    Unix.close fd;
    Some value
  with Not_found -> None

let put_aux handle key value timestamp =
  let entry = Data_file.make_entry key value timestamp in
  (* check if we need to rotate the active file *)
  let entry_size = 8 + 4 + 4 + Bytes.length key + Bytes.length value in
  let current_pos =
    match handle.active_fd with
    | Some fd -> Unix.lseek fd 0 Unix.SEEK_END
    | None -> 0
  in
  (* archive active log by renaming the current active log and creating a new one *)
  if current_pos + entry_size > max_file_size then rotate_active_log handle;
  (* write to active log *)
  let active_log = Filename.concat handle.dir_name "active.log" in
  Data_file.write_entry active_log entry;
  (* update keydir *)
  let value_pos = 8 + 4 + 4 + Bytes.length key + current_pos |> Int32.of_int in
  let keydir_entry =
    Key_dir.make_entry handle.active_file_id entry.value_size value_pos
      entry.timestamp
  in
  Key_dir.add handle.keydir entry.key keydir_entry

let put handle key value =
  let timestamp = C_utils.clock_gettime_ns () in
  put_aux handle key value timestamp

let delete handle key =
  (* tombstone by set timestamp to 0 and value to be empty *)
  put_aux handle key Bytes.empty 0L

(* this gives all keys i never filtered out tombstones *)
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
