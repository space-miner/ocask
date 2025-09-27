let () =
  let test_dir = "test_db" in
  Db.init_datastore test_dir;
  let handle = Db.get_handle test_dir in

  let key1 = Bytes.of_string "hello" in
  let value1 = Bytes.of_string "world" in
  Db.put handle key1 value1;

  let key2 = Bytes.of_string "foo" in
  let value2 = Bytes.of_string "bar" in
  Db.put handle key2 value2;

  (* create large value that won't fit in current active.log *)
  (* should rotate files -- archive to 0001.log and create new active.log *)
  let key3 = Bytes.of_string "garbage" in
  let value3 = Bytes.create 10_000_000 in
  Bytes.fill value3 0 (Bytes.length value3) '@';
  Db.put handle key3 value3;

  let key4 = Bytes.of_string "tom" in
  let value4 = Bytes.of_string "atos" in
  Db.put handle key4 value4;
