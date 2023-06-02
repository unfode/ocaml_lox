let read_file (filename: string) : (string, exn) result = (
  let channel = open_in filename in
  try
    let contents = really_input_string channel (in_channel_length channel) in
    close_in channel;
    Ok contents
  with e ->
    close_in channel;
    Error e
)

