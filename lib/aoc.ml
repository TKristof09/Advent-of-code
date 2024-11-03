open Core

let read_to_list filename = In_channel.read_lines ("inputs/" ^ filename)

let read_to_list_filtered filename =
    In_channel.read_lines ("inputs/" ^ filename)
    |> List.filter ~f:(fun s -> not (String.is_empty s))

let read_to_array filename = Array.of_list @@ read_to_list filename

let read_to_array_filtered filename =
    Array.of_list @@ read_to_list_filtered filename
