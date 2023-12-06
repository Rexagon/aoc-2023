open Stdlib.Printf

let parse_races lines f =
  let parse_numbers line =
    match String.split_on_char ':' line with
    | _::numbers::[] -> String.split_on_char ' ' numbers |> f
    | _ -> raise (Invalid_argument "line")
  in match lines |> List.map (parse_numbers) with
    | times::distances::[] -> List.combine times distances
    | _ -> raise (Invalid_argument "lines")

let count_timings (max_time, record) =
  let (a, b, c) = (-1.0, float_of_int max_time, float_of_int (-record)) in
  let d_sqrt = sqrt (b *. b -. 4.0 *. a *. c) in
  let x1 = (b -. d_sqrt) /. 2.0 in
  let x2 = (b +. d_sqrt) /. 2.0 in
  (Float.trunc x2 |> int_of_float) - (Float.ceil x1 |> int_of_float) + 1

let process_timings parser lines =
  parse_races lines (parser) |>
  List.map count_timings |>
  List.fold_left ( * ) 1

let part1 = process_timings (fun line ->
    List.filter (fun x -> String.length x > 0) line |>
    List.map int_of_string)

let part2 = process_timings (fun line -> [String.concat "" line |> int_of_string])

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
