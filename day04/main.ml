open Stdlib.Printf

let fold_lines f lines acc =
  let process_line acc line =
    let line = match String.split_on_char ':' line with
      | _::line::[] -> line
      | _ -> raise (Invalid_argument "line")
    in
    let parse_part part =
      String.split_on_char ' ' part |>
      List.filter (fun x -> String.length x > 0) |>
      List.map int_of_string
    in
    let (left, right) = match
      String.split_on_char '|' line |>
      List.map String.trim |>
      List.map parse_part
    with
      | first::second::[] -> (first, second)
      | _ -> raise (Invalid_argument "line")
    in
    f acc left right
  in List.fold_left (process_line) acc lines
;;

let count_matching left right =
  let winning = List.fold_left
    (fun acc x -> Hashtbl.add acc x (); acc)
    (Hashtbl.create 10)
    left
  in let on_number acc n =
    match Hashtbl.find_opt winning n with
    | Some _ -> acc + 1
    | None -> acc
  in List.fold_left (on_number) 0 right
;;

let part1 lines =
  let on_line acc left right =
    acc + match count_matching left right with
    | n when n >= 1 -> 1 lsl (n - 1)
    | _ -> 0
  in fold_lines (on_line) lines 0
;;

let part2 lines =
  let repeats = List.length lines |> Hashtbl.create in
  let on_line (i, acc) left right =
    let copies = 1 + match Hashtbl.find_opt repeats i with
      | Some copies -> copies
      | None -> 0
    in
    let n = count_matching left right in
    for j = i + 1 to i + n do
      match Hashtbl.find_opt repeats j with
      | Some c -> Hashtbl.replace repeats j (c + copies)
      | None -> Hashtbl.add repeats j copies
    done;
    (i + 1, acc + copies)
  in let (_, total) = fold_lines (on_line) lines (0, 0) in total
;;

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
