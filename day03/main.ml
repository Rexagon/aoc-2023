open Stdlib.Printf

type symbol = Grear of int | Other ;;

let transform_lines lines =
  let cols = List.hd lines |> String.length in
  let empty_line = List.init cols (fun _ -> '.') in
  let lines = List.map (fun line -> List.init (String.length line) (String.get line)) lines in
  (cols, empty_line :: lines @ [empty_line])
;;

let lists_are_empty a b = match a, b with ([], []) -> true | _ -> false ;;

let fold_numbers (cols, lines) f a =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let char_of_digit c = Char.code c - Char.code '0' in
  let on_char cur f a n symbols new_symbols =
    if is_digit cur then
      let cur = char_of_digit cur in
      let n = match n with
        | Some n -> Some (n * 10 + cur)
        | None -> Some cur
      in (a, n, symbols @ new_symbols)
    else
      match n with
      | Some n when not (lists_are_empty symbols new_symbols) ->
        (f a n (symbols @ new_symbols), None, new_symbols)
      | _ -> (a, None, new_symbols)
  in
  let rec fold_line' i (top, cur, btm) f a n symbols =
    match (top, cur, btm) with
    | ([], [], []) -> let (a, _, _) = on_char '.' f a n symbols [] in a
    | (top::top_xs, cur::cur_xs, btm::btm_xs) -> (
      let parse_symbol (c, i) =
        match c with
        | '*' -> Some (Grear i)
        | _ when c != '.' && not(is_digit c) -> Some Other
        | _ -> None
      in
      let new_symbols = List.filter_map (parse_symbol) [(top, i - cols); (cur, i); (btm, i + cols)] in
      let (a, n, symbols) = on_char cur f a n symbols new_symbols in
      fold_line' (i + 1) (top_xs, cur_xs, btm_xs) f a n symbols
    )
    | _ -> raise (Invalid_argument "line")
  in
  let rec fold_lines' index_offset lines f a =
    match lines with
    | [] -> a
    | top::xs -> match xs with
      | cur::btm::_ ->
        fold_line' index_offset (top, cur, btm) f a None [] |>
        fold_lines' (cols + index_offset) xs f
      | _ -> a
  in fold_lines' 0 lines f a
;;

let part1 lines =
  let on_number acc number _ = acc + number in
  fold_numbers (transform_lines lines) (on_number) 0
;;

let part2 lines =
  let gears = Hashtbl.create 100 in
  let on_symbol number s =
    match s with
    | Grear i -> (
      match Hashtbl.find_opt gears i with
      | Some list -> Hashtbl.add gears i (number :: list)
      | None -> Hashtbl.add gears i [number]
    )
    | Other -> ()
  in
  let on_number _ number = List.iter (on_symbol number) in
  fold_numbers (transform_lines lines) (on_number) ();
  let on_attached_numbers _ items acc =
    match items with
    | a::b::[] -> acc + a * b
    | _ -> acc
  in
  Hashtbl.fold (on_attached_numbers) gears 0
;;

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
