open Stdlib.Printf

let extract_code line =
  let symbols = (List.init (String.length line) (String.get line)) in

  let combine_digits digits =
    match digits with
      | Some (first, last) -> 10 * first + last
      | _ -> raise (Invalid_argument "digits")
  in

  let update_digits digits digit =
      Some (match digits with
      | None -> (digit, digit)
      | Some (first, _) -> (first, digit))
  in

  let rec extract_code' symbols digits =
    match symbols with
    | [] -> combine_digits digits
    | x :: xs -> extract_code' xs
        (try update_digits digits (int_of_string (String.make 1 x))
        with Failure _ -> digits)
  in extract_code' symbols None
;;

let find_calibration_value extractor lines =
  let rec f lines sum =
    match lines with
    | [] -> sum
    | x :: xs -> f xs sum + (extractor x)
  in f lines 0
;;

let part1 = find_calibration_value extract_code ;;
let part2 lines =
  let regexes = [
    (Str.regexp "one", "o1e");
    (Str.regexp "two", "t2o");
    (Str.regexp "three", "t3e");
    (Str.regexp "four", "f4r");
    (Str.regexp "five", "f5e");
    (Str.regexp "six", "s6x");
    (Str.regexp "seven", "s7n");
    (Str.regexp "eight", "e8t");
    (Str.regexp "nine", "n9e");
  ]
  in
  let fix_line line =
    let rec fix_line' line regexes =
      match regexes with
      | [] -> line
      | (r, t) :: xs -> fix_line' (Str.global_replace r t line) xs
    in fix_line' line regexes
  in
  find_calibration_value extract_code (List.map fix_line lines)
;;

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
