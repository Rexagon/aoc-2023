open Stdlib.Printf

type direction = Up | Right | Down | Left
type pipe = V | H | UR | UL | DL | DR
type tile = Start | Ground | Pipe of pipe
type flow_tile = Empty | Flow of { prev: direction; next: direction }
type 'a field = { matrix: 'a array array; rows: int; cols: int }

let parse_field lines =
  let parse_tile c =
    match c with
    | '|' -> Pipe V
    | '-' -> Pipe H
    | 'L' -> Pipe UR
    | 'J' -> Pipe UL
    | '7' -> Pipe DL
    | 'F' -> Pipe DR
    | '.' -> Ground
    | 'S' -> Start
    | _ -> raise (Invalid_argument "tile")
  in
  let parse_line line =
    Array.init
      (String.length line)
      (fun i -> String.get line i |> parse_tile)
  in
  let matrix = List.map (parse_line) lines |> Array.of_list in
  let rows = Array.length matrix in
  let cols = Array.get matrix 0 |> Array.length in
  { matrix; rows; cols }

let field_get field (row, col) =
  if row >= field.rows || col >= field.cols then None
  else Some field.matrix.(row).(col)

let field_set field (row, col) value = field.matrix.(row).(col) <- value

let find_start field =
  let rec find_start' row col =
    if row >= field.rows then raise (Invalid_argument "start not found")
    else if col >= field.cols then find_start' (row + 1) 0
    else
      match field_get field (row, col) with
      | Some Start -> (row, col)
      | _ -> find_start' row (col + 1)
  in find_start' 0 0

let change_pos field (row, col) dir =
  match dir with
  | Up when row > 0 -> Some (row - 1, col)
  | Right when col + 1 < field.cols -> Some (row, col + 1)
  | Down when row + 1 < field.rows -> Some (row + 1, col)
  | Left when col > 0 -> Some (row, col - 1)
  | _ -> None

let follow_pipe field pos from =
  Option.bind (field_get field pos) (fun tile ->
    match tile with
    | Pipe V -> (match from with Up -> Some Up | Down -> Some Down | _ -> None)
    | Pipe H -> (match from with Left -> Some Left | Right -> Some Right | _ -> None)
    | Pipe UR -> (match from with Down -> Some Right | Left -> Some Up | _ -> None)
    | Pipe UL -> (match from with Down -> Some Left | Right -> Some Up | _ -> None)
    | Pipe DL -> (match from with Up -> Some Left | Right -> Some Down | _ -> None)
    | Pipe DR -> (match from with Up -> Some Right | Left -> Some Down | _ -> None)
    | _ -> None)

let find_adjucent field pos =
  [Up; Right; Down; Left] |>
  List.filter_map (fun dir -> change_pos field pos dir |> Option.map (fun pos -> (pos, dir))) |>
  List.filter_map (fun (pos, from) -> follow_pipe field pos from |> Option.map (fun _ -> (pos, from)))

let part1 lines =
  let field = parse_field lines in
  let start = find_start field in
  let adjucent = find_adjucent field start in
  let (start, (finish_pos, _)) = match adjucent with
    | first::second::[] -> (first, second)
    | _ -> raise (Invalid_argument "invalid start")
  in
  let rec compute_loop_len (pos, from) len =
    if pos = finish_pos then len + 1
    else
      let from = follow_pipe field pos from |> Option.get in
      let next_pos = change_pos field pos from |> Option.get in
      compute_loop_len (next_pos, from) (len + 1)
  in
  let loop_len = compute_loop_len start 1 in
  loop_len / 2

let part2 lines =
  let field = parse_field lines in
  let field_flow = {
    matrix = Array.make_matrix field.rows field.cols Empty;
    rows = field.rows;
    cols = field.cols;
  } in

  (* find loop start and adjucent pipes *)
  let start = find_start field in
  let adjucent = find_adjucent field start in
  let ((next_pos, next_dir), (finish_pos, finish_dir)) = match adjucent with
    | first::second::[] -> (first, second)
    | _ -> raise (Invalid_argument "invalid start")
  in

  (* inverse adjucent finish direction *)
  let finish_dir = match finish_dir with
    | Up -> Down
    | Right -> Left
    | Down -> Up
    | Left -> Right
  in

  (* update start tile in field flow *)
  field_set field_flow start (Flow { next = next_dir; prev = finish_dir });

  (* update other tiles in field flow *)
  let rec fill_field_flow prev_dir pos =
    if pos = finish_pos then
      field_set field_flow pos (Flow { next = finish_dir; prev = prev_dir })
    else
      let next_dir = follow_pipe field pos prev_dir |> Option.get in
      field_set field_flow pos (Flow { next = next_dir; prev = prev_dir });
      change_pos field pos next_dir |> Option.get |> fill_field_flow next_dir
  in fill_field_flow next_dir next_pos;

  (* compute area whithin the loop *)
  let compute_line_area line =
    let (area, _) =
      Array.fold_left (fun (area, n) tile ->
        match tile with
        | Empty when n <> 0 -> (area + 1, n)
        | Flow { prev = Up; _ } -> (area, n - 1)
        | Flow { next = Down; _ } -> (area, n + 1)
        | _ -> (area, n)
      ) (0, 0) line
    in area
  in
  Array.to_seq field_flow.matrix |>
  Seq.map (compute_line_area) |>
  Seq.fold_left (+) 0

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines

let () = solve "input.txt"
