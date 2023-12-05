open Stdlib.Printf

let parse_lines lines =
  let group_lines lines =
    let rec group_lines' lines current =
      match lines with
      | [] -> []
      | ""::xs -> current :: group_lines' xs []
      | line::xs -> group_lines' xs (line::current)
    in group_lines' lines [] |> List.map List.rev
  in
  let parse_seeds line =
    let line = match List.hd line |> String.split_on_char ':' with
      | _::line::[] -> line
      | _ -> raise (Invalid_argument "seeds line")
    in
    String.split_on_char ' ' line |>
    List.filter (fun x -> String.length x > 0) |>
    List.map int_of_string
  in
  let parse_ranges line =
    match String.split_on_char ' ' line |> List.map (int_of_string) with
      | dst_start::src_start::n::[] -> (dst_start, src_start, n)
      | _ -> raise (Invalid_argument "ranges line")
  in
  let parse_group lines =
    match lines with
    | _title::lines -> List.map (parse_ranges) lines
    | _ -> raise (Invalid_argument "groups")
  in
  match group_lines (lines @ [""]) with
  | first::rest -> parse_seeds first, List.map (parse_group) rest
  | _ -> raise (Invalid_argument "lines")
;;

let part1 lines =
  let (seeds, groups) = parse_lines lines in
  let resolve_location seed =
    let resolve_in_group src group =
      let map_in_range (dst_start, src_start, n) =
        if src >= src_start && src < src_start + n
          then Some (dst_start + (src - src_start))
          else None
      in
      match List.find_map (map_in_range) group with
      | Some matched -> matched
      | None -> src
    in List.fold_left (resolve_in_group) seed groups
  in
  let locations = List.map (resolve_location) seeds in
  List.fold_left (min) (List.hd locations) (List.tl locations)
;;

let part2 lines =
  let (seeds, groups) = parse_lines lines in
  let rec fold_seeds seeds =
    match seeds with
    | [] -> []
    | start::n::rest -> (start, n) :: (fold_seeds rest)
    | _ -> raise (Invalid_argument "seeds list")
  in
  let seed_ranges = fold_seeds seeds in
  let resolve_locations seed_range =
    let map_split_range (in_start, in_n) (dst_start, src_start, n) =
      let in_end = in_start + in_n in
      let src_end = src_start + n in
      if src_end < in_start || src_start > in_end then
        ([(in_start, in_n)], [])
      else if (src_start <= in_start && src_end >= in_end) then
        ([], [(dst_start + (in_start - src_start), in_n)])
      else if (src_start <= in_start && src_end < in_end) then
        let left_n = src_end - in_start in
        ([(in_start + left_n, in_n - left_n)], [(dst_start + (in_start - src_start), left_n)])
      else if (src_start > in_start && src_end >= in_end) then
        let right_n = in_end - src_start in
        ([(in_start, in_n - right_n)], [(dst_start + (in_start - src_start + in_n - right_n), right_n)])
      else
        let left_n = src_start - in_start in
        let right_n = in_end - src_end in
        ([(in_start, left_n); (in_start + in_n - right_n, right_n)], [(dst_start + left_n, in_n - left_n - right_n)])
    in
    let rec resolve_group_range group_ranges to_resolve =
      match group_ranges with
      | [] -> [to_resolve]
      | group_range::rest_group_ranges -> (
        match map_split_range to_resolve group_range with
        | [], resolved -> resolved
        | [unresolved], resolved -> resolved @ resolve_group_range rest_group_ranges unresolved
        | [left; right], resolved ->
            resolved
            @ resolve_group_range group_ranges left
            @ resolve_group_range group_ranges right
        | _ -> raise (Invalid_argument "range")
      )
    in List.fold_left (fun ranges group ->
      List.map (resolve_group_range group) ranges |>
      List.fold_left (@) []
      ) [seed_range] groups
  in
  let locations = List.fold_left
    (fun locations seed_range -> locations @ resolve_locations seed_range)
    [] seed_ranges
  in
  let (first_location_start, _) = List.hd locations in
  List.fold_left
    (fun min_location (range_start, _) -> min min_location range_start)
    first_location_start (List.tl locations)
;;

let solve filename =
  let ic = open_in filename in
  let content = In_channel.input_all ic in
  let lines = String.split_on_char '\n' content in
  printf "%d\n" @@ part1 lines;
  printf "%d\n" @@ part2 lines
;;

let () = solve "input.txt"
