open! Core

let read_all_lines () =
  let rec helper acc =
    try
      let line = Caml.read_line () in
      helper (line :: acc)
    with End_of_file -> acc
  in 
  helper []
  

let construct_list_of_sums input =
  let rec helper acc current_sum next =
    match next with
    | [] -> current_sum :: acc  
    | x :: xs -> match x with
      | "" -> helper (current_sum :: acc) 0 xs
      | _  -> helper acc (current_sum + int_of_string x) xs
  in
  helper [] 0 input

let sort_list input = List.sort input ~compare:Int.compare |> List.rev

let sum_top_3_calories input = 
  let rec helper sum next ctr =
    match ctr with
    | 3 -> sum
    | _ -> match next with
      | [] -> sum
      | x :: xs -> helper (sum + x) xs (ctr + 1)
  in 
  helper 0 input 0

(** Used for part1 of day1*)
let get_max_value input = List.fold input ~init:0 ~f:Int.max

let () = 
  let a = read_all_lines () in  
  let b = construct_list_of_sums a in 
  let sorted_list = sort_list b in
  let final_sum = sum_top_3_calories sorted_list in
  Printf.printf "%d" final_sum