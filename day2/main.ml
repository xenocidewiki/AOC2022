open! Core

module Shape = struct
  type choice = Rock | Paper | Scissors

  let match_character inpt = 
    match inpt with
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z"-> Scissors
    | _ -> raise (Invalid_argument "Expected ABC; Got something else")
  ;;

  let convert_to_type battle = 
    match battle with
    | [a; b] -> (match_character a, match_character b)
    | _ -> raise (Invalid_argument "Battle list had more than two elements.")
  ;;

  let value choice =
    match choice with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3
  ;;

end

module Outcome = struct
  type rps_result = Win | Draw | Loss

  let get_battle_result battle =
    match battle with
    | (Shape.Rock, Shape.Paper) | (Shape.Paper, Shape.Scissors) | (Shape.Scissors, Shape.Rock) -> Win
    | (Shape.Rock, Shape.Rock) | (Shape.Paper, Shape.Paper) | (Shape.Scissors, Shape.Scissors) -> Draw
    | (Shape.Rock, Shape.Scissors) | (Shape.Paper, Shape.Rock) | (Shape.Scissors, Shape.Paper) -> Loss
  ;;

  let value result =
    match result with
    | Win -> 6
    | Draw -> 3
    | Loss -> 0
  ;;

  let get_battle_score battle =
    let battle_result = get_battle_result battle in
    match battle with
    | (_, b) -> value battle_result + Shape.value b
  ;;

  (** Part 2 *)
  let get_intended_outcome battle =
    match battle with
    | (_, Shape.Rock) -> Loss
    | (_, Shape.Paper) -> Draw
    | (_, Shape.Scissors) -> Win
  ;;

  let get_expected_choice outcome battle =
    match outcome, battle with
    | Win, (Shape.Scissors, _) | Draw, (Shape.Rock, _) | Loss, (Shape.Paper, _) -> Shape.Rock
    | Win, (Shape.Rock, _) | Draw, (Shape.Paper, _) | Loss, (Shape.Scissors, _) -> Shape.Paper
    | Win, (Shape.Paper, _) | Draw, (Shape.Scissors, _) | Loss, (Shape.Rock, _) -> Shape.Scissors
  ;;

  let calculate_part2_score battle =
    let outcome = get_intended_outcome battle in
    let choice = get_expected_choice outcome battle in
    value outcome + Shape.value choice
  ;;
end

let read_input_from_stdin () =
  let rec helper acc =
    try
      let line = Caml.read_line () in
      helper (line :: acc)
    with End_of_file -> acc
  in 
  helper []

let calculate_score lines calc =
  let rec helper sum next =
    match next with
    | [] -> sum
    | x :: xs -> let battle = String.split ~on:' ' x in
      let numeric_outcome = calc (Shape.convert_to_type battle)
      in helper (sum + numeric_outcome) xs 
  in
  helper 0 lines

let () =
  let lines = read_input_from_stdin () in
  let score = calculate_score lines Outcome.get_battle_score in
  printf "%d" score;
  let part2_score = calculate_score lines Outcome.calculate_part2_score in
  printf "%d" part2_score;