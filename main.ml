open Bot
open State
open Poker
open List
open Command

exception End of State.t

(* Initializing the module for bot players *)
module TestBotInfo = struct
  let diff = Test
  let seed = 0
end
module MyTestBot = MyBot.Make(TestBotInfo)

(* This is a list of predetermined names for input into build_table *)
let table_names = 
  ["Cesar"; 
   "Dean"; 
   "Parker"; 
   "Clarkson";
   "Gries";
   "White";
   "Fan";
   "Foster";
   "Hopcroft"
  ]

(* [line_div n] is a function for creating a dividing line that spans a given 
   number of spaces *)
let line_div (length : int) : unit =
  for i = 0 to length - 1 do
    print_string "—"
  done;
  print_string "\n"

(* [print_ansi s color] prints a string using ANSITerminal *)
let print_ansi s color : unit =
  match color with
  | "red" -> ANSITerminal.(print_string [red] s)
  | "yellow" -> ANSITerminal.(print_string [yellow] s)
  | "magenta" -> ANSITerminal.(print_string [magenta] s)
  | "cyan" -> ANSITerminal.(print_string [cyan] s)
  | "bold" -> ANSITerminal.(print_string [Bold; blue] s)
  | _ -> ANSITerminal.(print_string [green] s)

(* [build_table] creates a list of players with a given stack size (100) for
   simplicity *)
let build_table (names : string list) (stack_size : int) =
  match init_state (create_players (names) 100) 1 with
  | Legal t -> t
  | Illegal -> failwith "unable to initialize table"

(* [name_list_generic num_players] creates a string list with the user's name
   added to a list of length [num_players] of generic names *)
let name_list_generic (num_players : int) : string list =
  let username = print_string(" What's your name?\n\n > "); read_line() in
  let tbl_names = 
    map (fun x -> if (x = username) then "Mimno" else x) table_names in
  let gen_names = fst (first_n tbl_names (num_players - 1)) in
  rev (username :: gen_names)

(* [print_stage st] prints the stage in which the state of the game [st] is *)
let print_stage (st : State.t) : unit =
  ANSITerminal.(print_string [green;Bold] "\n Game Stage: ");
  match get_stage st with
  | Init -> print_string ("Initial\n");
  | Deal -> print_ansi ("Deal\n") "cyan";
  | Flop -> print_ansi ("Flop\n") "magenta";
  | Turn -> print_ansi("Turn\n") "yellow";
  | River -> print_ansi ("River\n") "green";
  | End -> print_ansi ("End\n") "bold"

(* [get_active_players table] takes in a state [st] and returns out those
   players which are still in the game, aka, those which have an active field
   equal to true *)
let get_active_players (st : State.t) : Poker.player list =
  st 
  |> get_players 
  |> filter (fun x -> is_active x = true)

(* [print_pot table] prints the amount in the pot *)
let print_pot table : unit =
  let pot = table |> get_pot in
  print_string("\n | Pot: " ^ (string_of_int pot) ^ "\n")

(** [print_community_cards st] prints the field community_cards in the state
    [st] *)
let print_community_cards (st : State.t) (color_print : bool) : unit =
  let community_cards = 
    st
    |> get_community_cards
    |> card_list_to_string in
  if color_print then
    print_ansi ("\n The cards on the table are: " ^ community_cards) "blue"
  else (
    print_string (" | ");
    print_ansi ("Community cards: " ^ community_cards ^ "\n") "magenta")

(* [print_hole_cards table] prints the hole cards for player 1, which we assume 
   here to be the user *)
let print_hole_cards (st : State.t) (color_print : bool) : unit =
  let hole_cards = 
    st
    |> get_players
    |> hd
    |> get_hole_cards
    |> card_list_to_string in
  if color_print then
    print_ansi ("Your hole cards are: " ^ hole_cards ^ "\n\n") "blue"
  else (
    print_string (" | ");
    print_ansi ("Cards: " ^ hole_cards ^ "\n") "cyan")
(** print_string (" | Cards: " ^ hole_cards ^ "\n") *)

(* [get_player_stacks players] returns a string representation of players' 
   current information.
   For example, information for three players, each with a stack of 150 would 
   be:
    [ | Player 1-150 | Player 2-150 | Player 3-150 | ] *)
let rec get_player_stacks 
    (players : Poker.player list) 
    (cp_name: string) 
    (bb_name : string) 
    (sb_name : string)
  : string =
  match players with
  | [] -> " |"
  | h :: t ->
    (" | " ^ Poker.get_name h ^ " — " ^ (string_of_int (get_stack h))
     ^ (if Poker.get_name h = bb_name then " (Big Blind)" else "")
     ^ (if Poker.get_name h = sb_name then " (Small Blind)" else ""))
    ^ get_player_stacks t cp_name bb_name sb_name

(* [print_player_info players] returns information about the players in the 
   game. Specifically it prints a line with name of the active players and 
   their stack amounts.

   For example, information for three players, each with a stack of 150, player 
   1 has the big blind would be:
   [ | Player1-150 (Big Blind) | Player2-50 | Player3-50 | ] *)
let print_player_info (st : State.t) : unit =
  let players = get_active_players st in
  let cp_name = Poker.get_name ((current_player) st) in
  let bb_name = Poker.get_name ((get_big_blind) st) in
  let sb_name = Poker.get_name ((get_small_blind) st) in
  print_string (get_player_stacks players cp_name bb_name sb_name)

(* [print_winners winners] prints the names of round winners and their winning 
   hands *)
let rec print_winners (winners : (Poker.player * Poker.hand) list) : unit =
  match winners with
  | [] -> ()
  | (player, hand) :: t -> 
    let name = Poker.get_name player in
    let hand = hand_to_string hand in
    let msg = "\n\n - " ^ name ^ " with hand: " ^ hand  ^ "\n" in
    ANSITerminal.(print_string [Bold; blue] msg); 
    print_winners t

(* [transition st trans] takes in a game state [st] (at whatever point in the 
    game of poker that it may represent) and applies to it a transition function
    [trans], which in turn creates a new State.t object with said transition 
    applied. *)
let transition (st : State.t) (trans : State.t -> State.t) : State.t =
  let new_stage = incr_stage st in
  trans new_stage


let is_playing id st =
  match find_opt (fun x -> Poker.get_ID x = id) (st |> get_players) with
  | Some p -> true
  | None -> false

(* [play_bot_acton st] takes in the command for a bot represented by player [p]
   and returns [st], the state after the command is performed on the game. *)
let play_bot_action
    (st : State.t) 
    (p : Poker.player)
  : State.t =
  match MyTestBot.get_action st p with
  | Raise i ->
    begin match State.raise st p i with
      | Legal new_st -> 
        print_ansi 
          ("\n " ^ (Poker.get_name p) ^ " has chosen to raise " ^
           (new_st |> get_call_cost |> string_of_int) ^ "\n") "green"; 
        new_st
      | Illegal -> failwith "Bot cannot call"
    end
  | Call ->
    begin match State.call st p with
      | Legal new_st -> if get_call_cost st <> 0 then
          (print_ansi 
             ("\n " ^ (Poker.get_name p) ^ " has chosen to call " ^ 
              (st |> get_call_cost |> string_of_int) ^ "\n") "green"; )
        else 
          print_ansi 
            ("\n " ^ (Poker.get_name p) ^ " has chosen to check \n") "green"; 
        new_st
      | Illegal -> failwith "Bot cannot call"
    end
  | Fold -> begin 
      let new_st = State.fold st p in
      print_ansi 
        ("\n " ^ (Poker.get_name p) ^ " has chosen to fold\n") "green"; 
      new_st
    end
  | _ -> failwith "Bot command not supported"

(* [play_bots st] plays the commands for the bots in a round where only the
   user (aka player 1) has had their action processed. The idea here is to use 
   fold_left on the list of players besides the user. As such, when replaced 
   with the types being used here, the type of fold_left
   would be:
    (state -> player -> state) -> player list -> state
   Therefore, the state of the game [st] must be such that the current player is 
   the second player. This function returns a game state in which the last n - 1 
   players in an n-player table have made a decision.
*)
let play_bots (st : State.t) n : State.t =
  if n = 0 then (
    st
    |> get_active_players
    |> tl
    |> fold_left play_bot_action st)
  else st



(* [finish_game st] is a function that prints the necessary things once a game
   in state [st] is recognized to be over (aka there is only player with money
   left). *)
let finish_game (st : State.t) : unit =
  let 
    last_man_standing = st |> get_active_players |> List.hd |> Poker.get_name 
  in
  print_ansi 
    (" Congratulations " 
     ^ last_man_standing 
     ^ "! \n\n You've reached the end of the game. \n\n")
    "green"

let opt_to_keyword (input : string) : string =
  match input with
  | "ante" -> "go"
  | "check" -> "call"
  | "ok" -> "go"
  | _ -> input

let opt_descriptions (opt : string) : string =
  match opt with
  | "ante" -> " pay blind and deal cards."
  | "check" -> "bet nothing."
  | "call" -> " match the current bet or raise."
  | "fold" -> " forfeit your hand."
  | "raise" -> "bet a number on your hand."
  | "leave" -> "leave the table and quit the game."
  | "hand" -> " see your best current hand."
  | _ -> ""

let print_opts (opts : string list) : unit =
  print_string "\n  Commands  ————————————————————————————————\n";
  for i = 0 to (length opts) - 1 do
    print_string (" | ");
    ANSITerminal.(print_string [Bold] (nth opts i) );
    print_string (": " ^ opt_descriptions (nth opts i) ^ "\n");
  done;
  print_string "  —————————————————————————————————————————\n";
  print_string "\n"

let print_opts_short (opts) : unit =
  print_string " | Commands: ";
  for i = 0 to (length opts) - 1 do
    if i = (length opts) - 1 then
      print_string ({|"|} ^ nth opts i ^ {|"|})
    else
      print_string ({|"|} ^ nth opts i ^ {|"|} ^ ", ");
  done;
  print_string "\n\n"

let get_opts (st : State.t) : string list =
  match get_stage st with
  | Init -> ["ante"; "leave"]
  | Deal -> ["check"; "call"; "fold"; "raise"; "leave"]
  | Flop -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]
  | Turn -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]
  | River -> ["hand"; "check"; "call"; "fold"; "raise"; "leave"]
  | End -> ["ok"]

let print_malformed (st : State.t) : unit =
  let 
    msg = "\n This command is not appropriate, please enter one of the commands" 
          ^ " above or type help.\n"
  in
  print_ansi msg "red"

(** [play_round st] takes in a state [st] and transition function and returns 
    the state of the game after said transition is applied. The pattern for each
    individual round is:
    i) Apply the transition function (one of "deal," "flop," "turn," "river")
    ii) Process player_actions of the n-1 "bots." *)
let rec play_round (st : State.t) (trans : State.t -> State.t) n: State.t =
  let after_bots = if (State.get_stage st = Init) then st else play_bots st n in
  let after_check = raise_check after_bots trans n in
  let after_trans = transition after_check trans in
  if n= 0 then print_state after_trans;
  after_trans

and raise_check st trans n =
  if get_call_cost st = 0 || n > 0 then st
  else (
    print_state st;
    prompt_user_command st false)

and skip_bots st trans =
  transition st trans

(* [play_command st cmd] takes in a commmand [cmd] from the user and uses it to
   pattern match it to a new state which is just a transition function applied
   to the input state [st] *)
and play_command (st : State.t) (cmd : Command.t) : State.t =
  let 
    no_cards_msg = "\n You have not been dealt any cards yet."
                   ^ " Please pick a different option. \n\n" 
  in
  let to_next_stage =
    match get_stage st with
    | Init -> deal
    | Deal -> flop
    | Flop -> turn
    | Turn -> river
    | River -> (fun st -> st)
    | End -> deal
  in
  let user = get_player_by_id st 0 in
  match cmd with
  | Start -> play_round st to_next_stage 0
  | Hand -> 
    if (get_stage st = Init || get_stage st = Deal) 
    then (print_ansi no_cards_msg "red"; st)
    else 
      let 
        hand_str = hand_to_string (get_best_hand user (get_community_cards st)) 
      in
      print_ansi ("\n Your best hand is: " ^ hand_str ^ "\n\n") "cyan";
      st
  | Call -> 
    if (get_stage st = Init) 
    then (print_ansi no_cards_msg "red"; st)
    else
      begin match call st user with
        | Legal new_st -> if get_call_cost st <> 0 then
            (print_ansi ("\n You have chosen to call " ^
                         (new_st |> get_call_cost |> string_of_int) ^ 
                         "\n") "green";
             play_round (decr_stage new_st) to_next_stage 1)
          else (print_ansi "\n You have chosen to check\n" "green";
                play_round new_st to_next_stage 0)
        | Illegal -> 
          print_ansi (" \n You are unable to call" ^
                      (st |> get_call_cost |> string_of_int) ^ "\n\n") "red"; 
          st
      end
  | Fold -> print_string "\n You have chosen to fold\n\n"; 
    play_round (fold st user) to_next_stage 1
  | Raise c -> 
    if (get_stage st = Init) 
    then (print_ansi no_cards_msg "red"; st) 
    else 
      begin match raise st user c with
        | Legal new_st -> 
          print_ansi ("\n You have chosen to raise " ^ string_of_int c ^ "\n") 
            "green";
          if get_call_cost st = 0 then 
            play_round (decr_stage new_st) to_next_stage 1 else (
            print_ansi "\n You are not allowed to raise again.\n\n" "red"; 
            st)
        | Illegal -> 
          print_ansi "\n You are unable to raise this amount\n\n" "red"; 
          st
      end
  | Help -> print_opts (get_opts st); st
  | Quit -> print_ansi "\n\n Thanks for playing!\n\n" "green"; Stdlib.exit 0

and prompt_user_command (st : State.t) (same : bool) : State.t =
  if 
    (st 
     |> get_active_players 
     |> List.filter (fun x -> get_ID x = 0) 
     |> List.length = 0) || (st |> get_active_players |> List.length = 1)
  then play_command st Fold
  else (
    print_prompt_line same;
    print_string (" > ");
    let input = read_line() in
    match parse (opt_to_keyword input) with
    | exception Malformed -> print_malformed st; prompt_user_command st true
    | cmd -> begin match play_command st cmd with
        | same_st when same_st = st -> prompt_user_command same_st true
        | new_st -> new_st
      end
  )

and print_prompt_line same =
  if not same then let msg = 
                     " Please input a command below, "
                     ^ {|or type "help" for a list of possible commands.|}
                     ^ "\n "
    in
    print_ansi msg "green";
    line_div 80


and print_ante st =
  if get_stage st = Init then
    let small_blind = st |> get_blind_amount in
    let big_blind = small_blind * 2 in
    print_string (" | Ante: " ^ string_of_int big_blind ^ ", Small Blind: " ^
                  string_of_int small_blind ^ ", Big Blind: " ^ 
                  string_of_int big_blind ^ "\n")

(* [print_state st] prints information about the state [st], primarily
   - players (as well as their roles and stack amounts)
   - the pot
   - community cards (if any)
   - the user's hole cards (if any)
   - the user's viable responses *)
and print_state (st : State.t) : unit =
  if (get_stage st <> End) then (
    print_stage st;
    print_player_info (st);
    print_pot st;
    print_community_cards st false;
    print_hole_cards st false;
    print_ante st;
    print_opts_short (get_opts st))

let rec game_flow (st : State.t) : unit =
  print_state st;
  let init_st = st |> pay_big_blind |> pay_small_blind |> pay_ante in
  let deal_st = prompt_user_command init_st false in
  let flop_st = prompt_user_command deal_st false in
  let turn_st = prompt_user_command flop_st false in
  let river_st = prompt_user_command turn_st false in
  let end_st = prompt_user_command river_st false in
  (* let end_round = prompt_user_command_dep river_st in *)
  let after_subgame_st = (end_subgame end_st) in
  print_ansi "\n\n WINNER(s):" "bold";
  print_winners ((get_winners river_st));
  print_string ("\n");
  if List.length (get_active_players after_subgame_st) < 2 
  then finish_game after_subgame_st 
  else game_flow (incr_subgame after_subgame_st)

let play_game (num_players : int) : unit =
  let name_list = name_list_generic num_players in
  let init_st = build_table name_list 100 in
  game_flow init_st

(* [try_game input] is simply a way of constraining the user's input when 
   prompted for the number of players they would like to play with. *)
let rec try_game (input : string) =
  let num_error_msg = 
    "'" ^ input ^ "'" 
    ^ " is not a valid number. Please enter an integer from 2-9\n" 
  in
  match int_of_string input with
  | exception (Failure s) -> 
    print_string num_error_msg; 
    print_string " > "; 
    try_game (read_line())
  | x when (x > 9 || x < 2) -> 
    print_string num_error_msg; 
    print_string " > "; 
    try_game (read_line())
  | x -> play_game x

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_ansi "\n\n Welcome to 3110 Poker.\n" "green";
  let open_msg = 
    " Please enter a number of players [2 - 9]"
    ^ " that you would like at your poker table.\n" 
  in
  print_endline open_msg;
  print_string  " > ";
  match read_line () with
  | exception End_of_file -> ()
  | num_players -> try_game num_players

(* Execute the game engine. *)
let () = main ()
