type stage = Init | Deal | Flop | Turn | River | End

let string_of_stage_to_string stage = 
  match stage with 
  | Init -> "Init"
  | Deal -> "Deal"
  | Flop -> "Flop"
  | Turn -> "Turn"
  | River -> "River"
  | End -> "End"

type t = {
  subgame_number: int;
  game_stage: stage;
  players: Poker.player list;
  small_blind : Poker.player;
  big_blind : Poker.player;
  player_to_act: Poker.player;
  pot: int;
  community_cards: Poker.card list;
  call_cost: int;
  deck : Poker.card list;
  blind_amount : int;
}

type result = Legal of t | Illegal


(** returns the player after [player] in circular-list [players]
    requires: [player] in [players]
    Examples:
     players = [1,3,6] get_next_player 3 players is 6
     players = [1,3,6] get_next_player 6 players is 1*)
let get_next_player player players = 
  try
    let rec helper lst next = 
      match lst with
      | [] -> if next then List.hd players else failwith "player not in players"
      | h::t -> if next then h else 
        if Poker.get_ID h = Poker.get_ID player then helper t true 
        else helper t false 
    in
    helper players false
  with 
  | _ -> player

let init_state players blind= 
  if 1< List.length players && List.length players < 10 then
    let state = {subgame_number = 0; game_stage = Init; players = players; 
                 big_blind= List.hd players;
                 player_to_act = List.hd players; pot = 0;
                 community_cards = []; call_cost=0; 
                 deck=Poker.get_shuffled_deck (); 
                 small_blind = (get_next_player (List.hd players) players); 
                 blind_amount=blind;}
    in 
    if List.length players= 2 then 
      Legal {state with big_blind=state.small_blind;
                        small_blind= state.big_blind;
                        player_to_act=state.big_blind} 
    else 
      Legal {state with player_to_act=get_next_player state.small_blind players}
  else Illegal

let get_subgame state =
  state.subgame_number

let get_stage state =
  state.game_stage

let current_player state = 
  state.player_to_act

let get_players state = 
  state.players

let get_active_players state = 
  List.filter Poker.is_active (get_players state)

let get_big_blind state =
  state.big_blind

let get_small_blind state =
  state.small_blind

let get_community_cards state = 
  state.community_cards

let get_pot state = 
  state.pot

let get_call_cost state = 
  state.call_cost

let get_blind_amount state =
  state.blind_amount

let pay_ante state =
  let updated_player_list = 
    List.map 
      (fun player -> 
         if Poker.is_active player
         then (Poker.alter_stack player (-2*state.blind_amount)) 
         else player) 
      state.players
  in
  {state with players = updated_player_list;
              pot = state.pot + (List.length (get_active_players state)) 
                                * 2 * state.blind_amount}

let pay_big_blind state =
  let big_blind = get_big_blind state in
  let updated_player_list = 
    List.map 
      (fun player -> 
         if Poker.get_ID player = Poker.get_ID big_blind 
         then (Poker.alter_stack player (-2*state.blind_amount)) 
         else player) 
      state.players
  in
  {state with players = updated_player_list;
              pot = state.pot + 2*state.blind_amount}

let pay_small_blind state =
  let small_blind = get_small_blind state in
  let updated_player_list = 
    List.map 
      (fun player -> 
         if Poker.get_ID player = Poker.get_ID small_blind 
         then (Poker.alter_stack player (-state.blind_amount)) 
         else player) 
      state.players
  in
  {state with players = updated_player_list;
              pot = state.pot + state.blind_amount}

let get_deck state =
  state.deck

let incr_subgame state =
  let current_num = state.subgame_number in
  {state with subgame_number = current_num + 1; call_cost = 0;}

let incr_stage state =
  let new_stage =
    match get_stage state with
    | Init -> Deal
    | Deal -> Flop
    | Flop -> Turn
    | Turn -> River
    | River -> End
    | End -> Deal
  in
  {state with game_stage = new_stage}

let decr_stage state =
  let new_stage =
    match get_stage state with
    | Init -> End
    | Deal -> Init
    | Flop -> Deal
    | Turn -> Flop
    | River -> Turn
    | End -> River
  in
  {state with game_stage = new_stage}

let get_player_by_id state id = 
  List.find (fun x -> Poker.get_ID x = id) state.players

let ante state player =
  if 2 * state.blind_amount >Poker.get_stack player then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then 
        Poker.alter_stack x (-2 * state.blind_amount)
      else x) state.players in 
    Legal {state with players =  new_players; 
                      pot = state.pot + 2 * state.blind_amount;
                      call_cost = state.call_cost;
                      player_to_act=(get_next_player player new_players);}

let raise state player amount = 
  if amount + state.call_cost > Poker.get_stack player || amount < 0 
  then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then 
        Poker.alter_stack x (-(amount+state.call_cost))
      else x) state.players in 
    Legal {state with players =  new_players; 
                      pot = state.pot + state.call_cost + amount;
                      call_cost = state.call_cost + amount;
                      player_to_act=(get_next_player player new_players);}

let call state player = 
  if state.call_cost > Poker.get_stack player then Illegal
  else let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then 
        Poker.alter_stack x (-state.call_cost)
      else x) state.players in 
    Legal {state with players =  new_players; pot = state.pot + state.call_cost;
                      player_to_act=(get_next_player player new_players);}

let fold state player = 
  let new_players = List.map (fun x -> 
      if Poker.get_ID x = Poker.get_ID player then Poker.set_inactive x
      else x) state.players in
  {state with players = new_players; 
              player_to_act=(get_next_player player new_players);}

let rec first_n_helper list num stop acc = 
  match list with
  | [] -> acc,list
  | h::t -> if num = stop then acc,list 
    else first_n_helper t (num+1) stop ([h]@acc)


let first_n list n = 
  first_n_helper list 0 n []

let rec deal_helper players deck acc = 
  match players with
  | [] -> deck,acc
  | h::t -> 
    let hole_cards , remaining_deck = first_n deck 2 in
    let sorted_hole_cards = List.sort Poker.compare hole_cards in
    let new_player = Poker.set_hole_cards h sorted_hole_cards in
    deal_helper t (remaining_deck) (new_player::acc)

let deal state =
  let rev_player = List.rev state.players in
  let deck, players = deal_helper rev_player state.deck [] in
  {state with players= players; deck=deck; community_cards=[];
              call_cost=0;}

let flop state = 
  if List.length state.community_cards = 3 then state 
  else
    let flop_cards, remaining_deck = first_n state.deck 3 in
    {state with community_cards=flop_cards; deck = remaining_deck;
                call_cost=0;}

let turn state = 
  if List.length state.community_cards = 4 then state 
  else
    let turn_card, remaining_deck = first_n state.deck 1 in
    {state with community_cards= List.concat [state.community_cards;turn_card];
                deck = remaining_deck; call_cost=0;}

let river state = 
  if List.length state.community_cards = 5 then state 
  else
    let river_card, remaining_deck = first_n state.deck 1 in
    {state with community_cards= List.concat [state.community_cards;river_card];
                deck = remaining_deck;call_cost=0;}

let distribute_pot winners players pot = 
  List.map (fun x -> if List.mem x winners 
             then Poker.alter_stack x (pot/(List.length winners))
             else x) players

let get_winners state = 
  let player_to_tup x = (x, (Poker.get_best_hand x state.community_cards)) 
  in
  let best_hands_1 = List.map player_to_tup (get_active_players state) in
  let best_hands = 
    List.filter (fun x -> Poker.is_active (fst x)) best_hands_1 in
  let sorted_hands = 
    List.sort (fun x y -> Poker.hand_compare (snd x) (snd y)) best_hands |>
    List.rev
  in

  let best_hand = snd (List.hd sorted_hands) in
  List.filter (fun x -> Poker.hand_compare (snd x) best_hand = 0) sorted_hands 

let end_subgame state =
  let winners,_ = List.split (get_winners state) in
  let new_players' = distribute_pot winners state.players state.pot |> 
                     List.filter (fun x -> 
                         Poker.get_stack x > 2*state.blind_amount)
                     |> List.map (Poker.set_active) 
  in
  let new_players =  List.map (fun x -> Poker.set_hole_cards x []) new_players'
  in
  let new_state = 
    {state with players=new_players;
                game_stage=Init;
                pot = 0;
                community_cards = []; 
                call_cost=0; 
                deck=Poker.get_shuffled_deck ();
                big_blind = get_next_player state.big_blind state.players;
                small_blind = get_next_player state.small_blind state.players;
    }
  in 
  if List.length new_players != 2 then new_state
  else {new_state with big_blind=state.small_blind;
                       small_blind= state.big_blind;
                       player_to_act=state.big_blind} 