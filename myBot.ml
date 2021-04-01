open Bot
open Poker

module Make = functor (I : BotInfo) -> struct

  module Info = I

  type outs = {
    hand_type : hand_tp;
    single : float;
    double : float;
    tripple : float;
    quad : float
  }

  type out_prob = {
    hand_type : hand_tp;
    prob: float
  }

  (** BEGIN MISC HELPERS *)

  (** Calculates ([n] choose [k]) *)
  let choose n k = 
    let rec helper i acc = 
      if i = k then acc
      else helper (i +. 1.) (acc *.  ( n +. 1. -. i ) /. i) 
    in 
    helper 1.0 1.0

  let get_ranks cards = 
    List.map (fun x -> fst x) cards

  (** Requires that ranks is sorted *)
  let rec to_rank_acoss ranks acc current = 
    match current with 
    | None ->  begin      
        match ranks with 
        | [] -> acc
        | h::t -> to_rank_acoss t acc (Some (h,1))
      end
    | Some curr -> begin
        match ranks with 
        | [] -> acc@[curr]
        | h::t -> begin 
            let r,n = curr in
            if r = h then to_rank_acoss t acc (Some (h,n+1))
            else to_rank_acoss t (acc@[curr]) (Some (h,1))
          end
      end

  let get_suits cards = 
    List.map (fun x -> snd x) cards

  (** Requires that ranks is sorted *)
  let rec to_suit_acoss suits acc current = 
    match current with 
    | None ->  begin      
        match suits with 
        | [] -> acc
        | h::t -> to_suit_acoss t acc (Some (h,1))
      end
    | Some curr -> begin
        match suits with 
        | [] -> acc@[curr]
        | h::t -> begin 
            let r,n = curr in
            if r = h then to_suit_acoss t acc (Some (h,n+1))
            else to_suit_acoss t (acc@[curr]) (Some (h,1))
          end
      end

  (** Takes a hand and returns the lowest better hand. 
      Example: inc_hand pair -> two pair since two pair is the next hand
      fails if you try to call it on royal flush since there is no better hand
  *)
  let inc_hand h = 
    match h with 
    | Royal_Flush-> failwith "inc_hand should not be called on royal flush"
    | Straight_Flush -> Royal_Flush
    | Four_Kind -> Straight_Flush
    | Full_House -> Four_Kind
    | Flush -> Full_House
    | Straight -> Flush
    | Three_Kind -> Straight
    | Two_Pair -> Three_Kind
    | Pair -> Two_Pair
    | High_Card -> Pair

  (** END MISC HELPERS *)

  let highcard_helper (cards : card list) = 
    let high_card = List.hd cards in
    let rank = Poker.rank_to_int (fst high_card) in 
    let single_outs = 14.0 -. float_of_int (rank) in 
    {hand_type=High_Card;
     single=single_outs;
     double=0.0;
     tripple=0.0;
     quad=0.0}

  (** Requires that cards is sorted *)
  let pair_helper (cards: card list) = 
    let ranks = get_ranks cards in
    let rank_freq = to_rank_acoss ranks [] None in 
    let single = float_of_int (
        List.fold_left 
          (fun num x -> if (snd x) < 2 then num + 3 else num)
          0 rank_freq 
      )
    in
    let diff = List.length (List.sort_uniq 
                              (fun x y -> (rank_to_int x) - (rank_to_int y) ) 
                              ranks ) 
               - List.length ranks
    in 
    let double = float_of_int (diff * 6) in 
    {hand_type=Pair;
     single=single;
     double=double;
     tripple=0.0;
     quad=0.0}

  (** to see how many ways we can get a two pair we need to look at
      the number of ways we can get a matching card and a pair for ranks 
      we only have one of. We also need to look at how many ways we can get
      another pair if we already have one. 

      these are the numbers computed for when snd x = 1 and snd x = 2.*)
  let two_pair_helper cards =
    let ranks = get_ranks cards in
    let rank_freq = to_rank_acoss ranks [] None in
    let temp_outs = {hand_type=Two_Pair;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let update_outs state x = 
      if snd x = 1 then 
        let trip = 3.0 *. 6.0 *. float_of_int (13- List.length rank_freq) in 
        {state with tripple=state.tripple +. trip}
      else if snd x = 2 then  
        let double = 6. *. float_of_int (13- List.length rank_freq) in
        let single = 4. *. float_of_int (List.length rank_freq) -. 1. in
        {state with single=state.single +. single;double=state.double +. double}
      else state
    in
    List.fold_left update_outs temp_outs rank_freq

  let three_helper cards = 
    let ranks = get_ranks cards in
    let rank_freq = to_rank_acoss ranks [] None in
    let temp_outs = {hand_type=Three_Kind;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let update_outs state x = 
      if snd x = 1 then 
        let double = 3.0  in 
        {state with double=state.double +. double}
      else if snd x = 2 then  
        let single = 4. *. float_of_int (List.length rank_freq) -. 1. in
        {state with single=state.single +. single;}
      else state
    in
    List.fold_left update_outs temp_outs rank_freq

  let cards_missing straight ranks = 
    let missing = List.filter (fun x -> not (List.mem x ranks)) straight in 
    let cards_needed = missing |> List.length in
    (** This accounts for aces being counted as 1 and 14 *)
    if List.hd missing = 1 && List.mem 14 ranks 
    then cards_needed - 1
    else cards_needed


  let straight_helper cards = 
    let rec helper checking ranks acc = 
      let missing = cards_missing checking ranks in
      let new_outs = 
        match missing with 
        | 1 -> {acc with single=acc.single +. 4.0 }
        | 2 -> {acc with double=acc.double +. 16.0 }
        | 3 -> {acc with tripple=acc.tripple +. 64.0 }
        | 4 -> {acc with quad=acc.quad +. 256.0 }
        | _ -> acc
      in
      if List.hd checking = 10 then new_outs 
      else helper (List.map (fun x -> x + 1) checking) ranks new_outs
    in
    let ranks = get_ranks cards |> List.map rank_to_int in
    let temp_outs = {hand_type=Straight;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let straight = [1;2;3;4;5] in
    helper straight ranks temp_outs


  let flush_helper cards = 
    let suits = get_suits cards in 
    let suit_freq = to_suit_acoss suits [] None in
    let temp_outs = {hand_type=Flush;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let update_outs state x = 
      if snd x = 1 then 
        let quad = choose 12. 4. in 
        {state with quad=state.quad +. quad}
      else if snd x = 2 then  
        let trip = choose 11. 3. in 
        {state with tripple=state.tripple +. trip}
      else if snd x = 3 then 
        let doub = choose 10. 2. in
        {state with double=state.double +. doub}
      else if snd x = 4 then 
        let singl = choose 9. 1. in 
        {state with single=state.single +. singl}
      else state
    in
    List.fold_left update_outs temp_outs suit_freq


  let full_helper cards = 
    let ranks = get_ranks cards in
    let rank_freq = to_rank_acoss ranks [] None in
    let temp_outs = {hand_type=Full_House;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let update_outs state x = 
      if snd x = 1 then
        let quad = (3.0 *. 12. *. 4.) +. (3.0 *. 12. *. 6.) in 
        {state with quad=state.quad +. quad}
      else if snd x = 2 then  
        let trip = 12. *. 4. in 
        {state with tripple=state.tripple +. trip}
      else if snd x = 3 then 
        let doub = (13. -. float_of_int(List.length rank_freq)) *. 6. in
        let sing = float_of_int(List.length rank_freq) -. 1. *. 3. in
        {state with double=state.double +. doub; single=state.single +. sing}
      else state
    in
    List.fold_left update_outs temp_outs rank_freq

  let four_helper cards = 
    let ranks = get_ranks cards in
    let rank_freq = to_rank_acoss ranks [] None in
    let temp_outs = {hand_type=Four_Kind;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let update_outs state x = 
      if snd x = 1 then 
        let tripple = 1.0  in 
        {state with tripple=state.tripple +. tripple}
      else if snd x = 2 then  
        let double = 1.0 in
        {state with double=state.double +. double;}
      else if snd x = 3 then
        let single = 1.0 in 
        {state with single=state.single +. single}
      else state
    in
    List.fold_left update_outs temp_outs rank_freq

  let straight_flush_helper cards = 
    let rec helper checking ranks acc = 
      let missing = cards_missing checking ranks in
      let new_outs = 
        match missing with 
        | 1 -> {acc with single=acc.single +. 1.0 }
        | 2 -> {acc with double=acc.double +. 1.0 }
        | 3 -> {acc with tripple=acc.tripple +. 1.0 }
        | 4 -> {acc with quad=acc.quad +. 1.0 }
        | _ -> acc
      in
      if List.hd checking = 10 then new_outs 
      else helper (List.map (fun x -> x + 1) checking) ranks new_outs
    in
    let hearts = List.filter (fun x -> snd x = Hearts) cards 
                 |> get_ranks |> List.map rank_to_int in
    let spades = List.filter (fun x -> snd x = Spades) cards 
                 |> get_ranks |> List.map rank_to_int in
    let clubs = List.filter (fun x -> snd x = Clubs) cards 
                |> get_ranks |> List.map rank_to_int in
    let diamonds = List.filter (fun x -> snd x = Diamonds) cards 
                   |> get_ranks |> List.map rank_to_int in
    let temp_outs = {hand_type=Straight;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let straight = [1;2;3;4;5] in
    List.fold_left (fun outs ranks -> helper straight ranks outs) 
      temp_outs [hearts;spades;clubs;diamonds]

  let royal_helper cards = 
    let rec helper checking ranks acc = 
      let missing = cards_missing checking ranks in
      let new_outs = 
        match missing with 
        | 1 -> {acc with single=acc.single +. 1.0 }
        | 2 -> {acc with double=acc.double +. 1.0 }
        | 3 -> {acc with tripple=acc.tripple +. 1.0 }
        | 4 -> {acc with quad=acc.quad +. 1.0 }
        | _ -> acc
      in
      if List.hd checking = 10 then new_outs 
      else helper (List.map (fun x -> x + 1) checking) ranks new_outs
    in
    let hearts = List.filter (fun x -> snd x = Hearts) cards 
                 |> get_ranks |> List.map rank_to_int in
    let spades = List.filter (fun x -> snd x = Spades) cards 
                 |> get_ranks |> List.map rank_to_int in
    let clubs = List.filter (fun x -> snd x = Clubs) cards 
                |> get_ranks |> List.map rank_to_int in
    let diamonds = List.filter (fun x -> snd x = Diamonds) cards 
                   |> get_ranks |> List.map rank_to_int in
    let temp_outs = {hand_type=Straight;
                     single=0.0;
                     double=0.0;
                     tripple=0.0;
                     quad=0.0} 
    in
    let straight = [10;11;12;13;14] in
    List.fold_left (fun outs ranks -> helper straight ranks outs) 
      temp_outs [hearts;spades;clubs;diamonds]

  (** Go through each possible hand and figure out how many ways this hand
      can be obtained. How many combinations of 1, 2, 3, or 4 cards allow you to
      reach a certain hand *)
  let rec generate_outs_list_helper curr cards acc = 
    match curr with 
    | Royal_Flush -> ([(royal_helper cards)]@acc)
    | Straight_Flush -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(straight_flush_helper cards)]@acc)
    | Four_Kind -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(four_helper cards)]@acc)
    | Full_House -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(full_helper cards)]@acc)
    | Flush -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(flush_helper cards)]@acc)
    | Straight -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(straight_helper cards)]@acc)
    | Three_Kind -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(three_helper cards)]@acc)
    | Two_Pair -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(two_pair_helper cards)]@acc)
    | Pair ->  
      generate_outs_list_helper (inc_hand curr) cards 
        ([(pair_helper cards)]@acc)
    | High_Card -> 
      generate_outs_list_helper (inc_hand curr) cards 
        ([(highcard_helper cards)]@acc)

  let generate_outs_list start cards = 
    generate_outs_list_helper start cards []

  let get_outs_list player best_hand com_cards = 
    generate_outs_list best_hand.tp
      (List.concat [player.hole_cards;com_cards] |> 
       List.sort compare |> List.rev )

  let calculate_prob_of_drawing_cards outs_list state = 
    let stage = State.get_stage state in
    (** the probability of drawing [a] specific cards in the next b draws 
        out of n cards is: (n - a) choose (b - a) / (n choose b)

        explanation: if you are choosing [a] cards then those are fixed.
        therefore, you need to ask the question: "how many combinations of the 
        remaining cards are there?" Well, you have n-a choises (you can't pick 
        the [a] cards you already picked) for b-a draws. 
        As such, there are (n-a) choose (b-a) hands which contain the [a] cards
        you care about. In total, there are n choose b hands of b cards. 
        This gets us the proability: (n - a) choose (b - a) / (n choose b)

        the calculations in get_prob are pre-computed by this formula to save
        computation time *)
    let get_prob (out : outs) = 
      let single_prob = 
        match stage with
        | Deal -> 0.1 
        | Flop -> 2. /. 47.
        | Turn -> 1. /. 46.
        | _ -> failwith "this should be unreachable"
      in
      let double_prob = 
        match stage with
        | Deal -> 2. /. 245.
        | Flop -> 1. /. 1081.
        | Turn -> 0.
        | _ -> failwith "this should be unreachable"
      in 
      let tripple_prob = 
        match stage with
        | Deal -> 1. /. 1960.
        | Flop -> 0.
        | Turn -> 0.
        | _ -> failwith "this should be unreachable"
      in
      let quad_prob = 
        match stage with 
        | Deal -> 1. /. 46060.
        | Flop -> 0.
        | Turn -> 0.
        | _ -> failwith "This should be unreachable"
      in
      {hand_type = out.hand_type;
       prob = ((single_prob *. out.single)
               +.(double_prob *. out.double)
               +.(tripple_prob *. out.tripple)
               +.(quad_prob *. out.quad)
              )}
    in
    List.map get_prob outs_list

  (** These probabilities are found on wikipedia:
      https://en.wikipedia.org/wiki/Poker_probability  *)
  let average_winning_prob = function 
    | Royal_Flush-> 0.999968
    | Straight_Flush -> 0.999689
    | Four_Kind -> 0.99801
    | Full_House -> 0.972
    | Flush -> 0.9418
    | Straight -> 0.896
    | Three_Kind -> 0.847
    | Two_Pair -> 0.612
    | Pair -> 0.174
    | High_Card -> 0.0 

  (** calculate probability of winning by going through each possible hand
      seeing how likely you are to have it and then combining it with
      how likely that hand is to win *)
  let calculate_prob_of_winning best_hand out_probs = 
    let helper acc x = 
      if x.hand_type = best_hand then acc +. (average_winning_prob x.hand_type)
      else acc +. (average_winning_prob x.hand_type *. x.prob)
    in 
    List.fold_left helper 0.0 out_probs

  (** Simple bet formulation strategy, if the expected value of calling is
      positive then call. Else, fold.  *)
  let formulate_bet prob state player : Command.t = 
    let pot = State.get_pot state in 
    let call_cost = State.get_call_cost state in 
    let stack = Poker.get_stack player in 
    if call_cost > stack then Fold 
    else
      let expected_value = prob *. (float_of_int pot) 
                           -. float_of_int call_cost in
      (** print_string (string_of_float expected_value); *)
      if prob >= 0.6 && call_cost = 0 then Raise (stack / 12)
      else if prob >= 0.99 && call_cost = 0 then Raise (stack)
      else if expected_value >= 0. then Call 
      else Fold

  let get_action s p : Command.t = 
    let stage = State.get_stage s in 
    let num_players = List.length (State.get_active_players s) in
    let com_cards = State.get_community_cards s in
    let best_hand = Poker.get_best_hand p com_cards in 
    if num_players = 1 then 
      (** let _ = print_string "last player" in *)
      Call 
    else
      match stage with 
      | Init -> Call
      | River -> 
        let winning_prob = calculate_prob_of_winning (best_hand.tp) [] in 
        formulate_bet winning_prob s p
      | _ -> 
        let outs_list = get_outs_list p best_hand com_cards in 
        let prob_list = calculate_prob_of_drawing_cards outs_list s in 
        let winning_prob = calculate_prob_of_winning (best_hand.tp) prob_list in 
        formulate_bet winning_prob s p

end 