(** The module that handles each state and transitions for a poker game. *)

(** [stage] is the abstract type of the stage of the game *)
type stage = Init | Deal | Flop | Turn | River | End

(** [t] is the abstract type of values representing the game state. *)
type t

(** [result] is the type representing the result of an attempted bet *)
type result = Legal of t | Illegal

(** [init_state players blind] initalizes a game state with [players] and a
    [blind] that is equal to the small blind and half of the big blind. *)
val init_state : Poker.player list -> int -> result

(** [get_subgame t] returns how many subgames have past
    Example: when the game is first initalized, [get_subgame t] returns 0
          after end_subgame has been called once [get_subgame t] returns 1*)
val get_subgame : t -> int

(** [get stage t] returns the current stage of the subgame *)
val get_stage : t -> stage

(** returns the player after [player] in circular-list [players]
    requires: [player] in [players]
    Examples:
     players = [1,3,6] get_next_player 3 players is 6
     players = [1,3,6] get_next_player 6 players is 1*)
val get_next_player : Poker.player -> Poker.player list -> Poker.player

(** [current_player t] returns the player to act*)
val current_player : t -> Poker.player

(** [get_players t] returns a list of players in the game*)
val get_players : t -> Poker.player list

(** [get_active_players t] returns a list of ACTIVE players in the game*)
val get_active_players : t -> Poker.player list

(** [get_big_blind t] returns the player who currently has the big blind *)
val get_big_blind : t -> Poker.player

(** [get_small_blind t] returns the player who currently has the small blind *)
val get_small_blind : t -> Poker.player

(** [get_big_blind t id] returns the player with id [id] *)
val get_player_by_id : t -> int -> Poker.player

(** [get_community_cards t] returns a list containing the community cards.
    'community cards' refers to the cards on the table not in players hands
    i.e. the cards from the flop turn and river*)
val get_community_cards : t -> Poker.card list

(** [get_pot t] returns the pot
    The pot is the sum of all the players bets *)
val get_pot : t -> int

(** [get_call_cost t] returns the call cost
    the call cost is the price a player must bet to continue playing *)
val get_call_cost : t -> int

(** [get_blind_amount t] returns the blind amount in state [t] *)
val get_blind_amount : t -> int

(* [play_ante st] takes the amount of the ante out of all currently active
   players, then returns the state of the game in which all players have paid
   the ante amount. *)
val pay_ante : t -> t

(* [play_big_blind st] takes the amount of the big blind out of the player who 
   has it, and then returns the state of the game in which the player with the
   big blind has payed it. *)
val pay_big_blind : t -> t

(* [play_small_blind st] takes the amount of the small blind out of the player
   who has it, and then returns the state of the game in which the player with
   the small blind has payed it. *)
val pay_small_blind : t -> t

(** [get_deck] returns all the cards that have not been delt *)
val get_deck : t -> Poker.card list

(** [incr_subgame t] increments the subgame counter by 1*)
val incr_subgame : t -> t

(** [incr_stage t] returns t with the next state
    example: if t was in stage Flop, [incr_state t] would return
    {t with stage=Turn} *)
val incr_stage : t -> t

(** [decr_stage t] returns t with the previous state
    example: if t was in stage Flop, [decr_state t] would return
    {t with stage=Deal} *)
val decr_stage : t -> t

(** [raise t p amount] returns legal of t if p has more than amount in stack
    and Illegal otherwise. t is also updated to include new call cost, pot, and 
    updated players *)
val raise : t -> Poker.player -> int -> result

(** [call t p] returns legal of t if player p has enough in stack to cover the 
    cost to call. 
    returns Illegal otherwise. if legal, t is updated to reflect new pot and 
    player stacks. *)
val call : t -> Poker.player -> result

(** [fold t p] returns t with p as inactive. inactive players are no longer able
    to win the subgame. *)
val fold : t -> Poker.player -> t

(** [first_n lst n] returns a tuple, the first element is the first n elements
    of [lst] and the second element is the rest of [lst] *)
val first_n : 'a list -> int -> 'a list * 'a list

(** [deal t] gives all players two hole cards *)
val deal : t -> t

(**  [flop t] adds three cards from the deck to the community cards. These
     cards are removed from the deck.*)
val flop : t -> t

(** [turn t] adds one card from the deck to the community cards. This card is 
    removed from the deck*)
val turn : t -> t

(** [river t] adds one card from the deck to the community cards. This card is 
    removed from the deck*)
val river : t -> t

(** [get_winners t] gets the winners of the current state.  *)
val get_winners : t -> (Poker.player * Poker.hand) list

(** [end_subgame t] ends current subgame. 
    the returned state gives winner(s) the pot and moves blinds. 
    it also gets a newly shuffled deck and resets the pot and call costs *)
val end_subgame : t -> t