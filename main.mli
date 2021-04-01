(** The main interface of the poker game. *)

(** [prompt_user st] parses a user's input into a command, and performs the
    command by applying a it to the state [st] in order to advance the game
    and show relevant information to the user. *)
val prompt_user_command : State.t -> bool -> State.t

(** [print_state st] prints information about the current state of the 
    game [st]. For example, the very first time print_state is called, it should 
    show the players in the game, their stacks, and the pot. Later it should 
    include the community cards as well as the user's hole cards. *)
val print_state : State.t -> unit

(** [play_game num_players] begins a game of poker with a certain amount of
    players [num_players]. Usually this will be referred to as an n-player game
    or n-player table, where n is [num_players] *)
val play_game : int -> unit