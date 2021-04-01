(** A [Bot] takes a game state and produces a command *)


(** [difficulty] represents a bot's level of complexity and skill*)
type difficulty = Test | Easy | Medium | Hard

(** A module that matches [BotInfo] is suitable for use as
    the type of Info in a [Bot]. *)
module type BotInfo = sig
  val diff : difficulty
  val seed : int
end

(** A [Bot] takes a game state and produces a command *)
module type Bot = sig

  (** [Info] is a module representing information about the bot*)
  module Info : BotInfo

  (** [get_action s] take a game state and produces a command.
      The bot acts from the perspective of [State.current_player s] *)
  val get_action : State.t -> Poker.player -> Command.t
end

module type BotMaker = 
  functor (I : BotInfo) -> Bot with module Info = I