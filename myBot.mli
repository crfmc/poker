(** A bot that folds and calls based on the expected value of doing so
    the bot creates a probabilistic model of attaining potential hands 
    and calculates the probability of those hands winning. *)
open Bot

(** [Make] makes a bot following a probabilistic / statistical model *)
module Make : BotMaker