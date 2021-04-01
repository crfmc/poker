(** The module handeling player actions *)

(** A command consists of an action and possibly an action amount as well. For
    example, a command can be ["start"] or it can also be ["Raise 56"]. The
    former begins the game, the latter raises the bet by 56 coins. *)
type t = | Raise of int | Start | Hand | Call | Fold | Quit | Help


(** [Malformed] is raised when a player's input string cannot be parsed into a
    command. A command is {i malformed} if the string contains no action,
    or if the action is "raise" and there is not a number following it,
    or if the action is not "raise" but there is something following it.
    [Failure "int_of_string"] if the substring following "raise" is not a
    string representation of an int*)
exception Malformed

(** [Command.parse str] parses a player's input [str] into a command as follows. 
    The first word will always be the action taken by the player. This can be to 
    raise the bet, start the game, see your best hand, call, fold, quit the 
    game, or get help.
    Requires: [str] contains only lowercase alphanumeric (a-z, 0-9) and space 
    characters (only ASCII character code 32; not tabs or newlines, etc.). If 
    there is a substring following the action, then the command is 
    Raise and the substring consists of a string representation of an int.
    Raises: [Malformed] if the command is malformed.  *)
val parse : string -> t


