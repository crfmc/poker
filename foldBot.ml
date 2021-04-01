open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  let valid_bot = 
    match I.diff with
    | Fold -> ()
    | _ -> failwith "FoldBot must have difficulty Fold"

  let get_action s = (Fold : Command.command)

end