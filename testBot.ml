open Bot

module Make = functor (I : BotInfo) -> struct

  module Info = I

  let valid_bot = 
    match I.diff with
    | Test -> ()
    | _ -> failwith "FoldBot must have difficulty Fold"

  let get_action s p = 
    let open State in
    match State.get_stage s with
    | River -> (Fold :Command.t)
    | _ -> (Call : Command.t)
end