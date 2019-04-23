module Client

open Elmish
open Elmish.React

open Fable.React

open Elmish.Bridge
open Shared
open ClientModel

let init () =
  {
    Connection = Disconnected
    ConnectedUsers = []
    Messages = []
    TextField = ""
    UserField = ""
    ColorField = Black
  }, Cmd.none
let update (msg : ClientMsg) (model : Model)  =
  match msg with
  | SendUser ->
    match model.UserField with
    |"" -> model, Cmd.none
    |_ ->
      Bridge.Send(SetUser {Name = model.UserField; Color = model.ColorField})
      {model with Connection=Waiting}, Cmd.none
  | ConnectionLost -> {model with Connection = Disconnected}, Cmd.none
  | RC msg ->
    match msg with
    | GetUsers l -> {model with ConnectedUsers = l}, Cmd.none
    | QueryConnected ->
        match model.Connection with
        |Connected u -> Bridge.Send(SetUser u)
        |Waiting | Disconnected -> ()
        Bridge.Send UsersConnected
        {model with ConnectedUsers = []}, Cmd.none
    | NameStatus s ->
        match model.Connection with
        |Waiting ->
          {model with Connection =  if s then Connected {Name = model.UserField; Color = model.ColorField} else Disconnected}
        |_ -> model
        , Cmd.none

    | AddUser u ->
      {model with ConnectedUsers = u::model.ConnectedUsers}, Cmd.none
    | RemoveUser u ->
      {model with
        ConnectedUsers =
          model.ConnectedUsers
          |> List.filter (fun {Name=n} -> n<>u)}, Cmd.none
    | AddMsg m ->
      {model with Messages = m::model.Messages}, Cmd.none
    | ColorChange (u,c) ->
      let newConnUsers = model.ConnectedUsers |> List.map (fun ({Name=n} as o) ->if n=u then {o with Color=c} else o )
      let newConn =
        match model.Connection with
        |Connected (({Name=us}) as user) when us = u -> Connected {user with Color = c}
        |e -> e
      {model with
        ConnectedUsers = newConnUsers
        Connection = newConn}
      ,Cmd.none
    | AddMsgs m -> {model with Messages = m}, Cmd.none
  | SetTextField tx -> {model with TextField = tx}, Cmd.none
  | SetUserField tx -> {model with UserField = tx}, Cmd.none
  | SetColor c ->
      match model.Connection with
      |Connected {Color=o} when o<>c -> Bridge.Send(ChangeColor c)
      |_ -> ()
      {model with ColorField = c},Cmd.none

#if DEBUG
open Elmish.HMR
#endif

let view =
  let v = FunctionComponent.Lazy(ClientView.view, div [][])
  fun m d -> v {| model=m;dispatch=d |}


Program.mkProgram init update view
|> Program.withBridgeConfig
  (Bridge.endpoint Remote.socketPath
  |> Bridge.withMapping RC
  |> Bridge.withWhenDown ConnectionLost)
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.run