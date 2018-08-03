open System.IO
open Shared
open Elmish.Bridge
open Elmish
open System
let publicPath = Path.GetFullPath "./public"
type ServerMsg =
  | RS of RemoteServerMsg
  | Closed
let port = System.Environment.GetEnvironmentVariable("PORT") |> uint16

type State =
  | Connected of User
  | Disconnected

let connections =
  ServerHub<State,ServerMsg,RemoteClientMsg>()
    .RegisterServer(RS)

type History<'a> = {
  Get: unit -> 'a list
  Put: 'a -> unit
  }
let history =
  let mb =
    MailboxProcessor.Start
     (fun (mb:MailboxProcessor<Choice<AsyncReplyChannel<Msgs list>,Msgs>>) ->
       let rec loop l =
        async {
          let! msg = mb.Receive()
          match msg with
          |Choice1Of2 r ->
            r.Reply l
            return! loop l
          |Choice2Of2 m ->
            return! loop (m::l |> List.truncate 50)}
       loop [])
  {Get = fun () -> mb.PostAndReply (fun e -> Choice1Of2 e)
   Put = fun m -> mb.Post(Choice2Of2 m)}

let update clientDispatch msg state =
 match msg with
 | Closed ->
      match state with
      |Disconnected -> ()
      |Connected u ->
        connections.BroadcastClient(RemoveUser u.Name)
        let msg = SysMsg {Time=System.DateTime.Now; Content = u.Name+" left the room"}
        history.Put msg
        connections.BroadcastClient(AddMsg msg)
      Disconnected, Cmd.none
 |RS msg ->
  match state, msg with
  | _, UsersConnected ->
      let users =
        connections.GetModels()
        |> Seq.choose (function Disconnected -> None | Connected u -> Some u)
        |> Seq.toList
      clientDispatch (GetUsers users)
      clientDispatch (AddMsgs (history.Get()))
      state, Cmd.none
  | Disconnected, SetUser u ->
      if connections.GetModels() |> Seq.exists (function Disconnected -> false | Connected {Name=n} -> n=u.Name) then
        clientDispatch (AddMsg (SysMsg {Time=System.DateTime.Now; Content = "Name is in use"}))
        clientDispatch (NameStatus false)
        state, Cmd.none
      else
        let state = Connected u
        connections.BroadcastClient(AddUser u)
        let msg = SysMsg {Time=System.DateTime.Now; Content = u.Name+" joined the room"}
        history.Put msg
        connections.BroadcastClient(AddMsg msg)
        clientDispatch(NameStatus true)
        state, Cmd.none
  | Disconnected, _  | _, SetUser _ -> state, Cmd.none
  | (Connected u),SendMsg m ->
      if String.IsNullOrWhiteSpace m then
          ()
      else
          let msg = ClientMsg (u.Name,{Content=m;Time = DateTime.Now})
          history.Put msg
          connections.BroadcastClient(AddMsg msg)
      state, Cmd.none
  | (Connected u),ChangeColor c ->
      connections.BroadcastClient(ColorChange(u.Name,c))
      Connected {u with Color = c}, Cmd.none
let init (clientDispatch:Dispatch<RemoteClientMsg>) () =
  clientDispatch QueryConnected
  Disconnected, Cmd.none

open Saturn.Application
let server =
  Bridge.mkServer Remote.socketPath init update
  |> Bridge.withConsoleTrace
  |> Bridge.register RS
  |> Bridge.whenDown Closed
  |> Bridge.withServerHub connections
  |> Bridge.run Giraffe.server

let app =
  application {
    use_static publicPath
    use_router server
    disable_diagnostics
    app_config Giraffe.useWebSockets
    url ("http://0.0.0.0:" + port.ToString() + "/")
  }

run app