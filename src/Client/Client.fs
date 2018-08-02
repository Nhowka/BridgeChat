module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared

open Elmish.Bridge
open Fable.Core.JsInterop
importAll "../../node_modules/bulma/bulma.sass"

type ClientMsg =
    | RC of RemoteClientMsg
    | SendUser
    | ConnectionLost
    | SetTextField of string
    | SetUserField of string
    | SetColor of Color

type Connection =
  | Disconnected
  | Waiting
  | Connected of User


type Model = {
    Connection : Connection
    ConnectedUsers : User list
    Messages : Msgs list
    TextField : string
    UserField : string
    ColorField : Color
}

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
open Fulma
let ColorToColor = function
  | Red -> IsDanger
  | Green -> IsSuccess
  | Blue -> IsLink
  | Black -> IsDark
let formatUser {Name=n;Color=c} = Message.message [Message.Size IsSmall;Message.Color (ColorToColor c)] [Message.body [][str n]]
let colorSelector dispatch =
  [Red;Green;Blue;Black]
  |> List.map (fun c ->
    Column.column [Column.Width (Screen.All,Column.IsOneQuarter)]
      [Button.a [
        Button.Color (ColorToColor c)
        Button.OnClick (fun _ -> dispatch(SetColor c))][str " "]])
  |>
  Columns.columns [Columns.IsMobile;Columns.CustomClass "is-variable is-1";Columns.IsMultiline]
let formatMessage users = function
  | SysMsg {Time=t;Content=c}->
    Message.message [Message.Color IsWarning;Message.Size IsSmall][
      Message.body [][
        Columns.columns [Columns.IsMobile][
          Column.column[Column.Width (Screen.All,Column.Is11)][str c]
          Column.column[Column.Width (Screen.All,Column.Is1)][str (t.ToShortTimeString())]
        ]
      ]
    ]
  | ClientMsg (user,{Time=t;Content=c}) ->
    let color =
      users
      |> List.tryPick (fun {Name=n;Color = c} -> if n = user then Some (ColorToColor c) else None)
      |> Option.defaultValue IsWarning
    Message.message [Message.Color color;Message.Size IsSmall][
      Message.header[][str user]
      Message.body [][
        Columns.columns [Columns.IsMobile][
          Column.column[Column.Width (Screen.All,Column.Is11)][str c]
          Column.column[Column.Width (Screen.All,Column.Is1)][str (t.ToShortTimeString())]
        ]
      ]
    ]

open Fable.Import

let view model dispatch =
  let props size : IHTMLProp list = [ Style [CSSProp.Height size;CSSProp.MaxHeight size;CSSProp.Overflow "auto"]]
  div [Style[CSSProp.Overflow "none"]]  [
    Container.container [Container.Props [Style [CSSProp.Height "100vh";CSSProp.MaxHeight "100vh";CSSProp.Overflow "none"]]] [
        Columns.columns [Columns.IsMobile] [
          Column.column [Column.Width (Screen.All,Column.IsFourFifths);Column.Props (props "80vh")]
            (model.Messages |> List.map (formatMessage model.ConnectedUsers))
          Column.column [Column.Props (props "80vh")]
            (model.ConnectedUsers |> List.map formatUser)
        ]
        Container.container [Container.Props [Style [CSSProp.Height "10vh";CSSProp.MaxHeight "20vh";CSSProp.Position "absolute";CSSProp.Bottom "0"]]]
          (match model.Connection with
          | Connected _ ->
             [
              Media.media[][
                 Media.left [] [(colorSelector dispatch)]
                 Media.content[][
                  Input.text [
                    Input.Placeholder "Message"
                    Input.Value model.TextField
                    Input.OnChange (fun e -> dispatch (!!e.target?value |> SetTextField))
                    Input.Props [
                      OnKeyDown (fun (ev:React.KeyboardEvent) ->
                                if ev.keyCode = Fable.PowerPack.Keyboard.Codes.enter then
                                  ev.preventDefault()
                                  Bridge.Send(SendMsg model.TextField)
                                  dispatch (SetTextField ""))

                    ]]]

                 Media.right[][
                  Button.a [
                    Button.OnClick (fun _ ->
                      Bridge.Send(SendMsg model.TextField)
                      dispatch (SetTextField ""))
                    Button.Disabled (model.TextField = "") ][str "Send"]
                 ]
            ]]
          | d ->
            [
                Media.media[][
                 Media.left[][(colorSelector dispatch)]
                 Media.content[][
                     Input.text [
                      Input.Placeholder "User name"
                      Input.Disabled (d = Waiting)
                      Input.Value model.UserField
                      Input.OnChange (fun e -> dispatch (!!e.target?value |> SetUserField))
                      Input.Props [
                        OnKeyDown (fun (ev:React.KeyboardEvent) ->
                                if ev.keyCode = Fable.PowerPack.Keyboard.Codes.enter &&  d <> Waiting then
                                  ev.preventDefault()
                                  dispatch SendUser)

                      ]]]
                 Media.right[][
                  Button.a [Button.OnClick (fun _ -> dispatch SendUser);Button.IsLoading (d = Waiting) ][str "Send"]
                 ]
              ]
            ])
      ]
  ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withBridgeConfig
  (Bridge.endpoint Remote.socketPath
  |> Bridge.withMapping RC
  |> Bridge.withWhenDown ConnectionLost)
#if DEBUG
|> Program.withConsoleTrace
|> Program.withDebugger
#endif
|> Program.withReactUnoptimized "elmish-app"
#if DEBUG
|> Program.withHMR
#endif
|> Program.run