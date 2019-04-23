module ClientView


open Fable.React
open Fable.React.Props
open ClientModel
open Shared
open Fulma


open Fable.Core.JsInterop
importAll "../../node_modules/bulma/bulma.sass"
open Elmish.Bridge
open Browser.Types

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


let view (p:{|model:Model; dispatch:ClientMsg->unit|}) =
  let props size : IHTMLProp list = [ Style [CSSProp.Height size;CSSProp.MaxHeight size;CSSProp.Overflow "auto"]]
  div [Style[CSSProp.Overflow "none"]]  [
    Container.container [Container.Props [Style [CSSProp.Height "100vh";CSSProp.MaxHeight "100vh";CSSProp.Overflow "none"]]] [
        Columns.columns [Columns.IsMobile] [
          Column.column [Column.Width (Screen.All,Column.IsFourFifths);Column.Props (props "80vh")]
            (p.model.Messages |> List.map (formatMessage p.model.ConnectedUsers))
          Column.column [Column.Props (props "80vh")]
            (p.model.ConnectedUsers |> List.map formatUser)
        ]
        Container.container [Container.Props [Style [CSSProp.Height "10vh";CSSProp.MaxHeight "20vh";CSSProp.Position PositionOptions.Absolute;CSSProp.Bottom "0"]]]
          (match p.model.Connection with
          | Connected _ ->
             [
              Media.media[][
                 Media.left [] [(colorSelector p.dispatch)]
                 Media.content[][
                  Input.text [
                    Input.Placeholder "Message"
                    Input.Value p.model.TextField
                    Input.OnChange (fun e -> p.dispatch (!!e.target?value |> SetTextField))
                    Input.Props [
                      OnKeyDown (fun (ev:KeyboardEvent) ->
                                if ev.keyCode = 13. then
                                  ev.preventDefault()
                                  Bridge.Send(SendMsg p.model.TextField)
                                  p.dispatch (SetTextField ""))

                    ]]]

                 Media.right[][
                  Button.a [
                    Button.OnClick (fun _ ->
                      Bridge.Send(SendMsg p.model.TextField)
                      p.dispatch (SetTextField ""))
                    Button.Disabled (p.model.TextField = "") ][str "Send"]
                 ]
            ]]
          | d ->
            [
                Media.media[][
                 Media.left[][(colorSelector p.dispatch)]
                 Media.content[][
                     Input.text [
                      Input.Placeholder "User name"
                      Input.Disabled (d = Waiting)
                      Input.Value p.model.UserField
                      Input.OnChange (fun e -> p.dispatch (!!e.target?value |> SetUserField))
                      Input.Props [
                        OnKeyDown (fun (ev:KeyboardEvent) ->
                                if ev.keyCode = 13. &&  d <> Waiting then
                                  ev.preventDefault()
                                  p.dispatch SendUser)

                      ]]]
                 Media.right[][
                  Button.a [Button.OnClick (fun _ -> p.dispatch SendUser);Button.IsLoading (d = Waiting) ][str "Send"]
                 ]
              ]
            ])
      ]
  ]
