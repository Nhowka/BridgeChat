module ClientModel
open Shared

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