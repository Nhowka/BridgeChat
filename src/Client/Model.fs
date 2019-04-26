module ClientModel
open Shared

type ClientMsg =
    | RC of RemoteClientMsg
    | SendUser of string * Color
    | ConnectionLost
    | ToggleMode
    | SetColor of Color

type Connection =
  | Disconnected
  | Waiting
  | Connected of User

type FieldMode =
  | User
  | Message

type Model = {
    Mode : FieldMode
    Connection : Connection
    ConnectedUsers : User list
    Messages : Msgs list
}