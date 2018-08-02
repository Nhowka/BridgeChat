namespace Shared

type Color =
    | Red
    | Green
    | Blue
    | Black

type User = {Name : string; Color: Color}
type Message = {Time : System.DateTime; Content: string}

type Msgs =
  | ClientMsg of (string*Message)
  | SysMsg of Message

type RemoteClientMsg =
    | QueryConnected
    | GetUsers of User list
    | NameStatus of bool
    | AddUser of User
    | RemoveUser of string
    | AddMsg of Msgs
    | AddMsgs of Msgs list
    | ColorChange of (string*Color)

type RemoteServerMsg = SetUser of User | ChangeColor of Color | SendMsg of string | UsersConnected


module Remote =
    let socketPath = "/socket"