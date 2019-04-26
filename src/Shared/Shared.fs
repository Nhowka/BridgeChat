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
    | NameStatus of User option
    | AddUser of User
    | RemoveUser of string
    | AddMsg of Msgs
    | AddMsgs of Msgs list
    | ColorChange of string * Color
    | NameChange of string * string

type RemoteServerMsg = SetUser of User | SendMsg of string | UsersConnected


module Remote =
    let socketPath = "/socket"