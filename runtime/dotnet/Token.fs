module Token
type TokenInterface =
   abstract member type_: string
   abstract member value: string
   abstract member ToString: unit -> string

type DefaultToken(t, v) =

    interface TokenInterface with
        member val type_ = t
        member val value = v
        member this.ToString() = sprintf "[%s:%s]" (this :> TokenInterface).type_ (this :> TokenInterface).value
