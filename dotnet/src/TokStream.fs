module TokStream

type TokStream = Tokens.Token Stream.Stream

let takeToken (tokens: TokStream) : Result<Tokens.Token * TokStream, string> =
    match Stream.next tokens with
    | Some (tok, rest) -> Ok (tok, rest)
    | None -> Error "Unexpected end of file"

let peek (tokens: TokStream) : Tokens.Token option =
    Stream.peek tokens

let npeek n (tokens: TokStream) = Stream.npeek n tokens

let isEmpty (tokens: TokStream) = Stream.isEmpty tokens

let ofList = Stream.ofList
