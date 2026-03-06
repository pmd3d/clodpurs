module StringUtil
    let drop n (s: string) = s.Substring(n)

    let chopSuffix (n: int) (s: string) = s.Substring(0, s.Length - n)

    let chopSuffix1 (s: string) = chopSuffix 1 s

    let ofList (l: char list) = l |> List.toArray |> System.String

    let isAlnum (c: char) =
        "abcdefghijklmnopqrstuvwxzyABCDEFGHIJKLMNOPQRSTUVWXZY0123456789".Contains(c)