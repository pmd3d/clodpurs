module UniqueIds

type Counter = int

let makeTemporary (counter: Counter) : Counter * string =
    (counter + 1, "tmp." + string counter)

let makeLabel (prefix: string) (counter: Counter) : Counter * string =
    (counter + 1, prefix + "." + string counter)

let makeNamedTemporary = makeLabel

let initialCounter : Counter = 0
