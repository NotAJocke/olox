exception ParseError of Token.t * string
exception RuntimeError of Token.t * string
exception RuntimeReturn of Types.literal

let report line where message =
  Printf.eprintf "[line %d] Error %s: %s\n%!" line where message

let error line message = report line "" message
