exception ParseError of Token.t * string
exception RuntimeError of Token.t * string
exception RuntimeReturn of Types.literal
