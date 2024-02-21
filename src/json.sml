structure JsonUtil =
struct
  datatype Element =
    LIST of Element list
  | STR of string
  | NUM of int
  | OBJ of (string * Element) list

  type Member = string * Element
  type Json = (string * Element) list

  fun elemToString (e: Element): string =
    case e of
      OBJ j => jsonToString j
    | LIST l => "["^(String.concatWith "," (map elemToString l))^"]"
    | NUM i => Int.toString i
    | STR s => "\""^s^"\""

  and jsonToString (j: Json) : string =
    "{"^(String.concatWith ","
        (map (fn (str, elem) => "\""^str^"\":"^(elemToString elem)) j)
    )^"}"
end
