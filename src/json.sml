structure JsonUtil =
struct
  datatype Json =
    OBJECT of Json list
  | LIST of Json list
  | MEMBER of string * Json
  | STR of string

  fun jsonToString (j: Json) : string =
    case j of
      OBJECT l => "{"^(String.concatWith "," (map jsonToString l))^"}"
    | LIST l => "["^(String.concatWith "," (map jsonToString l))^"]"
    | MEMBER (s, j) => "\""^s^"\":"^(jsonToString j)
    | STR s => "\""^s^"\""
end
