structure Asterism = struct
  open Ceptre
  open JsonUtil

  fun headerTypes (header: tp_header_annotes): Json list =
    List.mapPartial
      (fn ((id, cl), annote) =>
        case cl of
          Type => SOME (case annote of
            SOME a => OBJECT [MEMBER ("name", STR id), MEMBER ("annote", STR a)]
          | NONE => OBJECT [MEMBER ("name", STR id)])
        | _ => NONE
      )
      header

  fun headerPreds (header: tp_header_annotes): Json list =
    List.mapPartial
      (fn ((id, cl), annote) =>
        case cl of
          Pred (pclass, terms) =>
          let
            val json_t = map (fn t => STR (termToString t)) terms
          in
            case annote of
              SOME s =>
                SOME (OBJECT [MEMBER ("name", STR id),
                              MEMBER ("terms", LIST json_t),
                              MEMBER ("annote", STR s)])
            | NONE => SOME (OBJECT [MEMBER ("name", STR id),
                                    MEMBER ("terms", LIST json_t)])
          end
        | _ => NONE)
      header

  fun headerToJson (header: tp_header_annotes): Json =
    MEMBER ("header",
      OBJECT [MEMBER ("types", LIST (headerTypes header)),
          MEMBER ("preds", LIST (headerPreds header))])

  fun builtinToJson (builtins: (ident * builtin) list): Json =
    MEMBER ("builtins",
      LIST (map
        (fn (i, b) => OBJECT [MEMBER ("name", STR i),
                              MEMBER ("annote", STR (builtinToString b))])
        builtins))

  fun progToJson
    (sigma as {header, builtin, rules}: sigma)
    (program as {stages, links, init_stage, init_state}: program)
    : Json =
    OBJECT [
      builtinToJson builtin,
      headerToJson header,
      MEMBER ("bwd_rules", LIST (map (fn {name, ...} => STR name) rules))]
      (* "\nbwd rules" :: (map
        (fn (bwd_rule as { name, ... }) => name)
        rules)
      @ ["\n", programToString program] *)

  (* only dealing w/ single-state ceptre programs rn *)
  fun run (sigma: sigma) (program: program) (outfile: string) =
    let
      val joutfile = TextIO.openOut outfile
      val () = print "\ngenerating JSON file.\n"
      val json = jsonToString (progToJson sigma program)
      val () = TextIO.output(joutfile, json^"\n")
      val () = TextIO.flushOut joutfile
    in
      TextIO.closeOut joutfile
    end
end
