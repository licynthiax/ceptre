structure Asterism = struct
  open Ceptre
  open JsonUtil

  fun termsToJson (terms: term list): Element =
    LIST (map (fn t => STR (termToString t)) terms)

  fun headerTp (of_type: string) (header: tp_header_annotes): Member =
  let
    val filter = fn ((id, cl), _) =>
        case cl of
          Tp (args, tp) =>
          if String.compare (of_type, tp) = EQUAL then
            SOME (OBJ [("name", STR id), ("args", LIST (map (fn s => STR s) args))])
          else
            NONE
        | _ => NONE
  in
    ("tp", LIST (List.mapPartial filter header))
  end

  fun headerTypes (header: tp_header_annotes): Member =
  let
    val filter = fn ((id, cl), annote) =>
        case cl of
          Type => let
            val obj = [("name", STR id), headerTp id header]
          in
            SOME (case annote of
              SOME a => OBJ (obj @ [("annote", STR a)])
            | NONE => OBJ obj)
          end
        | _ => NONE
  in
    ("types", LIST (List.mapPartial filter header))
  end

  fun headerPreds (header: tp_header_annotes): Member =
  let
    val filter = fn ((id, cl), annote) =>
        case cl of
          Pred (pclass, terms) =>
          let
            val obj = [("name", STR id), ("terms", termsToJson terms)]
          in
            case annote of
              SOME s => SOME (OBJ (obj @ [("annote", STR s)]))
            | NONE => SOME (OBJ obj)
          end
        | _ => NONE
  in
    ("preds", LIST (List.mapPartial filter header))
  end

  fun headerToJson (header: tp_header_annotes): Member =
    ("header", OBJ [headerTypes header, headerPreds header])

  fun builtinToJson (builtins: (ident * builtin) list): Member =
    ("builtins",
      LIST (map
        (fn (i, b) => OBJ [("name", STR i), ("builtin", STR (builtinToString b))])
        builtins))

  fun atomToJson (atom as (m: mode, p: pred, t: term list)): Json =
  let
    val mode_string = case m of Pers => "pers" | Lin => "lin"
  in
    [
      ("name", STR p),
      ("mode", STR mode_string),
      ("terms", termsToJson t)
    ]
  end

  fun ruleToJson
    (rule as {name: ident, pivars: int, lhs: atom list, rhs: atom list}: rule_internal): Json =
      [
        ("name", STR name),
        ("pivars", NUM pivars),
        ("lhs", LIST (map (fn a => OBJ (atomToJson a)) lhs)),
        ("rhs", LIST (map (fn a => OBJ (atomToJson a)) rhs))
      ]

  fun stageToJson
    (s as {name: ident, nondet: nondet, body: rule_internal list}: stage): Json =
    [
      ("name", STR name),
      ("nondet", STR (nondetToString nondet)),
      ("body", LIST (map (fn r => OBJ (ruleToJson r)) body))
    ]

  fun stageruleToJson
    ({name: ident, pivars: int, pre_stage: ident, lhs: atom list, post_stage: ident, rhs: atom list}: stage_rule): Json =
    [
      ("name", STR name),
      ("pivars", NUM pivars),
      ("pre_stage", STR pre_stage),
      ("post_stage", STR post_stage),
      ("lhs", LIST (map (fn a => OBJ (atomToJson a)) lhs)),
      ("rhs", LIST (map (fn a => OBJ (atomToJson a)) rhs))
    ]

  fun progToJson
    (sigma as {header, builtin, rules}: sigma)
    (program as {stages, links, init_stage, init_state}: program)
    : Json =
    [
      builtinToJson builtin,
      headerToJson header,
      ("bwd_rules", LIST (map (fn {name, ...} => STR name) rules)),
      ("stages", LIST (map (fn s => OBJ (stageToJson s)) stages)),
      ("links", LIST (map (fn r => OBJ (stageruleToJson r)) links)),
      ("init_stage", STR init_stage),
      ("init_state", LIST (map (fn a => OBJ (atomToJson a)) init_state))
    ]

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
