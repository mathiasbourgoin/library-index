(** Quint specifications, [.qnt] (spec FR-120, extras batch).

    Selection: every [.qnt] file outside {!Api_item.skipped} paths. Detection: such a file
    exists, or the primary language is Quint.

    Items: each [module] (kind [module]) and the declarations directly inside a module
    body: [def], [val], [action], [temporal], [type], [const], [var], [run] (a [pure] or
    [nondet] qualifier is part of the signature, not of the kind), qualified as
    [Module.name] (nested modules as [Outer.Inner.name]). The doc is the comment ([//]
    run, or [/* */] block) directly above. *)

val extractor : Api_item.extractor
(** The [quint] extractor. *)
