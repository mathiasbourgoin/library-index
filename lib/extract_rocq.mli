(** Rocq/Coq theories, [.v] (spec FR-120, extras batch).

    Detection: a root [_CoqProject] / [_RocqProject], a [dune-project] using the [coq] or
    [rocq] extension, or a primary language of Coq/Rocq ([.v] alone also names Verilog, so
    it is not enough). Selection: every [.v] file outside {!Api_item.skipped} paths.

    Items: the sentences starting with [Definition], [Fixpoint], [CoFixpoint],
    [Inductive], [CoInductive], [Variant], [Theorem], [Lemma], [Corollary], [Proposition],
    [Fact], [Remark], [Record], [Structure], [Class], [Instance], [Axiom], [Parameter],
    [Module] or [Module Type] (attributes and [Global] / [Polymorphic] / [Program] …
    qualifiers allowed; [Local] declarations are not public and are skipped). The kind is
    the keyword in lowercase; the qualified path is the file's module name, then the open
    [Module]s ([Section]s add nothing), then the name: [Widget.Sub.parse]. The signature
    is the sentence up to [:=] or its final period. The doc is the [(** *)] comment
    directly above. *)

val extractor : Api_item.extractor
(** The [rocq] extractor. *)
