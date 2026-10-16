(** Python API extractor (spec FR-120): the [.py] modules outside tests, [docs/], the
    skipped directories and private ([_x.py]) modules, a [.pyi] stub replacing its module.
    Items are the top-level [def] / [async def] / [class] (with a multi-line header
    joined, and the docstring opening the body), restricted to [__all__] when the module
    declares it (whose plain assignments become [const] items), else to names without a
    leading underscore. Qualified paths are the module's dotted path ([src/] and
    [__init__] dropped), then the name. *)

val extractor : Api_item.extractor
(** The Python extractor. *)
