(** TypeScript / JavaScript API extractor (spec FR-120): every [.d.ts] (outside the
    skipped directories), plus the files reachable from [package.json]'s [types],
    [typings], [exports], [module] and [main] entries (with the [src/*.ts] a [dist/] entry
    is usually built from); without any of them, a root or [src/] [index] file. Items are
    the exported declarations
    ([export function|class|interface|type|const| enum|namespace], and the ambient
    top-level declarations of a [.d.ts]) with the JSDoc/TSDoc block written directly
    above. Qualified paths are the file's module path without [src/], [lib/] and [index],
    dot-separated, then the name. *)

val extractor : Api_item.extractor
(** The TypeScript/JavaScript extractor. *)
