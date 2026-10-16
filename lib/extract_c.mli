(** C/C++ public headers (spec FR-120, extras batch).

    Selection: headers ([.h], [.hh], [.hpp], [.hxx], [.h++]) at the repository root or
    under an [include] directory, outside {!Api_item.skipped} paths. Detection: such a
    header exists, or the primary language is C, C++, Objective-C or CUDA.

    Items are the top-level declarations (inside [extern "C"] and [namespace] blocks,
    whose names prefix C++ items as [ns::name]): function prototypes and inline
    definitions ([function]), [struct] / [union] / [enum] / [class] ([class] for C++),
    [typedef] (including function-pointer typedefs) and [#define] macros ([macro]; include
    guards excepted). The doc is the [/** */] or [///] comment directly above. Members of
    a struct or class body are not items. *)

val extractor : Api_item.extractor
(** The [c] extractor. *)
