(lang dune 3.14)

(name pathlib)

(generate_opam_files true)

(source
 (github atomicptr/pathlib-ml))

(authors "Christopher Kaster")

(maintainers "Christopher Kaster")

(license MIT)

(documentation https://github.com/atomicptr/pathlib-ml)

(package
 (name pathlib)
 (synopsis
  "Experimental path library for OCaml inspired by Pythons pathlib.")
 (description
  "Experimental path library for OCaml inspired by Pythons pathlib.")
 (depends ocaml dune))
