(lang dune 3.13)

(name xmas)

(generate_opam_files false)

(source
 (github ComanderP/xmas))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name xmas)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
