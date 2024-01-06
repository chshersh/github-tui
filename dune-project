(lang dune 3.12)

(name github_tui)

(generate_opam_files true)

(source
 (github chshersh/github-tui))

(authors "Dmitrii Kovanikov")
(maintainers "Dmitrii Kovanikov")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name github_tui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   dune
   ezcurl
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
