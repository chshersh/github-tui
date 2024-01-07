(lang dune 3.12)

(name github_tui)

(generate_opam_files true)

(source
 (github chshersh/github-tui))

(authors "Dmitrii Kovanikov <chshersh@gmail.com>")
(maintainers "Dmitrii Kovanikov <chshersh@gmail.com>")

(license MPL-2.0)

(package
 (name github_tui)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends 
   ocaml
   dune
   ezcurl
   minttea
   terminal
 )
 (tags
  (tui cli git github)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
