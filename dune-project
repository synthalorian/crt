(lang dune 3.14)
(name crt)
(generate_opam_files true)
(source (github synthalorian crt))
(license MIT)
(authors "synth")

(package
 (name crt)
 (synopsis "Static site generator as a functional signal pipeline")
 (depends
  (ocaml (>= 4.14))
  dune
  menhir
  omd
  tyxml))
