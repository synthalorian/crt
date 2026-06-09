(lang dune 3.14)
(using menhir 3.0)
(name crt)
(generate_opam_files true)
(license MIT)
(authors "synth")

(package
 (name crt)
 (synopsis "Static site generator as a functional signal pipeline")
 (depends
   (ocaml (>= 5.2))
  dune
  menhir
  omd
  tyxml
  ))
