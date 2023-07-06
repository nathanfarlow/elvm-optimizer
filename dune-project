(lang dune 3.7)

(name elvm)

(generate_opam_files true)

(source
 (github nathanfarlow/elvm-optimizer))

(authors "Nathan Farlow")

(maintainers "Nathan Farlow")

(license MIT)

(documentation https://github.com/nathanfarlow/elvm-optimizer)

(package
 (name elvm)
 (synopsis "Optimize elvm IR")
 (depends ocaml dune core))
