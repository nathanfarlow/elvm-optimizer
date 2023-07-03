(lang dune 3.7)

(name elvm_opt)

(generate_opam_files true)

(source
 (github nathanfarlow/elvm-opt))

(authors "Nathan Farlow")

(maintainers "Nathan Farlow")

(license MIT)

(documentation https://github.com/nathanfarlow/elvm-opt)

(package
 (name elvm_opt)
 (synopsis "Optimize elvm IR")
 (depends ocaml dune core))
