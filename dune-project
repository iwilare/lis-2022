(lang dune 3.0)

(name lis2022)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "")

(maintainers "<the same as above>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name lis2022)
 (synopsis "Project for the Laboratory for Innovative Software course 2021/2022")
 (description "This project is an OCaml emulator for the Sancus microprocessor, along with property-based tests to verify correctness and security properties of the CPU implementation and its model.")
 (depends
 (ocaml (>="4.13.0"))
  dune)
 (tags (sancus cpu-emulation security-based-compilation))
 (allow_empty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
