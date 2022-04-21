# Compiling and testing

### Dependencies

Ocamel https://ocaml.org/docs/install.html


```bash
opam install qcheck
```


From the root of the project:
- Basic build, :
    ```bash 
    dune build
    ```
- Release build, ignores unused errors:
    ```bash 
    dune build --profile release
    ```
- Run tests:
    ```bash
    dune test
    ```
- Run executable:
    ```bash 
    dune exec lis2022
    ```
