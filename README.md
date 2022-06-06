# Compiling and testing

### Dependencies

OCaml https://ocaml.org/docs/install.html

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
## Esy

- Install esy
    ```bash
    npm install -g esy
    ```
- Build sandbox and project
    ```bash
    esy
    ```
- Run tests in sandbox
    ```bash
    esy test
    ```
