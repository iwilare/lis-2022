# Compiling and testing
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