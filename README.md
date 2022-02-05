# Readme

Main document at: [Main.md](Main.md).

## Prerequisites

```sh
cabal update && cabal install markdown-unlit
cabal install --only dependencies
```

## Running as executable

```sh
cargo run worldturtle-literate
```

## Running through ghci

```sh
cargo repl Main.lhs
```
