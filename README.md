# github-tui

A TUI interface to GitHub.

> [!IMPORTANT]
> **DISCLAIMER:** `github-ui` is developed and maintained in free time
> voluntarily.  The development may continue for decades or may stop tomorrow. You
> can use
> [GitHub Sponsorship](https://github.com/sponsors/chshersh) to support
> the development of this project.

> [!WARNING]
> `github-tui` is in _alpha_ stage of development!
> Expect missing features and horrible bugs.

## Prerequisites

To use `github-tui`, you need to have the following installed:

1. OCaml toolchain: to build the project
1. `bat` version ⩾ 0.19.0
1. [Hack Mono Nerd Font](https://www.nerdfonts.com/)

## Development

Initialise the project when building for the first time:

```
opam switch create .
```

Build the project:

```
dune build
```

Run the project:

```
dune exec bin/main.exe -- owner/repo
```

Install dev dependencies:

```
opam install utop ocamlformat ocaml-lsp-server
```
