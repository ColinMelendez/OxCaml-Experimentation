# Bonsai Term Components

This repo contains some bonsai term components that you can use.

You can read more about how to install `bonsai_term` in [here](https://github.com/janestreet/bonsai_term).

You can install this library with (after installing `bonsai_term`) with:

```
opam install bonsai_term_components
```
To use them you can then add them to your dune file (e.g. adding
`bonsai_term_components.ncdu` to your dune file's "libraries" field.)

> NOTE: You may need to follow the instructions in `bonsai_term`'s readme. This library is
> currently only accessible in `OxCaml`.

## Color schemes

The `bonsai_term_color_scheme` library provides app-wide themes for Bonsai Term apps.
Apps can set a theme explicitly with `Bonsai_term_color_scheme.set_flavor_within` or
`Bonsai_term_color_scheme.set_flavor_within_app`.

If an app uses `bonsai_term_color_scheme` but does not set an app-wide flavor, users can
choose the default theme with the `BONSAI_TERM_COLOR_SCHEME` environment variable:

```bash
BONSAI_TERM_COLOR_SCHEME=Latte my-bonsai-term-app
```

Valid values include `Mocha`, `Macchiato`, `Frappe`, `Latte`, `Vscode_dark`,
`Vscode_light`, `Gruvbox_dark`, `Gruvbox_light`, `Dracula`, `Kanagawa`,
`Tokyo_night_dark`, `Tokyo_night_light`, `Monokai`, `Bluloco`, `Solarized_dark`,
`Solarized_light`, `Terminal_16`, and `Terminal_16_inverted`. Explicit app-wide themes
take precedence over the environment variable.
