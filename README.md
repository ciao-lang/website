# The Ciao Website

This bundle contains the sources for the Ciao website. The file
structure is as follows:

```
Manifest/           Manifest for this bundle
                    (includes some building code)
src/                Website contents in LPdoc format.
skel/               Additional website files
                    (images, CSS files, etc.).
```

The original sources (including alternative versions) of the logos and
images in this repository are stored separately in the `ciao-artwork`
repository.

## Installation

This bundle requires a few additional steps to fetch external
dependencies and prepare the website files under
`CIAOROOT/build/site/` directory.

First make sure that the `wui` bundle is built and installed:
```
ciao custom_run wui fetch_externals
ciao build --bin wui
ciao custom_run wui dist
```
Then build and install the website:
```
ciao custom_run website fetch_externals
ciao build --bin website
ciao custom_run website dist
```

NOTE: The metadata for the bundle catalog is pre-generated in the
`cached_catalog.pl` file. Use `bundle_extra_info:gen_catalog` to
generate it.

## Serving the website (browser-side dynamic content)

Use any HTTP server with root at `CIAOROOT/build/site/`. This requires
additional steps to build `ciaowasm` and `ciao_playground` bundles.

```
ciao build --grade=wasm website
ciao install --grade=wasm website
```

## Serving the website (server-side dynamic content)

Use the `ciao-serve` command to start a simple HTTP server at
`http://localhost:8000` with support for dynamic content. It can be
customized to listen from another address and port or be combined with
other HTTP servers a reverse proxy (see Deployment instructions
later).

Data from the dynamic components of the website will be locally stored
at the `CIAOROOT/build/data/` directory.

