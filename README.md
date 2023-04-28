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
dependencies, build `ciaowasm` and `ciao_playground` bundles, and
prepare the website files under `CIAOROOT/build/site/` directory.

You can build and install the website with:
```
ciao custom_run website fetch_externals
ciao build --bin website
ciao build --grade=wasm website
ciao install --grade=wasm website
ciao custom_run website dist
```

NOTE: The metadata for the bundle catalog is pre-generated in the
`cached_catalog.pl` file. Use `bundle_extra_info:gen_catalog` to
generate it.

## Serving the website

Use an HTTP server with multi-threading capabilities with root at
`CIAOROOT/build/site/`. See the `README.md` at the `ciao_playground`
for more details.
