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

## Generation

Use the following steps to generate an up-to-date website (fetch
externals and update):

```
ciao custom_run . fetch_externals
ciao custom_run . dist
```

This will prepare the website files under `CIAOROOT/build/site/`
directory.

To update the `cached_catalog.pl` file, execute
`bundle_extra_info:gen_catalog`.

## Serving the website

Use the `ciao-serve` command to start a simple HTTP server at
`http://localhost:8000` with support for dynamic content (like the
download component). It can be customized to listen from another
address and port or be combined with other HTTP servers a reverse
proxy (see Deployment instructions later).

Data from the dynamic components of the website will be locally stored
at the `CIAOROOT/build/data/` directory.

