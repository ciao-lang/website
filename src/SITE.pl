:- module(_, [], [doccfg]).

%! \title Ciao website configuration
%
%  This is LPdoc configuration to generate the Ciao website.

% --------------------------------------------------------------------------
%
% TODO:
%  - documentation.md:
%    - manuals listed by hand! Not synchronized! Missing .pdf version
%    - add playgrounds and list of bundles
%
% --------------------------------------------------------------------------

:- use_module(library(bundle/bundle_paths), [bundle_path/3]).

filepath := at_bundle(core, '..'). % (for INSTALLATION.md)
filepath := ~bundle_path(website, 'src').
%tmplpath := ~bundle_path(website, 'tmpl').

doc_structure := 
    'index.md'-[
      'install.md',
      'documentation.md',
      'project.md',
      phony_link('playground.md', url('/playground')),
      % phony_link('bundles.md', url('/catalog_ui')), % (server-side dynamic version, use empty bundles.md)
      'bundles.md'
    ].

% No indices
index := _ :- fail. % concept|lib|pred|prop|regtype|decl|author|global.

doc_mainopts := no_patches|no_biblio|no_math.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_authors|no_biblio|no_math.

docformat := html. % we are a webpage!

% TODO: port this manual
allow_markdown := yes.
syntax_highlight := yes.
allow_runnable := yes.

% -----------------------------------------------------------------------------

% TODO: generalize? document
html_layout := website_layout([
  % vertical_navmenu,
  % icon('ciao-icon.ico'), % (use default favicon.ico)
  css('css/normalize.css'),
  %css('node_modules/source-sans-pro/source-sans-pro.css'), % Font
  %css('node_modules/source-code-pro/source-code-pro.css'), % Font
  css('css/website.css')
]).

% The website skeleton that is copied to the output directory
% (contains the CSS files, images, etc.)
html_asset := ~bundle_path(website, 'skel').

% -----------------------------------------------------------------------------

:- use_module(ciaobld(config_common), [site_root_dir/1]).

% Target URL
htmlurl := '/'.

