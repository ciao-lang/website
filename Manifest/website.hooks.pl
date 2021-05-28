:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title, "Bundle Hooks for Ciao website").

% ---------------------------------------------------------------------------

:- use_module(ciaobld(third_party_custom)).

% (hook)
'$builder_hook'(custom_run(fetch_externals, [])) :- !,
    true.
    %third_party_custom_install(website).

%m_bundle_foreign_dep(website, cmd, 'node', 'Node (http://nodejs.org)').
%m_bundle_foreign_dep(website, cmd, 'npm', 'NPM (http://www.npmjs.com)').
% Some web fonts
%m_bundle_foreign_dep(website, npm, 'source-sans-pro', 'https://github.com/adobe-fonts/source-sans-pro').
%m_bundle_foreign_dep(website, npm, 'source-code-pro', 'https://github.com/adobe-fonts/source-code-pro').

% ---------------------------------------------------------------------------

:- use_module(ciaobld(lpdoc_aux), [invoke_lpdoc/1]).
:- use_module(ciaobld(config_common), [site_root_dir/1]).
:- use_module(library(system), [working_directory/2]).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(ciaobld(site_aux)).

% (hook)
'$builder_hook'(custom_run(dist, [])) :- !,
    site_build.

site_build :-
    gen_static_html,
    site_link_npm_node_modules,
    site_link_builddoc.

sitedef := ~bundle_path(website, 'src/SITE.pl').

% Prepare static HTML (with LPdoc)
gen_static_html :-
    Settings = ~sitedef,
    Dist = ~site_root_dir,
    mkpath(Dist),
    working_directory(Old, Dist),
    % Do not do a realclean or you will lose any stored documentation 
    OutDirOpt = ~atom_concat('--output_dir=', Dist),
    invoke_lpdoc([OutDirOpt, '--realclean', Settings]), % temporary?
    invoke_lpdoc([OutDirOpt, '--clean', Settings]), % temporary?
    invoke_lpdoc([OutDirOpt, '-t', 'html', Settings]),
    working_directory(_, Old).

