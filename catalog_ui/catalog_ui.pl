:- module(catalog_ui, [], [assertions, dcg, fsyntax, actmod, wui(wui)]).

:- doc(title, "Catalog of Bundles (UI)").
:- doc(author, "Jose F. Morales").

:- doc(summary, "Web-based UI for the Ciao Bundle Catalog").

:- doc(module, "The bundle catalog implements a web-based UI to
   inspect the available bundles in this Ciao installation. See the
   @tt{README.md} file for build and usage details.").

:- include(ciao_website(website_config_auto)).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(bundle/bundle_info)).

:- use_module(.(render_lpdoc)).
:- use_module(catalog_ui(bundle_extra_info), [
    catalog_bundles/2,
    catalog_bundle_info/2
]).

% ---------------------------------------------------------------------------
% Options

% TODO: show external dependencies
% TODO: show all bundles in catalog (even not activated or installed)

% show_revdeps :- fail.
show_revdeps.

% show_local_tools :- fail.
show_local_tools.

% ---------------------------------------------------------------------------

:- include(library(http/http_server_hooks)).

% ---------------------------------------------------------------------------

%wui_service
:- dist_node. % TODO: needed here?

:- suspendable(setup).
setup :-
    set_title('Ciao Bundles'),
    set_layout(lpdoc_layout(singletab(catalog_ui))).

% ---------------------------------------------------------------------------
% Suspendable state

:- transient(navtab/1).
:- transient(catalog_focus/1).

:- suspendable('__init__').
'__init__' :-
    t_set_fact(navtab(catalog_ui)),
    t_set_fact(catalog_focus(all_bundles)).

% ---------------------------------------------------------------------------

% Select a bundle in code browser (used for initial queries)
:- suspendable(open_catalog(term)).
open_catalog(SrcFocus) :-
    t_set_fact(navtab(catalog_ui)),
    set_catalog_focus(SrcFocus).

% Select focus (on a bundle or file)
:- suspendable(catalog_at(term)).
catalog_at(SrcFocus) :-
    set_catalog_focus(SrcFocus).

set_catalog_focus(SrcFocus) :-
    t_set_fact(catalog_focus(SrcFocus)).

% ---------------------------------------------------------------------------

% TODO: add actions for 'Build' (which include: build and build ancestors)

:- impl(navtab, catalog_ui).

(catalog_ui as navtab).title(['Ciao Bundles']).
(catalog_ui as navtab).render(R) :-
    t_current_fact(catalog_focus(SrcFocus)),
    catalog_render(SrcFocus, R, []).

catalog_render(SrcFocus) -->
    local_tool_refs(SrcFocus),
    ( { SrcFocus = all_bundles } ->
        all_bundles_html
    ; { SrcFocus = bundle(Bundle) } ->
        wui_elem(link(~wui_redraw(~fiberSusp(catalog_at(all_bundles))), [], ['[All bundles]'])), [\\],
        bundle_card(Bundle),
        bundle_desc_html(Bundle)
    ; { fail }
    ).

% ---------------------------------------------------------------------------
% Listing of all bundles

all_bundles_html -->
    [begin(div, [])],
    % [h2([image('/images/ciao-logo.png'), ' Bundles'])],
    %
    % Image with half width (for retina display)
    %[env(div, [style='float: right; margin-right: 20px'], [image('/images/bundle_catalog_logo.gif', [width='160px'])])],
    %
    [h2(['The ', env(a, [href='/'], [image('/images/ciao-logo.png', [height='40px'])]), ' Bundle Catalog'])],
    ['This is the official collection of available bundles for Ciao.', \\,
     'The links below provide detailed information for each bundle, their dependencies, ',
     'source repositories, and manuals.',
     %'(description, manuals, repositories, dependencies, ',
     %'reverse dependencies, build status, and runnable and downloadable artifacts).',
     \\],
    [end(div)],
    [h3('Core bundles')],
    bundle_list(~findall(M, catalog_bundles([min], M))),
    [h3('Standard bundles')],
    bundle_list(~findall(M, catalog_bundles([main], M))),
    [h3('Other bundles')],
    bundle_list(~findall(M, catalog_bundles([other], M))).

bundle_list(Ms) -->
    %[env(table, [class='table table-condensed', style='font-size: 14px'], Rows)],
    [env(table, [class='lpdoc-table'], Rows)],
    { header_and_items(Ms, Rows, []) }.

header_and_items(Ms) -->
    [env(tr, [], [env(th, [], ['Name']),
                  env(th, [], ['Description'])
                  /*, env(th, [], [])*/])],
    bundle_items(Ms).

bundle_items([]) --> [].
bundle_items([M|Ms]) --> bundle_item(M), bundle_items(Ms).

% Short bundle item
bundle_item(M) -->
    [env(tr, [], [env(td, [], C1), env(td, [], C2)/*, env(td, [], C3)*/])],
    { atom_codes(M, Str) },
    { wui_elem(link(~wui_redraw(~fiberSusp(catalog_at(bundle(M)))), [], [Str]), C1, []) },
    { bundle_desc(M, C2, []) }.
    % { bundle_tags(M, C3, []) }.

% Bundle title (if available)
bundle_desc(M) -->
    ( { catalog_bundle_info(M, title(Title)) } -> [Title] ; [] ).

% Bundle tags
bundle_tags(M) -->
    ( { has_manual_entries(M) } -> [' ', ~glyphicon_env('book')] ; [] ),
    ( { has_play_entries(M) } -> [' ', ~glyphicon_env('play-circle')] ; [] ),
    ( { has_build_status(M) } -> [' ', ~glyphicon_env('cog')] ; [] ).

% ---------------------------------------------------------------------------
% Description of a bundle (main)

bundle_card(Bundle) -->
    { atom_codes(Bundle, BundleStr) },
    [h2(BundleStr)],
    [begin(div, [class='list-group'])],
    %
    [begin(div, [class='list-group-item'])],
    % Links to (http) services provided by the bundle
    ( { bundle_version(Bundle, Version) } ->
            ['Version:', ' ', Version, \\]
    ; []
    ),
    gen_play_refs(Bundle),
    gen_manual_refs(Bundle),
    gen_source_refs(Bundle),
    [end(div)],
    %
    ( { has_deps(Bundle) } ->
        [begin(div, [class='list-group-item'])],
        gen_deps_refs(Bundle),
        ( { show_revdeps } -> gen_revdeps_refs(Bundle) ; [] ),
        [end(div)]
    ; []
    ),
    %
    ( { has_build_status(Bundle) } ->
        [begin(div, [class='list-group-item list-group-item-warning'])],
        gen_build_status(Bundle),
        [end(div)]
    ; []
    ),
    [end(div)].

bundle_desc_html(Bundle) -->
    ( { catalog_bundle_info(Bundle, desc(Desc)) } ->
        % [~render_docfile(R)]
        [~render_docstring(Desc)]
    ; ['(This bundle contains no description)']
    ).

% ---------------------------------------------------------------------------
% Build status/archive for a bundle

% TODO: hardwired

has_build_status(core).
has_build_status(builder).

gen_build_status(_Bundle) -->
    [~glyphicon_env('cog'), b(' Automatic builds'), \\],
    [env(a, [href='https://github.com/ciao-lang/ciao/actions'], ['Builds'])], [' | '],
    [env(a, [href='https://github.com/ciao-lang/ciao/releases'], ['Releases'])].

% ---------------------------------------------------------------------------
% Bundle dependencies

has_deps(Bundle) :-
    ( catalog_bundle_info(Bundle, deps(_))
    ; show_revdeps, catalog_bundle_info(Bundle, revdeps(_))
    ),
    !.

gen_deps_refs(Bundle) -->
    { findall(R, catalog_bundle_info(Bundle, deps(R)), Deps) },
    ( { Deps = [] } -> []
    ; ['Dependencies:'],
      bundle_links(Deps), [\\]
    ).

gen_revdeps_refs(Bundle) -->
    { findall(R, catalog_bundle_info(Bundle, revdeps(R)), RevDeps) },
    ( { RevDeps = [] } -> []
    ; ['Used by:'],
      bundle_links(RevDeps), [\\]
    ).

bundle_links([]) --> [].
bundle_links([Name|Deps]) -->
    { atom_codes(Name, Str) },
    [' '], wui_elem(link(~wui_redraw(~fiberSusp(catalog_at(bundle(Name)))), [], [Str])),
    bundle_links(Deps).

% ---------------------------------------------------------------------------
% Bundle manuals

:- use_module(library(pathnames), [path_concat/3]).

has_manual_entries(Bundle) :- catalog_bundle_info(Bundle, manuals(_, _)), !.

gen_manual_refs(Bundle) -->
    { findall(manual(Name, HRef), bundle_manual_link(Bundle, Name, HRef), DocRefs) },
    ( { DocRefs = [] } -> []
    ; [~glyphicon_env('book'), ' Manuals:'],
      manual_links(DocRefs), [\\]
    ).

manual_links([]) --> [].
manual_links([manual(Name,HRef)|HRefs]) -->
    { atom_codes(Name, Str) },
    [' ', env(a, [href=HRef], [Str])],
    manual_links(HRefs).

bundle_manual_link(Bundle, Name, HRef) :-
    catalog_bundle_info(Bundle, manuals(Name, HRef0)),
    % Add URL prefix for docs
    path_concat('/ciao/build/doc', HRef0, HRef1),
    atom_concat(HRef1, '/', HRef). % TODO: Trick! otherwise local .css is not found! fixme?

% ---------------------------------------------------------------------------
% Source links (repositories)

gen_source_refs(Bundle) -->
    [~glyphicon_env('link'), ' Source:'],
    { catalog_bundle_info(Bundle, url(HRef)) },
    [' '], [env(a, [href=HRef], ['repository'])], [\\].

% ---------------------------------------------------------------------------
% "Play" (online service) entries of a bundle

:- use_module(library(service/service_registry), [bundle_http_entry/3]).

% TODO: for localhost views: add (somewhere) the 'stop' button to stop running instances of a bundle (ciao-serve stop)?

has_play_entries(Bundle) :- bundle_http_entry(Bundle, _, _), !.

% Online entries for the bundle
gen_play_refs(Bundle) -->
    { findall((Name,HRef), bundle_http_entry(Bundle, Name, HRef), HRefs) },
    ( { HRefs = [] } -> []
    ; [~glyphicon_env('play-circle'), ' Run:'],
      play_refs(HRefs), [\\]
    ).

play_refs([]) --> [].
play_refs([(Name,HRef)|HRefs]) -->
    { atom_codes(Name, Str) },
    [' ', env(a, [href=HRef], [Str])],
    play_refs(HRefs).

% ---------------------------------------------------------------------------
% Links to 'ciaoviz' (only if its bundle is available)

:- if(defined(has_ciaoviz)).

:- use_module(ciaoviz(ciaoviz_ui), _, [active]).

local_tool_refs(SrcFocus) -->
    ( { show_local_tools } ->
        { ciaoviz_button(SrcFocus, Xs, []) },
        [env(div, [style='float: right'], Xs)]
    ; []
    ).

ciaoviz_button(SrcFocus) -->
    wui_elem(link(~wui_redraw(~fiberSuspSpawn(open_ciaoviz(SrcFocus))), [], [big(~glyphicon_env('eye-open'))])).

:- else.

local_tool_refs(_Bundle) --> [].

:- endif.
