:- module(_, [], [assertions, fsyntax, dcg, datafacts]).

% TODO: This is the experimental wasm version (work in progress)
:- doc(title, "Catalog of Bundles (UI) (wasm version)").
:- doc(author, "Jose F. Morales").

:- doc(summary, "Web-based UI for the Ciao Bundle Catalog").

:- doc(module, "The bundle catalog implements a web-based UI to
   inspect the available bundles in this Ciao installation. See the
   @tt{README.md} file for build and usage details.").

% ===========================================================================

:- use_module(library(stream_utils), [write_string/1]).
:- use_module(library(pillow/json), [json_to_string/2]).
:- use_module(library(pillow/html), [html2terms/2]).

:- use_module(library(streams)).

:- use_module(library(lists)).

:- data curr_id/1.

:- export(render/2).
render(Id, State) :-
    set_fact(curr_id(Id)),
    %
    render_html(State, H, []),
    display('$$$js_eval$$$:'), display('{'),
    display('window.dynpreview_render('), display(Id), display(','), write_qhtml(H), display('); '),
    display('}'), nl.

write_qhtml(Html) :-
    html2terms(Str, Html), % from html term to str
    json_to_string(string(Str), Str2), % quote the string
    write_string(Str2).

dynpreview_visit(M) := Str :-
    curr_id(Id),
    number_codes(Id, IdCs),
    atom_codes(M, Cs),
    Str = "window.dynpreview_visit("||(~append(IdCs,",\""||(~append(Cs, "\")")))).

% % (arbitrary queries)
% pg_query(G) := Str :-
%     % TODO: missing quote of G str
%     Str = "window.pg_query(\""||(~append(~term_to_str(G), "\")")).
% 
% % TODO: format:format_to_string/3 implementation is not complete
% :- use_module(library(format_to_string), [format_to_string/3]).
% term_to_str(X, Str) :-
%     format_to_string("~q", [X], Str).

% ===========================================================================

:- use_module(catalog_ui(render_lpdoc)).

:- use_module(library(aggregates)).
:- use_module(catalog_ui(bundle_extra_info), [
    catalog_bundles/2,
    catalog_bundle_info/2
]).

render_html('') --> !,
    all_bundles_html.
render_html(Bundle) -->
    bundle_card(Bundle),
    bundle_desc_html(Bundle).

% ---------------------------------------------------------------------------
% Listing of all bundles

all_bundles_html -->
    [begin(div, [])],
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
    { bundle_link(M, C1) },
    { bundle_desc(M, C2, []) }.

% Bundle title (if available)
bundle_desc(M) -->
    ( { catalog_bundle_info(M, title(Title)) } -> [Title] ; [] ).

bundle_link(M) := env(a, [href="javascript:void(0)", onclick=JS], [M]) :-
    JS = ~dynpreview_visit(M).

% ---------------------------------------------------------------------------
% Build status/archive for a bundle

% TODO: hardwired

has_build_status(core).
has_build_status(builder).

gen_build_status(_Bundle) -->
    [b('Automatic builds'), \\],
    [env(a, [href='https://github.com/ciao-lang/ciao/actions'], ['Builds'])], [' | '],
    [env(a, [href='https://github.com/ciao-lang/ciao/releases'], ['Releases'])].

% ---------------------------------------------------------------------------
% Bundle dependencies

has_deps(Bundle) :-
    catalog_bundle_info(Bundle, deps(_)),
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
    { L = ~bundle_link(Name) },
    [' '], [L],
    bundle_links(Deps).

% ---------------------------------------------------------------------------
% Description of a bundle (main)

bundle_version(_Bundle, _Version) :- fail. % TODO: add in info

bundle_card(Bundle) -->
    { JS = ~dynpreview_visit('') },
    ['[&nbsp;', env(a, [href="javascript:void(0)", onclick=JS], ['bundles']), '&nbsp;&raquo;&nbsp;'],
    [Bundle, '&nbsp;]', \\],
    [\\],
    %
    [begin(div, [class='list-group'])],
    %
    [begin(div, [class='list-group-item'])],
    % Links to (http) services provided by the bundle
    ( { bundle_version(Bundle, Version) } ->
            ['Version:', ' ', Version, \\]
    ; []
    ),
    % gen_play_refs(Bundle),
    gen_manual_refs(Bundle),
    gen_source_refs(Bundle),
    [end(div)],
    %
    ( { has_deps(Bundle) } ->
        [begin(div, [class='list-group-item'])],
        gen_deps_refs(Bundle),
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
% Bundle manuals

:- use_module(library(pathnames), [path_concat/3]).

has_manual_entries(Bundle) :- catalog_bundle_info(Bundle, manuals(_, _)), !.

gen_manual_refs(Bundle) -->
    { findall(manual(Name, HRef), bundle_manual_link(Bundle, Name, HRef), DocRefs) },
    ( { DocRefs = [] } -> []
    ; ['Manuals:'],
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
    ['Source:'],
    { catalog_bundle_info(Bundle, url(HRef)) },
    [' '], [env(a, [href=HRef], ['repository'])], [\\].

