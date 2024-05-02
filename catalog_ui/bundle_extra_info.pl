:- module(bundle_extra_info, [], [assertions, dcg, fsyntax, datafacts]).

%! \title Extra bundle info (for catalog)
%  \author Jose F. Morales

% TODO: All this information should be readable without
%   reading .hooks.pl files!

:- use_module(library(lists), [member/2, append/3]).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames), [path_split/3, path_concat/3]).
:- use_module(library(stream_utils), [file_to_string/2]).

:- use_module(engine(internals), ['$bundle_id'/1]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3, bundle_path/4]).

% ---------------------------------------------------------------------------
%! # Catalog of bundles

:- use_module(library(read)).

% (Cached)
:- data cached_catalog_bundle/1.
:- data cached_catalog_bundle_info/2.

ensure_catalog_loaded :- cached_catalog_bundle(_), !.
ensure_catalog_loaded :- load_catalog.

:- export(load_catalog/0).
load_catalog :-
    retractall_fact(cached_catalog_bundle(_)),
    retractall_fact(cached_catalog_bundle_info(_,_)),
    open(~catalog_cache_file, read, S),
    ( repeat,
      read(S, X),
      ( X = bundle(A) ->
          assertz_fact(cached_catalog_bundle(A)),
          fail
      ; X = info(A,I) ->
          assertz_fact(cached_catalog_bundle_info(A,I)),
          fail
      ; X = end_of_file -> ! % (stop)
      )
    ; true
    ),
    close(S).

% ---------------------------------------------------------------------------

:- export(catalog_bundles/1).
% catalog_bundles(?M): enumerate all bundles in the catalog
catalog_bundles(M) :-
    ensure_catalog_loaded,
    cached_catalog_bundle(M).

:- export(catalog_bundles/2).
% catalog_bundles(+Filter, ?M): enumerate bundles in the catalog
%   Filter is a list of tags (currently `main` or `other`).
catalog_bundles(Filter, M) :-
    catalog_bundles(M),
    catalog_bundle_info(M, tags(Tags)),
    % Get category from tags
    ( member(min, Tags) -> Category = min
    ; member(main, Tags) -> Category = main
    ; Category = other
    ),
    % Filter by category (if needed)
    ( member(min, Filter) -> Category = min
    ; member(main, Filter) -> Category = main
    ; member(other, Filter) -> Category = other
    ; true % any category
    ).

:- export(catalog_bundle_info/2).
:- pred catalog_bundle_info(M, Info) # "Obtain bundle info from catalog. Info can be:
  @begin{itemize}
  @item @tt{tags(Tags)}: obtain list of tags
  @item @tt{title(Title)}: the title
  @item @tt{desc(Desc)}: description text
  @item @tt{deps(Dep)}: this bundle depends on Dep
  @item @tt{revdeps(RevDep)}: RevDep depends on this bundle
  @item @tt{manuals(Manual)}: name of manuals (for URLs)
  @item @tt{url(URL)}: URL of the bundle
  @end{itemize}
".

% TODO: be careful with choicepoints
catalog_bundle_info(Bundle, Info) :-
    ensure_catalog_loaded,
    cached_catalog_bundle(Bundle), % (check or enum Bundle)
    cached_catalog_bundle_info(Bundle, Info).

% ---------------------------------------------------------------------------
%! # Generate catalog of bundles

:- use_module(engine(runtime_control)).
:- use_module(library(streams)).
:- use_module(library(write)).

catalog_cache_file := F :-
    bundle_path(website, 'catalog_ui/cached_catalog.pl', F).

:- export(gen_catalog/0).
gen_catalog :-
    open(~catalog_cache_file, write, S),
    push_prolog_flag(write_strings, on),
    write(S, '% Automatically generated, see gen_catalog/0\n'),
    ( % (failure-driven loop)
      all_bundles(A),
      is_public_bundle(A),
      writeq(S, bundle(A)), write(S, '.'), nl(S),
      gen_catalog_bundle_info(A,I),
      writeq(S, info(A,I)), write(S, '.'), nl(S),
      fail
    ; true
    ),
    pop_prolog_flag(write_strings),
    close(S).

% TODO: this should not be in the bundle registry!
all_bundles(Bundle) :- '$bundle_id'(Bundle).

gen_catalog_bundle_info(Bundle, Info) :-
    '$bundle_id'(Bundle), % (check or enum Bundle)
    gen_catalog_bundle_info_(Info, Bundle).
    
gen_catalog_bundle_info_(tags(Tags), Bundle) :-
    get_bundle_tags(Bundle, Tags).
gen_catalog_bundle_info_(title(Title), Bundle) :-
    guess_bundle_title(Bundle, Title).
gen_catalog_bundle_info_(desc(R), Bundle) :-
    guess_bundle_desc(Bundle, R).
gen_catalog_bundle_info_(deps(Dep), Bundle) :-
    bundle_deps(Bundle, Dep).
gen_catalog_bundle_info_(revdeps(RevDep), Bundle) :-
    bundle_revdeps(Bundle, RevDep).
gen_catalog_bundle_info_(manuals(Name, HRef), Bundle) :-
    bundle_manuals(Bundle, Name, HRef).
gen_catalog_bundle_info_(url(URL), Bundle) :-
    bundle_url(Bundle, URL).

%:- export(is_public_bundle/1).
is_public_bundle(Bundle) :-
    % avoid if it has a NODISTRIBUTE
    bundle_path(Bundle, 'NODISTRIBUTE', R),
    file_exists(R),
    !,
    fail.
% is_public_bundle(Bundle) :-
%     % public if there is a .git repo
%     bundle_path(Bundle, '.git', R),
%     file_exists(R),
%     !.
is_public_bundle(Bundle) :-
    % or it is in bndls/ or in a first-level bundle
    bundle_path(Bundle, '.', R),
    ( BndlsDir = ~path_concat(~ciao_root, 'bndls')
    ; BndlsDir = ~ciao_root
    ),
    path_split(R, BndlsDir, _),
    !.

:- use_module(engine(internals), [ciao_root/1]).

src_ciao_root := ~ciao_root. % TODO: customize?

% ---------------------------------------------------------------------------
%! ## Ad-hoc bundle info
% TODO: store and extract from manifest?
% TODO: add build status and build archive

bundle_tags(core, [min]).
bundle_tags(builder, [min]).
bundle_tags(devenv, [main]).
bundle_tags(lpdoc, [main]).
bundle_tags(ciaopp, [main]).
bundle_tags(ciao_emacs, [main]).
bundle_tags(ciaodbg_extra, [main]).
bundle_tags(ciaofmt, [main]).
bundle_tags(alldocs, [main]).

% The bundle is contained in a (larger) monorepo
% (otherwise it has its own repo)
bundle_monorepo(core, ciao, 'core').
bundle_monorepo(builder, ciao, 'builder').

% Bundles that are in the implicit dependencies (hidden)
implicit_dep(core).

:- export(github_ciaolang_url/1). % TODO: Move somewhere else
% Base URL for the ciao-lang organization at GitHub
github_ciaolang_url := 'https://github.com/ciao-lang'.

% ---------------------------------------------------------------------------

get_bundle_tags(Bundle, Tags) :-
    ( bundle_tags(Bundle, Tags0) -> Tags = Tags0
    ; Tags = []
    ).

% ---------------------------------------------------------------------------
%! ## Bundle description

:- export(guess_bundle_title/2). % TODO: Move somewhere else
% guess_bundle_title(+Bundle, ?Title):
%   Guess the bundle title.
%   It currently extracts it from the title of the README.md file.
%   The predicate fails if no title is found.
guess_bundle_title(Bundle, Title) :-
    get_md_readme(Bundle, R),
    file_to_string(R, Str),
    append("# ", Str2, Str),
    append(Title0, "\n"||_, Str2),
    !,
    Title = Title0.

%:- export(guess_bundle_desc/2).
% guess_bundle_desc(+Bundle, ?Title):
%   Guess the bundle description.
guess_bundle_desc(Bundle, Str) :-
    get_md_readme(Bundle, R),
    file_to_string(R, Str).

% A README.md file at the base of the bundle
get_md_readme(Bundle, R) :-
    bundle_path(Bundle, 'README.md', R),
    file_exists(R).

% ---------------------------------------------------------------------------
%! ## Bundle URLs at GitHub clone

:- export(github_bundle_url/3). % TODO: Move somewhere else
% github_bundle_url(+Bundle, +RelPath, ?HRef): URL for the given
%   source at the GitHub repository.
github_bundle_url(Bundle, RelPath, HRef) :-
    bundle_monorepo(Bundle, MonoRepo, MonoPath),
    !,
    ( RelPath = '.' -> RelPath2 = RelPath % TODO: OK? point to mono repo
    ; path_concat(MonoPath, RelPath, RelPath2)
    ),
    github_repo_url(MonoRepo, RelPath2, HRef).
github_bundle_url(Bundle, RelPath, HRef) :-
    github_repo_url(Bundle, RelPath, HRef).

github_repo_url(Repo, '.', HRef) :- !,
    HRef = ~path_concat(~github_ciaolang_url, Repo).
github_repo_url(Repo, RelPath, HRef) :-
    HRef0 = ~path_concat(~github_ciaolang_url, Repo),
    HRef1 = ~path_concat(HRef0, 'tree/master'),
    HRef = ~path_concat(HRef1, RelPath).

% ---------------------------------------------------------------------------
%! ## Dependencies of a bundle

:- use_module(ciaobld(manifest_compiler), [
    ensure_load_manifest/1, manifest_call/2, bundle_manual_base/2]).

:- use_module(library(pathnames),
    [path_concat/3, path_relocate/4, path_split/3, path_splitext/3]).

%:- export(bundle_deps/2).
bundle_deps(Bundle, Dep) :-
    ensure_load_manifest(Bundle),
    manifest_call(Bundle, dep(Dep0, _)),
    \+ implicit_dep(Dep0),
    Dep = Dep0.

%:- export(bundle_manuals/3).
bundle_manuals(Bundle, Name, HRef) :-
    ensure_load_manifest(Bundle), % TODO: was (calls ensure_load_manifest/1)
    maybe_bundle_manual_htmldir(Bundle, Path),
    % Name of manual (no extension, dir, etc.)
    path_split(Path, _, Name0),
    path_splitext(Name0, Name, _),
    % Obtain relative URL
    BuildDirDoc = ~bundle_path(Bundle, builddir, 'doc'),
    path_relocate(BuildDirDoc, '', Path, HRef).

%:- export(bundle_url/2).
% TODO: Not all bundles are in github!
bundle_url(Bundle) := HRef :- !,
    github_bundle_url(Bundle, '.', HRef).

% HtmlDir for specified Bundle manual, but do not check existence
% TODO: (see bundle_manual_htmldir/2)
maybe_bundle_manual_htmldir(Bundle, HtmlDir) :-
    NoExt = ~bundle_manual_base(Bundle),
    FileName = ~atom_concat(NoExt, '.html'),
    DocDir = ~bundle_path(Bundle, builddir, 'doc'),
    HtmlDir = ~path_concat(DocDir, FileName).

% ---------------------------------------------------------------------------
% Reverse dependencies
% NOTE: This may be slow! (it requires loading all bundle metadata)

:- data bundle_revdeps_computed/0.
:- data bundle_revdeps_/2.

%:- export(bundle_revdeps/2).
bundle_revdeps(Bundle, RevDep) :-
    ensure_revdeps,
    bundle_revdeps_(Bundle, RevDep).

ensure_revdeps :-
    ( bundle_revdeps_computed -> true
    ; compute_revdeps
    ).

compute_revdeps :-
    retractall_fact(bundle_revdeps_(_,_)),
    ( catalog_bundles(A),
        bundle_deps(A, Dep),
        \+ implicit_dep(Dep),
        ( current_fact(bundle_revdeps_(Dep, A)) -> true
        ; assertz_fact(bundle_revdeps_(Dep, A))
        ),
        fail
    ; true
    ),
    set_fact(bundle_revdeps_computed).
