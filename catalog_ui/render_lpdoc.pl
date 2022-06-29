:- module(_, [], [assertions, dcg, fsyntax]).

% From `docfile` (a .pl module, .lpdoc, or .md file containing
% documentation) to a HTML term. If translation fails, emits a
% verbatim text view.

:- use_module(library(pathnames)).
:- use_module(library(system), [file_exists/1, mktemp_in_tmp/2, make_directory/1]).
:- use_module(library(system_extra), [del_file_nofail/1]).
:- use_module(library(stream_utils), [file_to_string/2, string_to_file/2]).
:- use_module(library(port_reify)).

:- use_module(lpdoc(lpdoc_single_mod), [docfile_to_html_term/2]).

:- export(render_docfile/2).
% Render as HTML a documentation file (single file)
% (supports .lpdoc, .md)
render_docfile(File) := HtmlTerm :-
    \+ is_plain_text(File),
    catch(docfile_to_html_term(File, HtmlTerm0), _, fail),
    !,
    HtmlTerm = HtmlTerm0.
render_docfile(File) := ~render_txt(File) :- file_exists(File), !.
render_docfile(_File) := ['[File not found]'].

render_txt(File) := R :-
    file_to_string(File, Content),
    R = env(pre, [], [verbatim(Content)]).

is_plain_text(R) :-
    % No extension, assume plain text
    path_splitext(R, _, '').

create_tmp_base(TmpBase) :- % TODO: should we create TmpBase file??? otherwise there may be conflicts
    mktemp_in_tmp('modXXXXXX', TmpBase).

:- export(render_docstring/2).
render_docstring(String) := HtmlTerm :-
    % Create a temporary module with Input
    create_tmp_base(InBase), % (new name)
    atom_concat(InBase, '.lpdoc', InFile),
    string_to_file(String, InFile),
    once_port_reify(render_docfile(InFile, HtmlTerm), Port),
    del_file_nofail(InFile),
    port_call(Port).

