/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(pldoc_process,
          [ doc_comment/4,              % ?Object, ?Pos, ?Summary, ?Comment
            doc_file_has_comments/1,    % +File
            is_structured_comment/2,    % +Comment, -Prefixes
            parse_comment/3,            % +Comment, +FilePos, -Parsed
            parse_comment/5,            % +Comment, +FilePos, +Term, +VarNames,
                                        % -Parsed
            comment_modes/2,            % +Comment, -Synopsis
            process_comments/3,         % +Comments, +StartTermPos, +File
            process_comments/5,         % +Comments, +StartTermPos, +Term,
                                        % +VarNames, +File
            doc_signature/2,            % ?Object, ?Signature
            doc_file_name/3,            % +Source, -Doc, +Options
            doc_clean/1                 % +Module
          ]).

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(pldoc, library(pldoc)).

:- use_module(pldoc(doc_register)).
:- use_module(pldoc(doc_modes)).
:- use_module(pldoc(doc_wiki)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(operators)).
:- use_module(library(prolog_source)).


/** <module> Process source documentation
The pldoc module processes structured comments in Prolog source files into
well formatted HTML documents.

@author  Jan Wielemaker
@license GPL
*/

:- predicate_options(doc_file_name/3, 3,
                     [ format(oneof([html,tex]))
                     ]).

%!  prolog:predicate_summary(+PI, -Summary) is semidet.
%
%   Provide    predicate    summaries    to     the    XPCE    class
%   =prolog_predicate=, used by the IDE tools.

:- multifile
    prolog:predicate_summary/2,     % ?PI, -Summary
    prolog:doc_compile_comment/6.   % +Comment, +Lines, +FilePos, +Term,
                                    % +VarNames, -Compiled

%!  prolog:doc_compile_comment(+Comment:string, +Lines, +FilePos, ?Term,
%!                             +VarNames, -Compiled:list) is semidet.
%
%   Hook to compile structured comments that do not document a
%   predicate or section, such as the members of an xpce class.  The
%   hook is tried before the default processing.  Lines are the
%   indented lines of Comment and Term is the term that follows the
%   comment.  Term is unbound if Comment is not the last comment before
%   the term.  VarNames is a list Name=Var for the variables of Term.
%   If the hook succeeds, Compiled is a list of terms as
%   described with parse_comment/3, normally holding
%   object(Object, Summary, Comment) and optionally
%   signature(Object, Signature).


%!  is_structured_comment(+Comment:string,
%!                        -Prefixes:list(codes)) is semidet.
%
%   True if Comment is a structured comment that should use Prefixes
%   to extract the plain text using indented_lines/3.

is_structured_comment(Comment, Prefixes) :-
    is_structured_comment(Comment, Prefixes, _Style).

is_structured_comment(_Pos-Comment, Prefixes, Style) :-
    !,
    is_structured_comment(Comment, Prefixes, Style).
is_structured_comment(Comment, Prefixes, Style) :-
    is_list(Comment),
    !,
    (   phrase(structured_comment(Prefixes, Style), Comment, _)
    ->  true
    ).
is_structured_comment(Comment, Prefixes, Style) :-
    atom_string(CommentA, Comment),
    structured_command_start(Start, Prefixes, Style),
    sub_atom(CommentA, 0, Len, _, Start),
    !,
    sub_atom(CommentA, Len, 1, _, Space),
    char_type(Space, space),
    (   Style == block
    ->  true
    ;   \+ blanks_to_nl(CommentA)
    ).

structured_command_start('%%',  ["%"], percent_percent).        % Deprecated
structured_command_start('%!',  ["%"], percent_bang).           % New style
structured_command_start('/**', ["/**", " *"], block).          % block

blanks_to_nl(CommentA) :-
    sub_atom(CommentA, At, 1, _, Char),
    At >= 2,
    (   char_type(Char, end_of_line)
    ->  !
    ;   (   char_type(Char, space)
        ;   Char == '%'
        )
    ->  fail
    ;   !, fail
    ).
blanks_to_nl(_).

%!  structured_comment(-Prefixes:list(codes), -Style) is semidet.
%
%   Grammar rule version of the above.  Avoids the need for
%   conversion.

structured_comment(["%"], percent_percent) -->
    "%%", space,
    \+ separator_line.
structured_comment(["%"], percent_bang) -->
    "%!", space.
structured_comment(Prefixes, block) -->
    "/**", space,
    { Prefixes = ["/**", " *"]
    }.

space -->
    [H],
    { code_type(H, space) }.

%!  separator_line// is semidet.
%
%   Matches a line like %% SWI or %%%%%%%%%%%%%%%%%%%%%%%%%, etc.

separator_line -->
    string(S), "\n",
    !,
    {   maplist(blank_or_percent, S)
    ;   contains(S, " SWI ")
    ;   contains(S, " SICStus ")
    ;   contains(S, " Mats ")
    }.

string([]) --> [].
string([H|T]) --> [H], string(T).

blank_or_percent(0'%) :- !.
blank_or_percent(C) :-
    code_type(C, space).

contains(Haystack, Needle) :-
    string_codes(Needle, NeedleCodes),
    append(_, Start, Haystack),
    append(NeedleCodes, _, Start),
    !.


%!  doc_file_name(+Source:atom, -Doc:atom, +Options:list) is det.
%
%   Doc is the name of the file for documenting Source.
%
%   @param Source   Prolog source to be documented
%   @param Doc      the name of the file documenting Source.
%   @param Options  Option list:
%
%                   * format(+Format)
%                   Output format.  One of =html= or =tex=
%
%   @error  permission_error(overwrite, Source)

doc_file_name(Source, Doc, Options) :-
    option(format(Format), Options, html),
    file_name_extension(Base, _Ext, Source),
    file_name_extension(Base, Format, Doc),
    (   Source == Doc
    ->  throw(error(permission_error(overwrite, Source), _))
    ;   true
    ).

%!  doc_file_has_comments(+Source:atom) is semidet.
%
%   True if we have loaded comments from Source.

doc_file_has_comments(Source) :-
    source_file_property(Source, module(M)),
    locally_defined(M:'$pldoc'/4),
    M:'$pldoc'(_, _, _, _),
    !.


%!  doc_comment(?Objects, -Pos,
%!              -Summary:string, -Comment:string) is nondet.
%
%   True if Comment is the  comment   describing  object. Comment is
%   returned as a string object  containing   the  original from the
%   source-code.  Object is one of
%
%           * Name/Arity
%           Predicate indicator
%
%           * Name//Arity
%           DCG rule indicator.  Same as Name/Arity+2
%
%           * module(ModuleTitle)
%           Comment appearing in a module.
%
%           * Other objects
%           Created by the hook prolog:doc_compile_comment/5, e.g.,
%           xpce(Class, Kind, Name) for members of xpce classes.
%
%   If Object is  unbound  and  multiple   objects  share  the  same
%   description, Object is unified with a   list  of terms described
%   above.
%
%   @param Summary  First sentence.  Normalised spacing.
%   @param Comment  Comment string from the source-code (untranslated)

doc_comment(Object, Pos, Summary, Comment) :-
    var(Object),
    !,
    locally_defined(M:'$pldoc'/4),
    M:'$pldoc'(Obj, Pos, Summary, Comment),
    qualify(M, Obj, Object0),
    (   locally_defined(M:'$pldoc_link'/2),
        findall(L, M:'$pldoc_link'(L, Obj), Ls), Ls \== []
    ->  maplist(qualify(M), Ls, QLs),
        Object = [Object0|QLs]
    ;   Object = Object0
    ).
doc_comment(M:Object, Pos, Summary, Comment) :-
    !,
    locally_defined(M:'$pldoc'/4),
    (   M:'$pldoc'(Object, Pos, Summary, Comment)
    ;   locally_defined(M:'$pldoc_link'/2),
        M:'$pldoc_link'(Object, Obj2),
        M:'$pldoc'(Obj2, Pos, Summary, Comment)
    ).
doc_comment(Name/Arity, Pos, Summary, Comment) :-
    system_module(M),
    doc_comment(M:Name/Arity, Pos, Summary, Comment).


%!  doc_signature(?Object, ?Signature:string) is nondet.
%
%   True when Signature is the signature  of Object as provided by the
%   hook prolog:doc_compile_comment/6.  Object is qualified as with
%   doc_comment/4.

doc_signature(M:Object, Signature) :-
    (   var(M)
    ->  locally_defined(M:'$pldoc_signature'/2)
    ;   true
    ),
    catch(M:'$pldoc_signature'(Object, Signature), error(_,_), fail).
doc_signature(Object, Signature) :-
    Object \= _:_,
    locally_defined(M:'$pldoc_signature'/2),
    M:'$pldoc_signature'(Object0, Signature),
    qualify(M, Object0, Object).

locally_defined(M:Name/Arity) :-
    current_predicate(M:Name/Arity),
    functor(Head, Name, Arity),
%   \+ predicate_property(M:Head, imported_from(_)).
    \+ '$get_predicate_attribute'(M:Head, imported, _).


qualify(M, H, H) :- system_module(M), !.
qualify(M, H, H) :- sub_atom(M, 0, _, _, $), !.
qualify(M, H, M:H).

system_module(user).
system_module(system).


%       Make the summary available to external tools on plugin basis.

prolog:predicate_summary(PI, Summary) :-
    doc_comment(PI, _, Summary, _).


                 /*******************************
                 *      CALL-BACK COLLECT       *
                 *******************************/

%!  process_comments(+Comments:list, +TermPos, +File) is det.
%!  process_comments(+Comments:list, +TermPos, ?Term, +VarNames,
%!                   +File) is det.
%
%   Processes comments returned by read_term/3 using the =comments=
%   option.  It creates clauses of the form
%
%           * '$mode'(Head, Det)
%           * '$pldoc'(Id, Pos, Summary, Comment)
%           * '$pldoc_link'(Id0, Id)
%
%   where Id is one of
%
%           * module(Title)
%           Generated from /** <module> Title */
%           * Name/Arity
%           Generated from Name(Arg, ...)
%           * Name//Arity
%           Generated from Name(Arg, ...)//
%
%   @param Comments is a list Pos-Comment returned by read_term/3
%   @param TermPos is the start-location of the actual term
%   @param Term is the term read.  It is passed to the
%   prolog:doc_compile_comment/6 hook for the last comment before
%   the term.
%   @param VarNames is a list Name=Var for the variables of Term.
%   @param File is the file that is being loaded.

process_comments(Comments, TermPos, File) :-
    process_comments(Comments, TermPos, _, [], File).

process_comments([], _, _, _, _).
process_comments([Pos-Comment|T], TermPos, Term, VarNames, File) :-
    (   Pos @> TermPos              % comments inside term
    ->  true
    ;   (   last_comment(T, TermPos)
        ->  CTerm = Term
        ;   true
        ),
        process_comment(Pos, Comment, CTerm, VarNames, File),
        process_comments(T, TermPos, Term, VarNames, File)
    ).

last_comment([], _).
last_comment([Pos-_|_], TermPos) :-
    Pos @> TermPos.

process_comment(Pos, Comment, Term, VarNames, File) :-
    is_structured_comment(Comment, Prefixes, Style),
    !,
    stream_position_data(line_count, Pos, Line),
    FilePos = File:Line,
    process_structured_comment(FilePos, Comment, Prefixes,
                               Term, VarNames, Style).
process_comment(_, _, _, _, _).

%!  parse_comment(+Comment, +FilePos, -Parsed) is semidet.
%!  parse_comment(+Comment, +FilePos, ?Term, +VarNames,
%!                -Parsed) is semidet.
%
%   True when Comment is a  structured   comment  and  Parsed is its
%   parsed representation. Parsed is a list of the following terms:
%
%     * section(Id, Title, Comment)
%     Generated from /** <module> Title Comment */ comments.
%     * predicate(PI, Summary, Comment)
%     Comment for predicate PI
%     * link(FromPI, ToPI)
%     Indicate that FromPI shares its comment with ToPI.  The actual
%     comment is in ToPI.
%     * mode(Head, Determinism)
%     Mode declaration.  Head is a term with Mode(Type) terms and
%     Determinism describes the associated determinism (=det=,
%     etc.).
%     * object(Object, Summary, Comment)
%     Comment for some other Object, produced by the hook
%     prolog:doc_compile_comment/6.
%     * signature(Object, Signature)
%     Signature of Object as a string, produced by the hook
%     prolog:doc_compile_comment/6.
%
%   Term is the term that follows Comment and VarNames is a list
%   Name=Var for its variables.  Both are passed to
%   prolog:doc_compile_comment/6.

parse_comment(Comment, FilePos, Parsed) :-
    parse_comment(Comment, FilePos, _, [], Parsed).

parse_comment(Comment, FilePos, Term, VarNames, Parsed) :-
    is_structured_comment(Comment, Prefixes),
    !,
    compile_comment(Comment, FilePos, Prefixes, Term, VarNames, Parsed).


%!  comment_modes(+Comment, -Modes:list) is semidet.

comment_modes(Comment, Modes) :-
    is_structured_comment(Comment, Prefixes),
    string_codes(Comment, CommentCodes),
    indented_lines(CommentCodes, Prefixes, Lines),
    (   prolog_load_context(module, Module)
    ->  true
    ;   Module = user
    ),
    process_modes(Lines, Module, dummy:0, Modes0, _Vars, _),
    maplist(bind_mode, Modes0, Modes).

bind_mode(mode(Mode, Bindings), Mode) :-
    maplist(bind_var, Bindings).

bind_var(Name=Name).

%!  process_structured_comment(+FilePos,
%!                             +Comment:string,
%!                             +Prefixed:list,
%!                             ?Term, +VarNames,
%!                             +Style) is det.
%
%   Proccess a structured comment, adding the documentation facts to
%   the database. This predicate verifies that   the comment has not
%   already been loaded.
%
%   @tbd Note that as of version 7.3.12   clauses  from a file being
%   reloaded are not wiped before  the   reloading  and therefore we
%   cannot test the clause while  reloading   a  file. Ultimately we
%   need a better test for this.

process_structured_comment(FilePos, Comment, _, _, _, _) :- % already processed
    prolog_load_context(module, M),
    locally_defined(M:'$pldoc'/4),
    catch(M:'$pldoc'(_, FilePos, _, Comment), _, fail),
    (   FilePos = File:_,
        source_file_property(File, reloading)
    ->  debug(pldoc(reload), 'Reloading ~q', [FilePos]),
        fail
    ;   true
    ),
    !.
process_structured_comment(FilePos, Comment, Prefixes, Term, VarNames,
                           Style) :-
    catch(compile_comment(Comment, FilePos, Prefixes, Term, VarNames,
                          Compiled), E,
          comment_warning(Style, E)),
    maplist(store_comment(FilePos), Compiled).
process_structured_comment(FilePos, Comment, _Prefixes, _Term, _VarNames,
                           Style) :-
    comment_style_warning_level(Style, Level),
    print_message(Level,
                  pldoc(invalid_comment(FilePos, Comment))).

comment_style_warning_level(percent_percent, silent) :- !.
comment_style_warning_level(_, warning).

%!  comment_warning(+Style, +Error) is failure.
%
%   Print a warning  on  structured  comments   that  could  not  be
%   processed. Since the recommended magic   sequence is now =|%!|=,
%   we remain silent about comments that start with =|%%|=.

comment_warning(Style, E) :-
    comment_style_warning_level(Style, Level),
    print_message(Level, E),
    fail.

%!  compile_comment(+Comment, +FilePos, +Prefixes, ?Term, +VarNames,
%!                  -Compiled) is semidet.
%
%   Compile structured Comment into a list   of  terms that describe
%   the comment.
%
%   @see parse_comment/3 for the terms in Compiled.

compile_comment(Comment, FilePos, Prefixes, Term, VarNames, Compiled) :-
    string_codes(Comment, CommentCodes),
    indented_lines(CommentCodes, Prefixes, Lines),
    (   prolog:doc_compile_comment(Comment, Lines, FilePos, Term, VarNames,
                                   Compiled)
    ->  true
    ;   section_comment_header(Lines, Header, _RestLines)
    ->  Header = \section(Type, Title),
        Id =.. [Type,Title],
        Compiled = [section(Id, Title, Comment)]
    ;   prolog_load_context(module, Module),
        process_modes(Lines, Module, FilePos, Modes, _, RestLines)
    ->  maplist(compile_mode, Modes, ModeDecls),
        modes_to_predicate_indicators(Modes, AllPIs),
        decl_module(AllPIs, M, [PI0|PIs]),
        maplist(link_term(M:PI0), PIs, Links),
        summary_from_lines(RestLines, Codes),
        string_codes(Summary, Codes),
        append([ ModeDecls,
                 [ predicate(M:PI0, Summary, Comment) ],
                 Links
               ], Compiled)
    ),
    !.


store_comment(Pos, section(Id, Title, Comment)) :-
    !,
    compile_clause('$pldoc'(Id, Pos, Title, Comment), Pos).
store_comment(Pos, predicate(M:PI, Summary, Comment)) :-
    !,
    compile_clause(M:'$pldoc'(PI, Pos, Summary, Comment), Pos).
store_comment(Pos, link(PI, M:PI0)) :-
    !,
    compile_clause(M:'$pldoc_link'(PI, PI0), Pos).
store_comment(Pos, mode(Head, Det)) :-
    !,
    compile_clause('$mode'(Head, Det), Pos).
store_comment(Pos, object(Object, Summary, Comment)) :-
    !,
    compile_clause('$pldoc'(Object, Pos, Summary, Comment), Pos).
store_comment(Pos, signature(Object, Signature)) :-
    !,
    compile_clause('$pldoc_signature'(Object, Signature), Pos).
store_comment(_, Term) :-
    type_error(pldoc_term, Term).

link_term(To, From, link(From,To)).

decl_module([], M, []) :-
    (   var(M)
    ->  prolog_load_context(module, M)
    ;   true
    ).
decl_module([H0|T0], M, [H|T]) :-
    (   H0 = M1:H
    ->  M = M1
    ;   H = H0
    ),
    decl_module(T0, M, T).

%!  doc_clean(+Module) is det.
%
%   Clean documentation for Module.

doc_clean(Module) :-
    abolish(Module:'$mode'/2),
    abolish(Module:'$pldoc'/4),
    abolish(Module:'$pldoc_link'/2),
    abolish(Module:'$pldoc_signature'/2).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(pldoc(invalid_comment(File:Line, Comment))) -->
    [ url(File:Line), ': PlDoc: failed to process structured comment:~n~s~n'-
            [Comment]
    ].
