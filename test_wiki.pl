/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2017, University of Amsterdam
                              VU University Amsterdam
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

:- module(test_wiki,
          [ test_wiki/0,
            test_wiki/1
          ]).
:- use_module(library(prolog_source)).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(pldoc/doc_modes)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(doc_http)).
:- use_module(library(http/html_write)).

/** <module> PlDoc testing module

Just some random tests.
*/

process_comment(File, Pos-String, DOM) :-
    stream_position_data(line_count, Pos, Line),
    FilePos = File:Line,
    is_structured_comment(String, Prefixes),
    string_codes(String, Codes),
    indented_lines(Codes, Prefixes, Lines),
    (   section_comment_header(Lines, Header, Lines1)
    ->  DOM = [Header|DOM1],
        Args = []
    ;   process_modes(Lines, user, FilePos, Modes, Args, Lines1)
    ->  DOM = [\pred_dt(Modes, pubdef, []), dd(class=defbody, DOM1)]
    ),
    wiki_lines_to_dom(Lines1, Args, DOM0),
    strip_leading_par(DOM0, DOM1).

%!  read_structured_comments(+File, -Comments) is det.

read_structured_comments(File, Comments) :-
    setup_call_cleanup(
        prolog_open_source(File, In),
        read_comments(In, Comments0),
        prolog_close_source(In)),
    append(Comments0, Comments1),
    include(structured_comment, Comments1, Comments).

structured_comment(_Pos-Comment) :-
    is_structured_comment(Comment, _).

read_comments(In, [H|T]) :-
    prolog_read_source_term(In, Term, _, [comments(H)]),
    (   Term == end_of_file
    ->  T = []
    ;   read_comments(In, T)
    ).

%!  process_comment_list(+Comments, +File, -DOM) is det.
%
%   @param Mode     Enclosing environment, =body= or =dl=

process_comment_list(Comments, File, DOM) :-
    maplist(process_comment(File), Comments, DOMList),
    phrase(missing_tags(DOMList, body), DOM).

missing_tags([], _) -->
    [].
missing_tags([H|T0], Outer) -->
    { requires(H, Tag), Tag \== Outer,
      !,
      Env =.. [Tag,C],
      phrase(in_tag([H|T0], T, Tag), C)
    },
    [Env],
    missing_tags(T, Outer).
missing_tags([H|T], Outer) -->
    H,
    missing_tags(T, Outer).

in_tag([], [], _) -->
    !,
    [].
in_tag(L, L, Tag) -->
    { L = [H|_],
      \+ requires(H,Tag)
    },
    !,
    [].
in_tag([H|T0], T, Tag) -->
    H,
    in_tag(T0, T, Tag).


requires([\pred_dt(_)|_], dl).

test_wiki :-
    test('wiki_test_data').

test_wiki(Spec) :-
    absolute_file_name(Spec, File, [file_type(prolog)]),
    read_structured_comments(File, Comments),
    process_comment_list(Comments, File, DOM),
    doc_file_name(File, DocFile, [format(html)]),
    open(DocFile, write, Out),
    call_cleanup(doc_write_html(Out, File, DOM),
                 close(Out)).

doc :-
    Port = 4000,
    doc_server(Port),
    format(atom(URL), 'http://localhost:~w/', [Port]),
    www_open_url(URL).
