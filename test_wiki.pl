/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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


:- module(test_pldoc,
          [
          ]).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_process)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/jquery)).
:- use_module(library(pldoc/doc_modes)).
:- use_module(library(pldoc/doc_wiki)).
:- use_module(library(pldoc/doc_html), []).
:- use_module(library(readutil)).
:- use_module(library(sgml)).

/** <module> PlDoc test suite

This module provides the infrastructure to   define  and test PlDoc wiki
processing. To use it, run the command   below and point your browser at
http://localhost:4040/

    swipl test_wiki.pl
*/

user:file_search_path(js, js).
user:file_search_path(js, .).
user:file_search_path(css, css).
user:file_search_path(css, .).

:- initialization(server(4040), main).

%!  server(?Port)
%
%   Start the server at http://localhost:4040/

server(Port) :-
    set_prolog_flag(toplevel_goal, prolog),
    http_server(http_dispatch,
                [ port(localhost:Port)
                ]).

:- http_handler(root(.),       home,    []).
:- http_handler(root(wiki),    wiki,    []).
:- http_handler(root(approve), approve, []).
:- http_handler(root(tests),   tests,   []).

%!  home(+Request)
%
%   Present the test home page

home(_Request) :-
    reply_html_page(
        title('PlDoc test environment'),
        [ \html_requires(jquery),
          \html_requires(js('pldoc.js')),
          \html_requires(js('laconic.js')),
          \html_requires(js('test_pldoc.js')),
          \html_requires(css('pldoc.css')),
          \html_requires(css('test_pldoc.css')),
          h1('PlDoc test environment'),
          div(class(content), []),
          div(class(footer), button(id(new), 'Add new test'))
        ]).

%!  tests(+Request)
%
%   Read all defined test cases.

tests(_Request) :-
    expand_file_name('tests/*.in', Inputs),
    maplist(read_test, Inputs, Tests),
    reply_json_dict(Tests).

read_test(File, _{name:Test, text:String}) :-
    read_file_to_string(File, String, [encoding(utf8)]),
    file_base_name(File, Base),
    file_name_extension(Test, _, Base).

%!  approve(+Request)
%
%   Save and approve a test

approve(Request) :-
    http_read_json_dict(Request, In),
    directory_file_path(tests, In.name, Base),
    file_name_extension(Base, in, FileIn),
    file_name_extension(Base, ap, FileAp),
    text_to_html(In.text, HTML, DOM),
    with_output_to(string(DOMS), write_canonical(DOM)),
    setup_call_cleanup(
        open(FileIn, write, OutText, [encoding(utf8)]),
        format(OutText, '~w', [In.text]),
        close(OutText)),
    setup_call_cleanup(
        open(FileAp, write, OutAp, [encoding(utf8)]),
        json_write_dict(OutAp,
                        json{status:approved,
                             html:HTML,
                             dom:DOMS}),
        close(OutAp)),
    reply_json_dict(_{result: true}).


%!  wiki(+Request)
%
%   Handle a post request, returning  the   HTML.  Implemented as a JSON
%   POST request.

wiki(Request) :-
    http_read_json_dict(Request, In),
    text_to_html(In.text, HTML, DOM),
    approved(In.name, HTML, DOM, Approval),
    dom_pretty_string(DOM, DOMS),
    reply_json_dict(_{name: In.name, html:HTML, dom:DOMS}.put(Approval)).


text_to_html(String, HTML, DOM) :-
    is_structured_comment(String, Prefixes),
    !,
    string_codes(String, Codes),
    indented_lines(Codes, Prefixes, Lines),
    (   section_comment_header(Lines, Header, Lines1)
    ->  DOM = [Header|DOM1],
        Args = []
    ;   process_modes(Lines, user, tmp:1, Modes, Args, Lines1)
    ->  DOM = [\pred_dt(Modes, pubdef, []), dd(class=defbody, DOM1)]
    ),
    wiki_lines_to_dom(Lines1, Args, DOM0),
    strip_leading_par(DOM0, DOM1),
    add_missing_tag(DOM, body, DOMz),
    dom_to_html_string(DOMz, HTML).
text_to_html(String, HTML, DOM) :-
    string_codes(String, Codes),
    wiki_codes_to_dom(Codes, [], DOM),
    dom_to_html_string(DOM, HTML).

dom_to_html_string(DOM, HTML) :-
    phrase(html(pldoc_html:DOM), Tokens),
    with_output_to(string(HTML), print_html(current_output, Tokens)).

add_missing_tag(H, Outer, Env) :-
    requires(H, Tag), Tag \== Outer,
    !,
    Env =.. [Tag,H].
add_missing_tag(H, _Outer, H).

requires([\pred_dt(_)|_], dl).

dom_pretty_string(DOM, String) :-
    with_output_to(string(String), print_term(DOM, [output(current_output)])).

%! approved(+Test:string, +HTML:string, DOM:term, Approval:dict) is det.
%
%  Evaluate the test result against the approved version

approved(Test, HTML, DOM, Approval) :-
    directory_file_path(tests, Test, Base),
    file_name_extension(Base, ap, FileAp),
    exists_file(FileAp),
    !,
    setup_call_cleanup(
        open(FileAp, read, In, [encoding(utf8)]),
        json_read_dict(In, ApDict),
        close(In)),
    term_string(DOMAp, ApDict.dom),
    (   DOMAp =@= DOM
    ->  DOMOK = true
    ;   DOMOK = false
    ),
    (   same_html(HTML, ApDict.html)
    ->  HTMLOK = true
    ;   HTMLOK = false
    ),
    dom_pretty_string(ApDict.dom, DOMS),
    Approval = json{ approved:
                     _{ html:ApDict.html,
                        dom:DOMS
                      },
                     result:
                     _{ dom:DOMOK,
                        html:HTMLOK
                      }}.
approved(_, _, _, json{approved:null}).

same_html(HTML1, HTML2) :-
    html_dom(HTML1, DOM1),
    html_dom(HTML2, DOM2),
    DOM1 =@= DOM2.

html_dom(String, DOM) :-
    setup_call_cleanup(
        open_string(String, In),
        load_html(In, DOM, []),
        close(In)).
