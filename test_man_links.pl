/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_man_links,
          [ test_man_links/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(pldoc)).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(pldoc/man_index), [manual_object/5]).
:- use_module(library(doc_http), []).   % register the PlDoc HTTP handlers
:- if(exists_source(library(help))).
:- use_module(library(help)).
:- endif.
:- use_module(library(http/html_write)).
:- use_module(library(sgml)).
:- use_module(library(apply)).
:- use_module(library(yall)).
:- use_module(library(lists)).
:- use_module(library(terms), [mapsubterms/3]).

/** <module> Test resolving manual hyperlinks

Verify that the links help/1 embeds into  its output as OSC8 hyperlinks
can be resolved back into the object they refer to.

@see man_link/2 in library(help).
*/

test_man_links :-
    run_tests([ man_links
              ]).

%!  manual_available is semidet.
%
%   True when the HTML manual and library(help) are installed.  Both are
%   only installed if the documentation is built.  See the use of
%   ``INSTALL_DOCUMENTATION`` in src/CMakeLists.txt.

manual_available :-
    exists_source(library(help)),
    manual_object(_,_,_,_,_),
    !.

%!  page_hrefs(+Object, -HREFs) is det.
%!  page_hrefs(+Object, +Options, -HREFs) is det.
%
%   HREFs are the links in the page   help/1 creates for Object. Uses the
%   same options as help_html/3.

page_hrefs(Object, HREFs) :-
    page_dom(Object, [server(false), link_scheme(man)], DOM0),
    mapsubterms(prolog_help:man_link, DOM0, DOM),
    dom_hrefs(DOM, HREFs).

page_hrefs(Object, Extra, HREFs) :-
    page_dom(Object, Extra, DOM),
    dom_hrefs(DOM, HREFs).

page_dom(Object, Extra, DOM) :-
    append(Extra,
           [ no_manual(fail),
             links(false),
             link_source(false),
             navtree(false),
             qualified(always)
           ], Options),
    phrase(html(html([ head([]),
                       body(dl(\man_page(Object, Options)))
                     ])),
           Tokens),
    with_output_to(string(HTML), print_html(Tokens)),
    load_html(string(HTML), DOM, []).

%!  page_img_src(+Object, +Extra, -Src) is semidet.
%
%   Src is the =src= of the first image on the page for Object.

page_img_src(Object, Extra, Src) :-
    page_dom(Object, Extra, DOM),
    sub_term(element(img, Attrs, _), DOM),
    memberchk(src=Src, Attrs),
    !.

dom_hrefs(DOM, HREFs) :-
    findall(HREF,
            ( sub_term(element(a,Attrs,_), DOM),
              memberchk(href=HREF, Attrs)
            ), HREFs0),
    sort(HREFs0, HREFs).

%!  clickable(+URI) is semidet.
%
%   True when URI is a `man:` IRI  and   help/1  finds the object it maps
%   to. Note that help_objects_how/3 is  the   internal  entry point that
%   does not print anything.

clickable(URI) :-
    man_uri_object(URI, Object),
    prolog_help:help_objects_how(Object, _Matches, exact).

%!  href_roundtrip(+HREF) is semidet.
%
%   True when HREF, a link to the  PlDoc   server,  maps to an object and
%   the `man:` IRI of that object maps back to the same object.

href_roundtrip(HREF) :-
    pldoc_href_object(HREF, Object),
    man_object_uri(Object, URI),
    man_uri_object(URI, Object2),
    Object2 =@= Object.

pages([ format/2,
        msort/2,
        assertz/1,
        section('sec:exception'),
        f(sqrt/1),
        c('PL_unify_atom')
      ]).

all_hrefs(HREFs) :-
    pages(Pages),
    maplist(page_hrefs, Pages, Lists),
    append(Lists, HREFs).

:- begin_tests(man_links, [condition(manual_available)]).

test(pages_have_links) :-
    all_hrefs(HREFs),
    assertion(HREFs \== []).

% Every link man_page//2 creates for help/1 is a `man:` IRI that
% resolves to an object help/1 accepts.

test(clickable, Bad == []) :-
    all_hrefs(HREFs),
    exclude(clickable, HREFs, Bad).

% The links of the PlDoc server map onto the same objects.  These reach
% help/1 through the links doc_html.pl creates, e.g., the synopsis.

test(server_roundtrip, Bad == []) :-
    pages(Pages),
    maplist([O,H]>>page_hrefs(O,[],H), Pages, Lists),
    append(Lists, HREFs),
    include(pldoc_link, HREFs, Ours),
    assertion(Ours \== []),
    exclude(href_roundtrip, Ours, Bad).

test(uri, URI == 'man:format/3') :-
    man_object_uri(format/3, URI).

test(uri_section, URI == 'man:section(''sec:exception'')') :-
    man_object_uri(section(2, '4.10', 'sec:exception', '/doc/exception.html'),
                   URI).

test(uri_operator, Object == (==)/2) :-
    man_object_uri((==)/2, URI),
    man_uri_object(URI, Object).

% The objects apropos/1 prints are clickable and so is the line that
% offers the next page.

test(apropos_clickable, Bad == []) :-
    findall(URI,
	    ( help_apropos(open, Obj, _Summary, _Score),
	      man_object_uri(Obj, URI)
	    ), URIs),
    assertion(URIs \== []),
    exclude(clickable, URIs, Bad).

test(apropos_uri, Goal == apropos('open file', [offset(20)])) :-
    prolog_help:apropos_uri('open file', 20, URI),
    prolog_help:apropos_uri_goal(URI, Goal).

test(not_an_object, fail) :-
    pldoc_href_object('/pldoc/doc/home/jan/x.pl#foo/1', _).

% Figures embedded in the manual.  Without a server (help/1) they become
% a `file://` URI; the server routes them through the pldoc_refman
% handler.

test(image_no_server, true(sub_atom(Src, 0, _, _, 'file://'))) :-
    page_img_src(section('sec:broadcast'), [server(false)], Src).

test(image_server, Src == '/pldoc/refman/broadcast.png') :-
    page_img_src(section('sec:broadcast'), [], Src).

% Every documented object renders as a help/1 page without raising.

test(render_all, Bad == []) :-
    findall(Obj-E, render_error(Obj, E), Bad).

:- end_tests(man_links).

pldoc_link(HREF) :-
    sub_atom(HREF, 0, _, _, '/pldoc/').

%!  render_error(-Object, -Error) is nondet.
%
%   True when rendering the help/1 page for Object raises Error.

render_error(Object, Error) :-
    manual_object(Object, _, _, _, _),
    catch(( page_dom(Object, [server(false)], _),
            fail
          ), Error, true).
