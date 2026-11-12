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


:- module(test_doc_access,
          [ test_doc_access/0
          ]).
:- use_module(library(plunit)).

:- if(exists_source(library(http/http_hook))).	% http may not be built
:- use_module(library(pldoc/doc_access)).
:- use_module(library(http/http_hook)).

/** <module> Access control of the PlDoc server

The PlDoc edit handler starts an editor on the server.  It is only for
the user on localhost, but a web page in that user's browser could
otherwise make it start editors, and any client could claim to be
localhost using X-Forwarded-For.
*/

test_doc_access :-
    run_tests([ pldoc_edit_access ]).

:- begin_tests(pldoc_edit_access).

test(local) :-
    can_edit([peer(ip(127,0,0,1)), host(localhost), port(4000)]).
test(forwarded_by_remote_client, fail) :-
    can_edit([peer(ip(10,1,2,3)), x_forwarded_for('127.0.0.1')]).
test(forwarded_by_local_proxy, fail) :-
    can_edit([peer(ip(127,0,0,1)), x_forwarded_for('10.1.2.3')]).
test(cross_site, fail) :-
    can_edit([peer(ip(127,0,0,1)), sec_fetch_site('cross-site')]).
test(same_origin) :-
    can_edit([peer(ip(127,0,0,1)), sec_fetch_site('same-origin')]).
test(foreign_referer, fail) :-
    can_edit([ peer(ip(127,0,0,1)), host(localhost), port(4000),
               referer('http://evil.example/page')
             ]).
test(own_referer) :-
    can_edit([ peer(ip(127,0,0,1)), host(localhost), port(4000),
               referer('http://localhost:4000/pldoc/')
             ]).
test(other_local_port, fail) :-
    can_edit([ peer(ip(127,0,0,1)), host(localhost), port(4000),
               origin('http://localhost:8000')
             ]).

:- end_tests(pldoc_edit_access).

can_edit(Request) :-
    http:authenticate(pldoc(edit), Request, _).

:- else.

test_doc_access.

:- endif.
