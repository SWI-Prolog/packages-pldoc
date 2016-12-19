/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2011, University of Amsterdam
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

:- module(pldoc_csssrc,
          [ write_source_css/0,         % Create pllisting.css
            write_source_css/1          % +Stream
          ]).
:- use_module(library(pce)).
:- use_module(doc_htmlsrc).
:- use_module(doc_colour).


%!  write_source_css is det.
%!  write_source_css(+Out:stream) is det.
%
%   Create   a   style-sheet   from    the   style-declarations   in
%   doc_colour.pl    and    the    element     declaration    above.
%   write_source_css/0 writes the style-sheet to =|pllisting.css|=.

:- op(990, xfx, :=).

write_source_css :-
    open('pllisting.css', write, Out),
    call_cleanup(write_source_css(Out),
                 close(Out)).

write_source_css(Out) :-
    (   prolog_src_style(Term, Style0),
        (   html_style(Term, Style)
        ->  true
        ;   Style = Style0
        ),
        pldoc_htmlsrc:element(Term2, Tag, Class),
        Term2 =@= Term,
        findall(Name=Value, style_attr(Style, Name, Value),
                [N=V|NV]),
        format(Out, '~w.~w~n', [Tag, Class]),
        format(Out, '{ ~w: ~w;~n', [N, V]),
        forall(member(N2=V2, NV),
               format(Out, '  ~w: ~w;~n', [N2, V2])),
        format(Out, '}~n~n', []),
        fail
    ;   true
    ).

style_attr(Style, Name, Value) :-
    arg(_, Style, PceName := PceValue),
    pce_to_css_attr(PceName, Name),
    pce_to_css_value(Name, PceValue, Value).

pce_to_css_attr(colour, color).
pce_to_css_attr(background, 'background-color').
pce_to_css_attr(underline, 'text-decoration').
pce_to_css_attr(bold, 'font-weight').
pce_to_css_attr('font-style', 'font-style').
pce_to_css_attr(display, display).

pce_to_css_value(color, Name, RGB) :-
    x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('background-color', Name, RGB) :-
    x11_colour_name_to_rgb(Name, RGB).
pce_to_css_value('text-decoration', @(on), underline).
pce_to_css_value('font-weight', @(on), bold).
pce_to_css_value('font-style', Style, Style).

x11_colour_name_to_rgb(red, red) :- !.
x11_colour_name_to_rgb(blue, blue) :- !.
x11_colour_name_to_rgb(Name, RGB) :-
    get(@(pce), convert, Name, colour, Obj),
    get(Obj, red, R),
    get(Obj, green, G),
    get(Obj, blue, B),
    R256 is R//256,
    G256 is G//256,
    B256 is B//256,
    format(atom(RGB),
           '#~|~`0t~16r~2+~`0t~16r~2+~`0t~16r~2+',
           [R256, G256, B256]).

%!  html_style(+Term, -Style) is semidet.
%
%   Redefine styles from prolog_src_style/2 for better ones on
%   HTML output.

html_style(var,
           style(colour := red4,
                 'font-style' := italic)).
html_style(directive,
           style(background := grey90,
                 'display' := block)).
