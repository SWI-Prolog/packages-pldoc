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

:- module(doc_changes,
          [ doc_introduced/2,    % ?Indicator, ?Version
            doc_added/2,         % ?Pred, ?Version
            doc_last_changed/2,  % ?Pred, ?Version
            doc_last_changed/3,  % ?Pred, ?Version, ?Type
            doc_history/2        % ?Indicator, -Events
          ]).
:- autoload(library(pairs), [pairs_values/2]).
:- autoload(library(aggregate), [aggregate_all/3]).
:- autoload(library(solution_sequences), [distinct/2]).

:- if(exists_source(library(pldoc/changelog_events))).
:- autoload(library(pldoc/changelog_events), [changelog_event/6]).
:- else.
% keep silent, but does not work
:- multifile changelog_event/6.
:- endif.

%!  doc_introduced(?Indicator, ?Version) is nondet.
%
%   Indicator is a Name/Arity (or library(M)) that was first introduced
%   in Version.

doc_introduced(Indicator, Version) :-
    changelog_event(Indicator, introduced, Version, _, _, _).

%!  doc_added(?Pred, ?Version) is nondet.
%
%   Some functionality was added to Pred in Version (a new option,
%   property, mode, ...).  Pred was *not* introduced in this event.

doc_added(Pred, Version) :-
    changelog_event(Pred, added, Version, _, _, _).

%!  doc_last_changed(?Pred, ?Version) is nondet.
%!  doc_last_changed(?Pred, ?Version, ?Type) is nondet.
%
%   Version is the most recent ADDED/ENHANCED/MODIFIED/FIXED event
%   for Pred.  "introduced" events are deliberately excluded — they
%   mark the predicate's origin, not a change to an existing one.

doc_last_changed(Pred, Version) :-
    doc_last_changed(Pred, Version, _).

doc_last_changed(Pred, Version, Type) :-
    nonvar(Pred),
    !,
    last_change(Pred, Version, Type).
doc_last_changed(Pred, Version, Type) :-
    distinct(Pred, changed_predicate(Pred)),
    last_change(Pred, Version, Type).

last_change(Pred, Version, Type) :-
    aggregate_all(max(Date, V),
                  ( changelog_event(Pred, Type, V, Date, _, _),
                    change_type(Type)
                  ),
                  max(_, Version)).

change_type(added).
change_type(enhanced).
change_type(modified).
change_type(fixed).

changed_predicate(Pred) :-
    changelog_event(Pred, T, _V, _D, _H, _S),
    change_type(T).

%!  doc_history(?Pred, -Events) is nondet.
%
%   Events is the list of all events for Pred, oldest first.  Each
%   element is event(Type, Version, Hash, Subject).

doc_history(Pred, Events) :-
    (   ground(Pred)
    ->  true
    ;   distinct(Pred, changelog_event(Pred,_T,_V,_D,_H,_S))
    ),
    findall(Date-event(Type, Version, Hash, Subject),
            changelog_event(Pred, Type, Version, Date, Hash, Subject),
            Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Events).
