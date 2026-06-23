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

:- module(git_extract_changes,
          [ git_extract_changelog/0,
            git_extract_changelog/1                  % +Options
          ]).

:- autoload(library(git), [git_shortlog/3, git_log_data/3, git/2]).
:- autoload(library(apply), [include/3, exclude/3, maplist/3, maplist/2]).
:- autoload(library(lists), [member/2, append/2, subtract/3, last/2, append/3]).
:- autoload(library(option), [option/3, option/2]).
:- autoload(library(process), [process_create/3]).
:- autoload(library(filesex), [directory_file_path/3]).
:- autoload(library(listing), [portray_clause/2]).
:- use_module(library(settings), [setting/4, setting/2]).
:- autoload(library(aggregate), [aggregate_all/3]).
:- autoload(library(dcg/basics),
            [whites/2, white/2, integer/3, csym/3, string/3, eol/2, remainder/3]).
:- autoload(library(prolog_xref), [xref_public_list/3]).
:- autoload(library(pure_input), [phrase_from_file/2]).

/** <module> Analyse GIT history to generate predicate and library history

This module analyses the GIT commits and   diffs  of selected commits to
generate a file that provides  information   on  the  history of library
files as well as individual predicates. In particular, we monitor:

  - introduced
    Which commit and version introduced the library or predicate
  - added
    Some functionality was added without modifying existing
    functionality
  - enhanced
    Something was improved (performance, cleanup)
  - fixed
    A bug was fixed
  - modified
    Some existing behaviour changed.

This module generates a Prolog file that is   intended to be used by the
manual and possibly the development tools (check/0, PceEmacs, ...).
*/

:- setting(default_start_version, atom, 'V8.0.0',
           "Default version after which to track changes").
:- setting(source, atom, default,
           "Default location of the sources").

%!  git_extract_changelog is det.
%!  git_extract_changelog(+Options) is det.
%
%   Rebuild changelog_events.pl from the SWI-Prolog git checkout.
%   Options:
%
%     * swipl_source(+Dir)
%       The SWI-Prolog source directory.  Default is the setting
%       `source`.
%     * revisions(+Range)
%       Git revision range, e.g. 'V10.1.0..HEAD'.
%       Default: '<default_start_version>..HEAD'.
%     * output(+File)
%       Output path. Default:
%       swi('library/ext/pldoc/pldoc/changelog_events.pl')
%     * submodules(+Bool)
%       Recurse into git submodules.  Default: true.
%     * next_version(+Atom)
%       Next version for not-yet-committed changes.  By default, the
%       patch level is incremented.

git_extract_changelog :-
    git_extract_changelog([]).

git_extract_changelog(Options) :-
    resolve_source(Dir, Options),
    resolve_output(OutFile, Options),
    resolve_revisions(Range, Options),
    option(submodules(DoSubs), Options, true),
    print_message(informational, changelog(scanning(Dir, Range))),
    next_version(Dir, NextV, Options),
    print_message(informational, changelog(next_version(NextV))),
    repo_of(Dir, MainRepo),
    mine_repo(Dir, Range, NextV, '', MainRepo, MainEvents),
    (   DoSubs == true
    ->  mine_submodules(Dir, Range, NextV, SubEvents)
    ;   SubEvents = []
    ),
    append(MainEvents, SubEvents, Events0),
    sort(Events0, Events),
    write_events_file(OutFile, Events),
    length(Events, NE),
    print_message(informational, changelog(wrote(NE, OutFile))),
    use_module(OutFile).

resolve_source(Dir, Options) :-
    option(swipl_source(Dir), Options), !.
resolve_source(Dir, _) :-
    setting(source, Dir0),
    (   Dir0 == default
    ->  discover_source(Dir)
    ;   expand_file_name(Dir0, [Dir])
    ).

discover_source(Dir) :-
    current_prolog_flag(cmake_binary_directory, BinDir),
    directory_file_path(BinDir, 'CMakeCache.txt', CacheFile),
    phrase_from_file((string(_), "SWI-Prolog_SOURCE_DIR:STATIC=", !,
                      string(DirCodes), whites, eol, remainder(_)), CacheFile),
    !,
    atom_codes(Dir, DirCodes).

resolve_output(File, Options) :-
    option(output(File), Options), !.
resolve_output(File, _) :-
    absolute_file_name(swi('library/ext/pldoc/pldoc/changelog_events.pl'),
                       File,
                       [ access(write)
                       ]).

resolve_revisions(Range, Options) :-
    option(revisions(Range), Options), !.
resolve_revisions(Range, _) :-
    setting(default_start_version, V),
    atomic_list_concat([V, '..HEAD'], Range).

%!  next_version(+Dir, -N, +Options) is det.
%
%   Integer version one patch beyond the latest V<M>.<m>.<p> tag
%   reachable from HEAD.  Used for unreleased commits.

next_version(_Dir, N, Options) :-
    option(next_version(Tag), Options),
    !,
    version_int(Tag, N).
next_version(Dir, N, _) :-
    git(['for-each-ref',
         '--format=%(refname:short)',
         'refs/tags/V*'],
        [ directory(Dir), output(OutCodes), error(_), status(_) ]),
    string_codes(S, OutCodes),
    split_string(S, "\n", "", Lines),
    (   aggregate_all(max(Version),
                      ( member(Tag, Lines),
                        Tag \== "",
                        version_int(Tag, Version)
                      ),
                      Latest)
    ->  N is Latest+1
    ;   N = 1
    ).

%!  version_int(+Tag, -N) is semidet.
%
%   Convert a tag like 'V?10.1.9' into 10*10000 + 1*100 + 9 = 100109.
%   Fails for tags that do not parse as V<int>.<int>.<int>(...).

version_int(Tag, N) :-
    (   string_concat('V', S, Tag)
    ->  true
    ;   S = Tag
    ),
    split_string(S, ".", "", Parts),
    Parts = [_,_,_],                          % require Major.Minor.Patch
    parts_to_int(Parts, [10000, 100, 1], 0, N).

parts_to_int([], _, N, N).
parts_to_int([P|Ps], [M|Ms], A, N) :-
    number_string(K, P),
    integer(K),
    A1 is A + K*M,
    parts_to_int(Ps, Ms, A1, N).
parts_to_int([_|_], [], N, N).

%!  mine_repo(+Dir, +Range, +NextV, +Label, +Repo, -Events) is det.
%
%   Walk the git log of Dir in Range, emit changelog_event/7 terms for
%   ADDED/FIXED/MODIFIED/ENHANCED commits.  Label is a short prefix
%   shown in the progress message ('' for the main repo).  Repo is the
%   GitHub org/name (e.g. 'SWI-Prolog/swipl-devel') used as the 7th
%   field so the manual can link to the correct repo's commits.

mine_repo(Dir, Range, NextV, Label, Repo, Events) :-
    shortlog(Dir, Range, Log),
    length(Log, Total),
    include(interesting, Log, Cands),
    length(Cands, NC),
    (   Label == ''
    ->  print_message(informational, changelog(main_repo(Total, NC)))
    ;   print_message(informational, changelog(submodule(Label, Total, NC)))
    ),
    maplist(events_for_commit(Dir, NextV, Repo), Cands, Lists),
    append(Lists, Events).

%!  repo_of(+Dir, -Repo) is det.
%
%   Repo is an atom of the form `<Org>/<Name>` derived from the remote
%   origin URL of the git checkout at Dir.  Recognises both the
%   `https://github.com/Org/Name(.git)` and `git@github.com:Org/Name(.git)`
%   forms.  Returns the unparsed URL when the remote is not on GitHub,
%   and `unknown` when there is no remote.

repo_of(Dir, Repo) :-
    git(['config', '--get', 'remote.origin.url'],
        [ directory(Dir), output(OutCodes), error(_), status(Status) ]),
    (   Status == exit(0),
        string_codes(S, OutCodes),
        split_string(S, "", " \n\t", [Trim]),
        Trim \== ""
    ->  normalize_repo_url(Trim, Repo)
    ;   Repo = unknown
    ).

normalize_repo_url(URL, Repo) :-
    (   string_concat("https://github.com/", Rest, URL)
    ;   string_concat("git@github.com:", Rest, URL)
    ),
    !,
    (   string_concat(OrgName, ".git", Rest)
    ->  true
    ;   OrgName = Rest
    ),
    atom_string(Repo, OrgName).
normalize_repo_url(URL, Repo) :-
    atom_string(Repo, URL).

%!  shortlog(+Dir, +Range, -Log) is det.
%
%   Like git_shortlog/3, but falls back to the whole history if the
%   requested range's lower bound doesn't resolve in this repo - which
%   happens for submodules that didn't yet exist at the default start
%   version.  The check is done up front rather than via catch/3 so
%   git's stderr does not leak into the user's progress output.

shortlog(Dir, Range, Log) :-
    (   range_resolvable(Dir, Range)
    ->  git_shortlog(Dir, Log, [revisions(Range)])
    ;   git_shortlog(Dir, Log, [revisions('HEAD')])
    ).

range_resolvable(Dir, Range) :-
    (   sub_atom(Range, B, _, _, '..'),
        sub_atom(Range, 0, B, _, From),
        From \== ''
    ->  ref_resolves(Dir, From)
    ;   true
    ).

ref_resolves(Dir, Ref) :-
    git(['rev-parse', '--verify', '--quiet', Ref],
        [ directory(Dir), output(_), error(_), status(Status) ]),
    Status == exit(0).

%!  mine_submodules(+MainDir, +Range, +NextV, -Events) is det.

mine_submodules(MainDir, Range, NextV, Events) :-
    list_submodules(MainDir, Subs),
    length(Subs, NSubs),
    print_message(informational, changelog(submodule_count(NSubs))),
    maplist(mine_one_submodule(MainDir, Range, NextV), Subs, Lists),
    append(Lists, Events).

list_submodules(MainDir, Subs) :-
    git([submodule, foreach, '--quiet', 'echo $sm_path'],
        [ directory(MainDir), output(OutCodes), error(_), status(_) ]),
    string_codes(S, OutCodes),
    split_string(S, "\n", "", Lines),
    exclude(=(""), Lines, NE),
    maplist(atom_string, Subs, NE).

mine_one_submodule(MainDir, Range, NextV, SubRel, Events) :-
    directory_file_path(MainDir, SubRel, SubAbs),
    (   exists_directory(SubAbs),
        is_real_git_checkout(SubAbs)
    ->  repo_of(SubAbs, Repo),
        mine_repo(SubAbs, Range, NextV, SubRel, Repo, Events)
    ;   print_message(informational, changelog(submodule_skipped(SubRel))),
        Events = []
    ).

is_real_git_checkout(Dir) :-
    directory_file_path(Dir, '.git', GitDirOrFile),
    ( exists_directory(GitDirOrFile) ; exists_file(GitDirOrFile) ),
    !.

%!  interesting(+LogRec) is semidet.

interesting(LogRec) :-
    git_log_data(subject, LogRec, Subject),
    marker_type(Subject, _).

marker_type(Subject, added)    :- sub_atom(Subject, 0, _, _, 'ADDED:'), !.
marker_type(Subject, fixed)    :- sub_atom(Subject, 0, _, _, 'FIXED:'), !.
marker_type(Subject, modified) :- sub_atom(Subject, 0, _, _, 'MODIFIED:'), !.
marker_type(Subject, enhanced) :- sub_atom(Subject, 0, _, _, 'ENHANCED:').

events_for_commit(Dir, _NextV, _Repo, LogRec, []) :-
    git_log_data(commit_hash, LogRec, Hash),
    commit_only_in_dialect(Dir, Hash), !.
events_for_commit(Dir, NextV, Repo, LogRec, Events) :-
    git_log_data(subject, LogRec, Subject),
    git_log_data(commit_hash, LogRec, Hash),
    git_log_data(committer_date_unix, LogRec, Date),
    marker_type(Subject, Marker),
    indicators_for_commit(Marker, Dir, Hash, Subject, IndicatorTypes),
    (   IndicatorTypes == []
    ->  Events = []
    ;   describe_int(Dir, Hash, NextV, V),
        atom_string(Hash, HashStr),
        atom_string(Subject, SubjectStr),
        findall(changelog_event(I, T, V, Date, HashStr, SubjectStr, Repo),
                member(I-T, IndicatorTypes),
                Events)
    ).

%!  describe_int(+Dir, +Hash, +NextV, -N) is det.
%
%   Numeric version of the first V<M>.<m>.<p> tag containing Hash, or
%   NextV if no such tag yet contains it.

describe_int(Dir, Hash, NextV, N) :-
    git([describe, '--contains', '--match=V*', Hash],
        [ directory(Dir), output(OutCodes), error(_),
          status(Status)
        ]),
    (   Status == exit(0),
        parse_tag_int(OutCodes, N0)
    ->  N = N0
    ;   N = NextV
    ).

parse_tag_int(Codes, N) :-
    string_codes(S, Codes),
    split_string(S, "~^", " \n\t", [Tag|_]),
    atom_string(TagA, Tag),
    version_int(TagA, N).

%!  indicators_for_commit(+Marker, +Dir, +Hash, +Subject, -IndicatorTypes)
%!  is det.
%
%   IndicatorTypes is a list of  Indicator-Type   pairs.  For  an ADDED:
%   commit,  indicators  discovered  in  the   *diff*  (new  PRED_DEF  /
%   PRED_IMPL / PL_register_foreign in C,   new module-export entries in
%   Prolog, library(M) for a newly-added .pl   module)  are emitted with
%   type `introduced`; predicates mentioned  in   the  subject+body that
%   were NOT introduced by the diff are  emitted with type `added` (some
%   functionality was added to an  existing   predicate).  For the other
%   markers we use the subject+body scan with the matching type verbatim
%   (fixed, modified, enhanced).
%
%   Predicates whose name begins  with  `$`   or  `_`  are  filtered out
%   throughout.

indicators_for_commit(added, Dir, Hash, Subject, IndicatorTypes) :-
    !,
    added_predicates_in_commit(Dir, Hash, IntroducedRaw),
    exclude(hidden_pred, IntroducedRaw, Introduced),
    subject_primary_predicates(Subject, MentionedRaw),
    exclude(hidden_pred, MentionedRaw, Mentioned),
    subtract(Mentioned, Introduced, Added),
    maplist(tag(introduced), Introduced, L1),
    maplist(tag(added),      Added,      L2),
    append(L1, L2, IndicatorTypes).
indicators_for_commit(Marker, _Dir, _Hash, Subject, IndicatorTypes) :-
    subject_primary_predicates(Subject, Raw),
    exclude(hidden_pred, Raw, Mentioned),
    maplist(tag(Marker), Mentioned, IndicatorTypes).

tag(Tag, V, V-Tag).

%!  subject_primary_predicates(+Subject, -Preds) is det.
%
%   Extract the primary predicate cluster from a commit subject line.
%   Scan left-to-right until the first Name/Arity (or library(Name))
%   indicator, then keep collecting siblings as long as they are
%   joined by `,` or `and` — the conventional list connectors used in
%   subjects like "ADDED: foo/3, bar/4 and baz/5".  Stop at the first
%   non-indicator, non-connector token.  Anything in the commit body
%   is ignored entirely.
%
%   This deliberately drops references like "... using freeze/2 and
%   when/2 ..." or "Similar to same_functor/2" further down the line,
%   which are about pre-existing predicates rather than changes.

subject_primary_predicates(Subject, Preds) :-
    atom_codes(Subject, Codes),
    scan_subject(Codes, [], Preds).

scan_subject([], _, []) :-
    !.
scan_subject(Codes, Context, Preds) :-
    phrase(indicator(P), Codes, Rest),
    !,
    consumed_codes(Codes, Rest, PredCodes),
    append(Context, PredCodes, AccContext0),
    trim_recent(AccContext0, AccContext),
    (   bad_pred_context(Context)
    ->  scan_subject(Rest, AccContext, Preds)
    ;   phrase(sibling_indicators(Sibs), Rest, _),
        Preds = [P|Sibs]
    ).
scan_subject([C|Cs], Context, Preds) :-
    append(Context, [C], NewContext0),
    trim_recent(NewContext0, NewContext),
    scan_subject(Cs, NewContext, Preds).

consumed_codes(Codes, Rest, Consumed) :-
    length(Codes, L1),
    length(Rest, L2),
    K is L1 - L2,
    length(Consumed, K),
    append(Consumed, Rest, Codes).

trim_recent(Codes, Trimmed) :-
    length(Codes, L),
    (   L =< 40
    ->  Trimmed = Codes
    ;   K is L - 40,
        length(Drop, K),
        append(Drop, Trimmed, Codes)
    ).

%!  bad_pred_context(+Codes) is semidet.
%
%   True if the recent text immediately before the indicator candidate
%   makes the indicator a context reference rather than the subject's
%   primary topic.  Two cases: an unclosed backtick span (e.g. p/1
%   inside `:- dynamic(p/1, q/1).`), or a phrase like "instead of",
%   "via the", "to inject", "by using" -- the indicator is what's
%   being replaced, used, or invoked, not what's being changed.

bad_pred_context(Codes) :-
    inside_backticks(Codes),
    !.
bad_pred_context(Codes) :-
    string_codes(S, Codes),
    string_lower(S, SL),
    bad_ending(SL),
    !.

inside_backticks(Codes) :-
    include(==(0'`), Codes, Backticks),
    length(Backticks, Count),
    1 is Count mod 2.

bad_ending(S) :- string_at_end(S, "instead of using the ").
bad_ending(S) :- string_at_end(S, "instead of using ").
bad_ending(S) :- string_at_end(S, "instead of the ").
bad_ending(S) :- string_at_end(S, "instead of ").
bad_ending(S) :- string_at_end(S, "via the ").
bad_ending(S) :- string_at_end(S, "via ").
bad_ending(S) :- string_at_end(S, "i.e. via the ").
bad_ending(S) :- string_at_end(S, "i.e. via ").
bad_ending(S) :- string_at_end(S, "by using the ").
bad_ending(S) :- string_at_end(S, "by using ").
bad_ending(S) :- string_at_end(S, "to inject ").
bad_ending(S) :- string_at_end(S, "to call ").
bad_ending(S) :- string_at_end(S, "to invoke ").
bad_ending(S) :- string_at_end(S, "based on the ").
bad_ending(S) :- string_at_end(S, "based on ").

string_at_end(Long, Short) :-
    sub_string(Long, _, _, 0, Short).

sibling_indicators([P|Ps]) -->
    indicator_connector,
    indicator(P), !,
    sibling_indicators(Ps).
sibling_indicators([]) --> [].

indicator_connector -->
    whites, ",", whites.
indicator_connector -->
    ws_required, "and", ws_required.

indicator(Name/Arity) -->
    csym(Name), "/", integer(Arity),
    !.
indicator(library(L)) -->
    "library(", lib_chars(LC), ")",
    { LC \== [],
      atom_codes(L, LC)
    }.

ws_required --> white, whites.

lib_chars([C|T]) --> lib_char(C), !, lib_chars(T).
lib_chars([]) --> [].

lib_char(C) --> [C], { C = 0'/ }, !.
lib_char(C) --> [C], { code_type(C, csym) }.

hidden_pred(Name/_) :-
    atom(Name),
    sub_atom(Name, 0, 1, _, First),
    hidden_first_char(First).

hidden_first_char('$').
hidden_first_char('_').

added_predicates_in_commit(Dir, Hash, Indicators) :-
    changed_files_status(Dir, Hash, Files),
    maplist(added_indicators_in_file(Dir, Hash), Files, Lists),
    append(Lists, All),
    sort(All, Indicators).

%!  commit_only_in_dialect(+Dir, +Hash) is semidet.
%
%   True if every .c / .pl file the commit touches lives under
%   library/dialect/.  Such commits maintain emulation shims for other
%   Prolog systems and should not contribute events — even when the
%   subject mentions a predicate name, the predicate it refers to is
%   the dialect-specific re-implementation, not the SWI one.

commit_only_in_dialect(Dir, Hash) :-
    changed_files_status(Dir, Hash, Files),
    findall(F,
            ( member(_-F, Files),
              file_name_extension(_, Ext, F),
              memberchk(Ext, [c, pl])
            ),
            Code),
    Code \== [],
    forall(member(F, Code),
           sub_atom(F, 0, _, _, 'library/dialect/')).

%!  changed_files_status(+Dir, +Hash, -Files) is det.
%
%   Files is a list of Status-File pairs, where Status is one of
%   0'A | 0'M | 0'D | 0'R (added, modified, deleted, renamed) and File
%   is the (post-rename) path as an atom.

changed_files_status(Dir, Hash, Files) :-
    git([show, '--name-status', '--format=', Hash],
        [ directory(Dir), output(OutCodes), error(_), status(_) ]),
    string_codes(S, OutCodes),
    split_string(S, "\n", "", Lines),
    findall(Status-File,
            ( member(L, Lines), L \== "",
              split_string(L, "\t", "", [StStr|Rest]),
              Rest \== [],
              string_codes(StStr, [Status|_]),
              last(Rest, FileStr),
              atom_string(File, FileStr)
            ),
            Files).

predicate_bearing_file(F) :-
    file_name_extension(_, Ext, F),
    memberchk(Ext, [c, pl]),
    \+ ignored_path(F).

ignored_path(F) :- sub_atom(F, 0, _, _, 'tests/').
ignored_path(F) :- sub_atom(F, 0, _, _, 'test/').
ignored_path(F) :- sub_atom(F, 0, _, _, 'doc/').
ignored_path(F) :- sub_atom(F, 0, _, _, 'man/').
ignored_path(F) :- sub_atom(F, 0, _, _, 'cmake/').
ignored_path(F) :- sub_atom(F, 0, _, _, 'library/dialect/').

added_indicators_in_file(_, _, _-File, []) :-
    \+ predicate_bearing_file(File), !.
added_indicators_in_file(Dir, Hash, _-File, Preds) :-
    file_name_extension(_, c, File), !,
    c_added_predicates(Dir, Hash, File, Preds).
added_indicators_in_file(Dir, Hash, Status-File, Indicators) :-
    file_name_extension(_, pl, File), !,
    prolog_added_indicators(Dir, Hash, Status, File, Indicators).
added_indicators_in_file(_, _, _, []).

%!  prolog_added_indicators(+Dir, +Hash, +Status, +File, -Indicators) is det.
%
%   For a newly-added .pl file (Status == 0'A) we emit a library(M)
%   indicator for the module, plus an indicator for every export.  For
%   a modified file, only the *new* export entries are returned.  In
%   both cases the file's module must resolve via
%   absolute_file_name(library(M), ...) — otherwise the file is not
%   installed as part of the SWI-Prolog library (demos, examples,
%   internal helpers, ...) and contributes no events.

prolog_added_indicators(Dir, Hash, Status, File, Indicators) :-
    file_module_exports(Dir, Hash, File, MaybeModule, NewExports),
    file_base_name(File, BaseName),
    file_name_extension(LibName, _, BaseName),
    (   MaybeModule = module(_),
        is_installed_library(LibName)
    ->  (   Status == 0'A
        ->  Indicators = [library(LibName)|NewExports]
        ;   atom_concat(Hash, '^', ParentRef),
            file_exports(Dir, ParentRef, File, OldExports),
            subtract(NewExports, OldExports, Indicators)
        )
    ;   Indicators = []
    ).

%!  is_installed_library(+LibName) is semidet.
%
%   True if `library(LibName)` resolves to an installed source file in
%   the SWI-Prolog used to run update_changelog/0.  LibName is the
%   file's base name (as used in `library(Name)` references), not its
%   declared module name — those differ for e.g. tableutil.pl, whose
%   module is prolog_table_utils.

is_installed_library(LibName) :-
    absolute_file_name(library(LibName), _,
                       [ file_type(source),
                         file_errors(fail),
                         access(read)
                       ]).

%!  file_module_exports(+Dir, +Ref, +File, -MaybeModule, -Exports) is det.

file_module_exports(Dir, Ref, File, module(Module), Exports) :-
    catch(setup_call_cleanup(
              git_show_file(Dir, Ref, File, In),
              call_cleanup(xref_public_list(In, -,
                                            [ exports(AllExports),
                                              module(Module),
                                              silent(true)
                                            ]),
                           read_rest(In)),
              close(In)),
          error(_,_), fail),
    !,
    include(is_pi, AllExports, Exports).
file_module_exports(_Dir, _Ref, _File, none, []).

is_pi(Name/Arity), atom(Name), integer(Arity), between(0,20,Arity) => true.
is_pi(Name//Arity), atom(Name), integer(Arity), between(0,20,Arity) => true.
is_pi(_) => fail.

read_rest(In) :-
    setup_call_cleanup(
        open_null_stream(Null),
        copy_stream_data(In, Null),
        close(Null)).


c_added_predicates(Dir, Hash, File, Preds) :-
    git([show, '-p', '-U0', '--format=', Hash, '--', File],
        [ directory(Dir), output(OutCodes), error(_), status(_) ]),
    string_codes(S, OutCodes),
    split_string(S, "\n", "", Lines),
    diff_predicates(Lines, "+", "+++", AddedSet),
    diff_predicates(Lines, "-", "---", RemovedSet),
    subtract(AddedSet, RemovedSet, Preds).

diff_predicates(Lines, Prefix, FileHeader, Set) :-
    findall(P,
            ( member(L, Lines),
              sub_string(L, 0, 1, _, Prefix),
              \+ sub_string(L, 0, 3, _, FileHeader),
              string_codes(L, LC),
              phrase(find_c_pred(P), LC, _)
            ),
            Preds),
    sort(Preds, Set).

find_c_pred(Name/Arity) -->
    c_pred_call(Name, Arity), !.
find_c_pred(P) -->
    [_], find_c_pred(P).

c_pred_call(Name, Arity) -->
    c_macro_name, c_ws, "(", c_ws,
    "\"", c_string_chars(NameCodes), "\"", c_ws,
    ",", c_ws,
    c_digits([D|Ds]), c_ws,
    ",",
    { atom_codes(Name, NameCodes),
      number_codes(Arity, [D|Ds])
    }.

c_macro_name --> "PRED_DEF".
c_macro_name --> "PRED_IMPL".
c_macro_name --> "PL_register_foreign".

c_string_chars([C|T]) --> [C], { C \= 0'" }, !, c_string_chars(T).
c_string_chars([]) --> [].

c_ws --> [C], { code_type(C, white) }, !, c_ws.
c_ws --> [].

c_digits([D|T]) --> [D], { code_type(D, digit) }, !, c_digits(T).
c_digits([]) --> [].

file_exports(Dir, Ref, File, Exports) :-
    catch(setup_call_cleanup(
              git_show_file(Dir, Ref, File, In),
              call_cleanup(xref_public_list(In, -,
                                            [ exports(AllExports),
                                              silent(true)
                                            ]),
                           read_rest(In)),
              close(In)),
          error(_,_), fail),
    !,
    include(is_pi, AllExports, Exports).
file_exports(_Dir, _Ref, _File, []).

%!  git_show_file(+Dir, +Ref, +File, -Stream) is det.
%
%   Like library(git):git_open_file/4 but with stderr suppressed.

git_show_file(Dir, Ref, File, In) :-
    atomic_list_concat([Ref, :, File], RefSpec),
    process_create(path(git),
                   [ '-C', file(Dir), show, RefSpec ],
                   [ stdout(pipe(In)),
                     stderr(null)
                   ]),
    set_stream(In, encoding(utf8)).

%!  write_events_file(+Path, +Events) is det.

write_events_file(Path, Events) :-
    setup_call_cleanup(
        open(Path, write, Out, [encoding(utf8)]),
        write_events_(Out, Events),
        close(Out)).

write_events_(Out, Events) :-
    format(Out,
           '/*  Predicate-version database -- generated by update_changelog/0.~n\c
~n\c
~4|Each changelog_event/7 fact records when a predicate was added,~n\c
~4|fixed, modified or enhanced.  Version is encoded as~n\c
~4|Maj*10000 + Min*100 + Patch (the value of PLVERSION); for~n\c
~4|unreleased commits the version is one patch beyond the~n\c
~4|latest reachable tag.  Submodule commits are tagged with the~n\c
~4|version of the first main-repo tag whose submodule pointer~n\c
~4|contains them.  The 7th field is the GitHub `Org/Name` of the~n\c
~4|repository in which the commit lives, so the documentation~n\c
~4|renderer can link to the correct repo.~n\c
*/~n~n\c
:- module(changelog_events, [changelog_event/7]).~n~n',
           []),
    maplist(portray_clause(Out), Events).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(changelog(scanning(Dir, Range))) -->
    [ 'Scanning ~w (~w) ...'-[Dir, Range] ].
prolog:message(changelog(next_version(N))) -->
    [ 'Next-release version: ~d'-[N] ].
prolog:message(changelog(main_repo(Total, NC))) -->
    [ '  main: ~d commits, ~d marker commits'-[Total, NC] ].
prolog:message(changelog(submodule_count(N))) -->
    [ '  ~d submodule(s)'-[N] ].
prolog:message(changelog(submodule(Sub, Total, NC))) -->
    [ '    ~w: ~d commits, ~d marker commits'-[Sub, Total, NC] ].
prolog:message(changelog(submodule_skipped(Sub))) -->
    [ '    ~w: not initialised, skipping'-[Sub] ].
prolog:message(changelog(wrote(N, Path))) -->
    [ 'Wrote ~d event(s) to ~w'-[N, Path] ].
