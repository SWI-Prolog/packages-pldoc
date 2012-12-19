/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(pldoc_pack,
	  [ doc_pack/1			% +Pack
	  ]).
:- use_module(library(prolog_pack)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(doc_html).
:- use_module(doc_index).

/** <module> Document Prolog extension packs
*/

:- http_handler(pldoc(packs), pldoc_packs, []).
:- http_handler(pldoc(pack),  pack_documentation, []).


pldoc_packs(_Request) :-
	reply_html_page(
	    pldoc(packs),
	    title('Installed extension packs'),
	    \pack_page([])).

pack_page(Options) -->
	html_requires(pldoc),
	object_page_header(-, Options),
	html([ h1('Installed extension packs'),
	       \pack_table(Options)
	     ]).


%%	pack_table(+Options)// is det.
%
%	Generate a table with installed packages

pack_table(_Options) -->
	{ findall(Pack, pack_property(Pack, directory(_)), Packs0),
	  sort(Packs0, Packs)
	},
	html(table(class(packs),
		   [ tr([th('Pack'), th('Title')])
		   | \packs(Packs)
		   ])).

packs([]) --> [].
packs([H|T]) --> pack(H), packs(T).

pack(Pack) -->
	{ http_link_to_id(pack_documentation, [pack=Pack], HREF),
	  (   pack_property(Pack, title(Title))
	  ->  true
	  ;   Title = '<no title>'
	  )
	},
	html(tr([ td(class(pack_name),  a(href(HREF), Pack)),
		  td(class(pack_title), Title)
		])).


%%	pack_documentation(+Request)
%
%	HTTP handler that creates a page   with  the PlDoc documentation
%	for an installed pack.

pack_documentation(Request) :-
	http_parameters(Request,
			[ pack(Pack, [])
			]),
	reply_html_page(
	    pldoc(pack),
	    title('Documentation for pack ~w'-[Pack]),
	    \pack_doc(Pack)).

pack_doc(Pack) -->
	{ pack_property(Pack, directory(PackDir)),
	  directory_file_path(PackDir, prolog, SourceDir),
	  pack_title(Pack, Title),
	  findall(O, pack_option(Pack, O), Options)
	},
	html(h1(Title)),
	dir_index(SourceDir,
		  [ if(true),
		    recursive(true),
		    Options
		  ]).


		 /*******************************
		 *	  STAND ALONE DOCS	*
		 *******************************/

%%	doc_pack(+Pack)
%
%	Generate stand-alone documentation for the package Pack.

doc_pack(Pack) :-
	pack_property(Pack, directory(PackDir)),
	pack_title(Pack, PackTitle),
	findall(O, pack_option(Pack, O), Options),
	directory_file_path(PackDir, prolog, SourceDir),
	directory_file_path(PackDir, doc, DocDir),
	doc_save(SourceDir,
		 [ title(PackTitle),
		   doc_root(DocDir),
		   if(true),
		   recursive(true)
		 | Options
		 ]).

pack_title(Pack, PackTitle) :-
	pack_property(Pack, title(Title)), !,
	format(atom(PackTitle), 'Pack ~w -- ~w', [Pack, Title]).
pack_title(Pack, PackTitle) :-
	format(atom(PackTitle), 'Pack ~w', [Pack]).

pack_option(Pack, Option) :-
	pack_option(Option),
	pack_property(Pack, Option).

pack_option(readme(_)).
pack_option(todo(_)).
