cmake_minimum_required(VERSION 2.8.12)
project(swipl-pldoc)

include("../cmake/PrologPackage.cmake")

set(PLDOC_PUBL pldoc.pl doc_http.pl doc_latex.pl doc_files.pl)
set(PLDOC_PRIV doc_html.pl doc_wiki.pl doc_modes.pl doc_register.pl
    doc_process.pl doc_index.pl doc_search.pl doc_man.pl  doc_library.pl
    hooks.pl doc_htmlsrc.pl doc_colour.pl  doc_util.pl doc_access.pl
    doc_pack.pl man_index.pl)
set(SUPPORT pldoc.css pldoc.js pllisting.css pldoc.sty edit.png
    private.png public.png reload.png favicon.ico up.gif source.png
    h1-bg.png pub-bg.png multi-bg.png priv-bg.png h2-bg.png editpred.png)

swipl_plugin(pldoc
	     PL_LIBS ${PLDOC_PUBL}
	     PL_LIB_SUBDIR pldoc
	         PL_LIBS ${PLDOC_PRIV} ${SUPPORT})

swipl_examples(server/README server/man_server.pl)

pkg_doc(pldoc
	SOURCES latex.txt
	SUBSECTION
	    doc_files.pl)
