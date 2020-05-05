cmake_minimum_required(VERSION 3.5)
project(swipl-pldoc)

include("../cmake/PrologPackage.cmake")

set(PLDOC_PUBL pldoc.pl doc_http.pl doc_latex.pl doc_files.pl)
set(PLDOC_PRIV doc_html.pl doc_wiki.pl doc_modes.pl doc_register.pl
    doc_process.pl doc_index.pl doc_search.pl doc_man.pl  doc_library.pl
    hooks.pl doc_htmlsrc.pl doc_colour.pl  doc_util.pl doc_access.pl
    doc_pack.pl man_index.pl doc_words.pl)
set(SUPPORT pldoc.css pldoc.js pllisting.css pldoc.sty edit.png
    private.png public.png reload.png favicon.ico up.gif source.png
    h1-bg.png pub-bg.png multi-bg.png priv-bg.png h2-bg.png editpred.png)

swipl_plugin(pldoc
	     NOINDEX
	     PL_LIBS ${PLDOC_PUBL}
	     PL_LIB_SUBDIR pldoc
	         PL_LIBS ${PLDOC_PRIV} ${SUPPORT})

set(SWIPL_QLF_BASE ${SWIPL_BUILD_HOME}/library/pldoc)
add_qcompile_target(doc_html
		    PRELOAD "lib:pldoc"
		    DEPENDS clib sgml http nlp)

swipl_examples(server/README server/man_server.pl)

pkg_doc(pldoc
	SOURCE latex.md --lib=doc_latex
	SUBSECTION
	    doc_files.pl)
