cmake_minimum_required(VERSION 3.10)
project(swipl-pldoc)

include("../cmake/PrologPackage.cmake")

set(PLDOC_PUBL pldoc.pl doc_latex.pl doc_files.pl doc_markdown.pl)
if(MULTI_THREADED)
  list(APPEND PLDOC_PUBL doc_http.pl)
endif()
set(PLDOC_PRIV doc_html.pl doc_wiki.pl doc_modes.pl doc_register.pl
    doc_process.pl doc_index.pl doc_search.pl doc_man.pl  doc_library.pl
    hooks.pl doc_htmlsrc.pl doc_colour.pl  doc_util.pl doc_access.pl
    doc_pack.pl man_index.pl doc_words.pl)
set(SUPPORT pldoc.css pldoc.js pllisting.css pldoc.sty edit.png
    private.png public.png reload.png favicon.ico up.gif source.png
    h1-bg.png pub-bg.png multi-bg.png priv-bg.png h2-bg.png editpred.png)

if(CHANGELOG)
  list(APPEND PLDOC_PRIV git_extract_changes.pl doc_changes.pl)
  has_package(xpce has_xpce)
  if(has_xpce)
    set(changelog_deps xpce-predicate-index)
  endif()
  add_swipl_target(changelog
		   OUTPUT ${SWIPL_BUILD_HOME}/library/ext/pldoc/pldoc/changelog_events.pl
		   LIBS pldoc/git_extract_changes
		   COMMAND "git_extract_changelog"
		   DEPENDS library_index ${changelog_deps}
		   QUIET
		   COMMENT "-- Machine readable changelog")
endif()

swipl_plugin(pldoc
	     PL_LIBS ${PLDOC_PUBL}
	     PL_LIB_SUBDIR pldoc
	         NOINDEX
	         PL_LIBS ${PLDOC_PRIV} ${SUPPORT})

swipl_examples(server/README server/man_server.pl)

pkg_doc(pldoc
	SOURCE latex.md --lib=doc_latex
	SUBSECTION
	    doc_files.pl)
