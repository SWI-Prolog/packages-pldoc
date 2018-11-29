---++ [doclatex] Including PlDoc in a LaTeX document

The LaTeX backend aims at producing quality paper documentation as well
as integration of predicate description and Wiki files in LaTeX
documents such as articles and technical reports.  It is realised by
the library doc_latex.pl.

The best practice for using the LaTeX backend is yet to be established.
For now we anticipate processing a Wiki document saved in a .txt file
using doc_latex/3 to produce either a simple complete LaTeX document or
a partial document that is included into the the main document using the
LaTeX =|\input|= command. Typically, this is best established by writing
a _|Prolog Script|_ that generates the required LaTeX document and call
this from a _Makefile_. We give a simple example from PlDoc, creating
this section from the wiki-file latex.txt below.

==
:- use_module(library(doc_latex)).
:- [my_program].
==

We generate latex.tex from latex.txt using this Makefile fragment:

==
.SUFFIXES: .txt .tex

.txt.tex:
	swipl -f script.pl \
	    -g "doc_latex('$*.txt','$*.tex',[stand_alone(false)]),halt" \
	    -t "halt(1)"
==

### Predicate reference for the LaTeX backend {#pldoc-latex-predicates}

High-level access is provided by doc_latex/3, while more low level
access is provided by the remaining predicates. Generated LaTeX depends
on the style file =|pldoc.sty|=, which is a plain copy of =|pl.sty|=
from the SWI-Prolog manual sources. The installation installs
=|pldoc.sty|= in the =pldoc= subdirectory of the Prolog manual.

	* [[doc_latex/3]]
	* [[latex_for_file/3]]
	* [[latex_for_wiki_file/3]]
	* [[latex_for_predicates/3]]

