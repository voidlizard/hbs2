all: hbs2-git-problem hbs2-git-new-repo hbs2-git-doc

.PHONY: all clean

%.pdf: %.tex
	xelatex $<
	xelatex $<

hbs2-git-problem: hbs2-git-problem.pdf

hbs2-git-new-repo: hbs2-git-new-repo.pdf

hbs2-git-doc: hbs2-git-doc-0.24.1.pdf

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.vrb *.toc *.pdf

