REV:=$(shell git rev-parse --short HEAD)

define make_target
$(basename $(1))-$(REV)$(suffix $(1))
endef


all: hbs2-git-problem hbs2-git-new-repo hbs2-git-doc

.PHONY: all clean

%.pdf: %.tex
	xelatex $<
	xelatex $<
	cp $@ $(call make_target,$@)

hbs2-git-problem: hbs2-git-problem.pdf

hbs2-git-new-repo: hbs2-git-new-repo.pdf

hbs2-git-doc: hbs2-git-doc-0.24.1.pdf

publish-hbs2-git-doc: hbs2-git-doc-0.24.1.pdf
	$(eval TARGET := $(call make_target,$<))
	hbs2 metadata create --hash --auto $(TARGET)

publish: publish-hbs2-git-doc

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.vrb *.toc *.pdf

