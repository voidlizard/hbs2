REV:=$(shell git rev-parse --short HEAD)

CHAPTERS:=intro.tex

define make_target
$(basename $(1))-$(REV)$(suffix $(1))
endef

all: hbs2-browser

.PHONY: all clean hbs2-browser publish publish-hbs2-browser

%.pdf: %.tex
	xelatex $<
	xelatex $<

hbs2-browser.pdf: hbs2-browser.tex $(CHAPTERS)
	xelatex hbs2-browser.tex
	xelatex hbs2-browser.tex

hbs2-browser: hbs2-browser.pdf


# publish-hbs2-git-doc: hbs2-git-doc-0.24.1.pdf
# 	$(eval TARGET := $(call make_target,$<))
# 	$(eval HASH := $(shell hbs2 metadata create --hash --auto $(TARGET)))
# 	@echo Updating $(HBS2GITDOCLWW) $(HASH)
# 	hbs2-peer lwwref update -v $(HASH) $(HBS2GITDOCLWW)

publish-hbs2-browser: hbs2-browser.pdf
	cp $< $(call make_target,$<)
	$(eval TARGET := $(call make_target,$<))
	$(eval HASH := $(shell hbs2 metadata create --hash --auto $(TARGET)))
	@echo Updating $(HBS2BROWSERDOCLWW) $(HASH) $(TARGET)
	hbs2-peer lwwref update -v $(HASH) $(HBS2BROWSERDOCLWW)

publish: publish-hbs2-browser

clean:
	rm -f *.aux *.log *.nav *.out *.snm *.vrb *.toc *.pdf

