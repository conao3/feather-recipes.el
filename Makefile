
SSHKEY        := ~/.ssh/id_rsa

DATE          := $(shell date '+%Y_%m_%d')
DATEDETAIL    := $(shell date '+%Y/%m/%d %H:%M:%S')

SOURCEDIR     := sources
SCRIPTDIR     := scripts
RECIPEDIR     := recipes
DETAILDIR     := detail

RECIPE        := recipes
DETAIL        := detail
LIST          := list

FETCHER       := lite melpa melpa_stable

SOURCES       := $(FETCHER:%=$(SOURCEDIR)/%.json)

RECIPES       := $(FETCHER:%=$(RECIPEDIR)/%.el)
RECIPES-L     := $(FETCHER:%=$(RECIPEDIR)/%-$(LIST).el)

DETAILS       := $(FETCHER:%=$(DETAILDIR)/%.el)
DETAILS-L     := $(FETCHER:%=$(DETAILDIR)/%-$(LIST).el)

EMACS         ?= emacs
EVALEL        := feather-recipes.el

##################################################

.PHONY: all commit

all: sources recipes recipes-list

sources: $(SOURCES)
recipes: $(RECIPES) $(DETAILS)
recipes-list: $(RECIPES-L) $(DETAILS-L)

##############################

$(RECIPEDIR)/%.el:         $(SOURCEDIR)/%.json $(EVALEL)
	$(EMACS) --script $(EVALEL) $< $@ nil nil

$(RECIPEDIR)/%-$(LIST).el: $(SOURCEDIR)/%.json $(EVALEL)
	$(EMACS) --script $(EVALEL) $< $@ nil list

$(DETAILDIR)/%.el:         $(SOURCEDIR)/%.json $(EVALEL)
	$(EMACS) --script $(EVALEL) $< $@ detail nil

$(DETAILDIR)/%-$(LIST).el: $(SOURCEDIR)/%.json $(EVALEL)
	$(EMACS) --script $(EVALEL) $< $@ detail list

##############################

$(SOURCEDIR)/%.json: $(SCRIPTDIR)/create-%-json.rb
	ruby $(SCRIPTDIR)/create-$*-json.rb > $@

##############################

commit:
	echo "$(shell git diff --stat | tail -n1) (job $$TRAVIS_JOB_NUMBER at $(DATEDETAIL))" >> commit.log
	git add .
	git commit -m "Travis CI (job $$TRAVIS_JOB_NUMBER) [skip ci]"

merge:
	git checkout master
	git merge --no-ff travis-$$TRAVIS_JOB_NUMBER -m "Merge travis-$$TRAVIS_JOB_NUMBER [skip ci]"
	git push origin master

##############################

clean-v:
	@echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs -n1

clean:
	-echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs rm
