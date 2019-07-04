
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

DOCKER := docker run --rm -v $$(pwd)/:/.make
EMACS  ?= conao3/emacs:alpine-min-26.2

EVALEL := feather-recipes.el

##################################################

.PHONY: all commit

all: sources recipes recipes-list

sources: $(SOURCES)
recipes: $(RECIPES) $(DETAILS)
recipes-list: $(RECIPES-L) $(DETAILS-L)

##############################

$(SOURCEDIR)/%.json: $(SCRIPTDIR)/create-%-json.rb
	ruby $(SCRIPTDIR)/create-$*-json.rb > $@

##############################

$(RECIPEDIR)/%.el:         $(SOURCEDIR)/%.json $(EVALEL)
	$(DOCKER) $(EMACS) emacs --script /.make/$(EVALEL) $< $@ nil nil

$(RECIPEDIR)/%-$(LIST).el: $(SOURCEDIR)/%.json $(EVALEL)
	$(DOCKER) $(EMACS) emacs --script /.make/$(EVALEL) $< $@ nil list

$(DETAILDIR)/%.el:         $(SOURCEDIR)/%.json $(EVALEL)
	$(DOCKER) $(EMACS) emacs --script /.make/$(EVALEL) $< $@ detail nil

$(DETAILDIR)/%-$(LIST).el: $(SOURCEDIR)/%.json $(EVALEL)
	$(DOCKER) $(EMACS) emacs --script /.make/$(EVALEL) $< $@ detail list

##############################

checkout:
	git checkout master
	git checkout -b travis-$$TRAVIS_JOB_NUMBER
	echo "job $$TRAVIS_JOB_NUMBER at $(DATEDETAIL)" >> commit.log

commit: commit-source commit-recipes commit-list-recipes
commit-source:
	git add $(SOURCES)
	git diff --cached --stat | tail -n1 >> commit.log
	git add commit.log
	git commit --allow-empty -m "update source (job $$TRAVIS_JOB_NUMBER) [skip ci]"

commit-recipes:
	git add $(RECIPES) $(DETAILS)
	git diff --cached --stat | tail -n1 >> commit.log
	git add commit.log
	git commit --allow-empty -m "generate recipes (job $$TRAVIS_JOB_NUMBER) [skip ci]"

commit-list-recipes:
	git add $(RECIPES-L) $(DETAILS-L)
	git diff --cached --stat | tail -n1 >> commit.log
	git add commit.log
	git commit --allow-empty -m "generate list recipes (job $$TRAVIS_JOB_NUMBER) [skip ci]"

merge:
	git checkout master
	git merge --no-ff travis-$$TRAVIS_JOB_NUMBER -m "merge travis-$$TRAVIS_JOB_NUMBER [skip ci]"

push:
	git push origin master

##############################

clean-v:
	@echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs -n1

clean:
	-echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs rm
