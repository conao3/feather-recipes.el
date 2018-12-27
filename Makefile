
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

commit: $(SSHKEY)
	echo "Commit by Travis-CI (job $$TRAVIS_JOB_NUMBER at $(DATEDETAIL))" >> commit.log

	git remote -v
	git remote set-url origin git@github.com:conao3/feather-recipes.git

	git checkout master
	git checkout -b travis-$$TRAVIS_JOB_NUMBER
	git add .
	git commit -m "Travis CI (job $$TRAVIS_JOB_NUMBER) [skip ci]"

	git checkout master
	git merge --no-ff travis-$$TRAVIS_JOB_NUMBER -m "Merge travis-$$TRAVIS_JOB_NUMBER [skip ci]"
	git push origin master

$(SSHKEY):
	openssl aes-256-cbc -K $$encrypted_875c55c1bd3d_key -iv $$encrypted_875c55c1bd3d_iv -in feather-recipes_rsa.enc -out ~/.ssh/id_rsa -d
	chmod 600 ~/.ssh/id_rsa
	git config --global user.name "conao3"
	git config --global user.email conao3@gmail.com

##############################

clean-v:
	@echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs -n1

clean:
	-echo $(SOURCES) $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)\
	  | xargs rm
