
SSHKEY        := ~/.ssh/id_rsa

DATE          := $(shell date '+%Y_%m_%d')
DATEDETAIL    := $(shell date '+%Y/%m/%d %H:%M:%S')

PREFIX        := feather
SOURCE        := source
RECIPE        := recipes
DETAIL        := detail
LIST          := list

FETCHER       := lite melpa
SOURCES       := $(FETCHER:%=$(PREFIX)-$(SOURCE)-%.el)
RECIPES       := $(FETCHER:%=$(PREFIX)-$(RECIPE)-%.el)
DETAILS       := $(FETCHER:%=$(PREFIX)-$(DETAIL)-%.el)
RECIPES-L     := $(FETCHER:%=$(PREFIX)-$(RECIPE)-%-$(LIST).el)
DETAILS-L     := $(FETCHER:%=$(PREFIX)-$(DETAIL)-%-$(LIST).el)

EMACS         ?= emacs

##################################################

.PHONY: all commit

all: recipe

recipe: $(RECIPES) $(DETAILS) $(RECIPES-L) $(DETAILS-L)

##############################

$(PREFIX)-$(RECIPE)-%.el: $(PREFIX)-$(SOURCE)-%.json feather-recipes.el
	$(EMACS) --script feather-recipes.el $< $@ nil nil
	@echo

$(PREFIX)-$(DETAIL)-%.el: $(PREFIX)-$(SOURCE)-%.json feather-recipes.el
	$(EMACS) --script feather-recipes.el $< $@ detail nil
	@echo

$(PREFIX)-$(RECIPE)-%-$(LIST).el: $(PREFIX)-$(SOURCE)-%.json feather-recipes.el
	$(EMACS) --script feather-recipes.el $< $@ nil list
	@echo

$(PREFIX)-$(DETAIL)-%-$(LIST).el: $(PREFIX)-$(SOURCE)-%.json feather-recipes.el
	$(EMACS) --script feather-recipes.el $< $@ detail list
	@echo

##############################

$(PREFIX)-$(SOURCE)-melpa.json:
	curl -O https://melpa.org/archive.json
	mv archive.json $@

##############################

commit: $(SSHKEY)
	echo "Commit by Travis-CI (job $$TRAVIS_JOB_NUMBER at $(DATEDETAIL))" >> commit.log

	git remote -v
	git remote set-url origin git@github.com:conao3/feather-recipes.git

	git checkout master
	git checkout -b travis-$$TRAVIS_JOB_NUMBER
	git add .
	git commit -m "Travis CI (job $$TRAVIS_JOB_NUMBER)"

	git checkout master
	git merge --no-ff travis-$$TRAVIS_JOB_NUMBER -m "Merge travis-$$TRAVIS_JOB_NUMBER [skip ci]"
	git push origin master

$(SSHKEY):
	openssl aes-256-cbc -K $$encrypted_875c55c1bd3d_key -iv $$encrypted_875c55c1bd3d_iv -in feather-recipes_rsa.enc -out ~/.ssh/id_rsa -d
	chmod 600 ~/.ssh/id_rsa
	git config --global user.name "conao3"
	git config --global user.email conao3@gmail.com

##############################

clean:
	-rm -rf $(SOURCES)
	-rm -rf $(RECIPES) $(DETAILS)
	-rm -rf $(RECIPES-L) $(DETAILS-L)
