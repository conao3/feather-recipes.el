
SSHKEY     := ~/.ssh/id_rsa

DATE       := $(shell date '+%Y_%m_%d')
DATEDETAIL := $(shell date '+%Y/%m/%d %H:%M:%S')

FETCHER    := melpa

##################################################

.PHONY: all commit log

all:

recipe: $(FETCHER:%=recipe-%.el)

feather-recipes-%.el: feather-recipes-%.json
	emacs --script feather-recipes.el $< $@

feather-recipes-melpa.json:
	curl -O https://melpa.org/archive.json
	mv archive.json $@

commit: $(SSHKEY)
	echo "Commit by Travis-CI (job $$TRAVIS_JOB_NUMBER at $(DATEDETAIL))" >> commit.log

	git remote -v
	git remote set-url origin git@github.com:conao3/feather-recipes.git

	git checkout master
	git add .
	git commit -m "Travis CI (job $$TRAVIS_JOB_NUMBER) [skip ci]"

	git push origin master

$(SSHKEY):
	openssl aes-256-cbc -K $$encrypted_875c55c1bd3d_key -iv $$encrypted_875c55c1bd3d_iv -in feather-recipes_rsa.enc -out ~/.ssh/id_rsa -d
	chmod 600 ~/.ssh/id_rsa
	git config --global user.name "conao3"
	git config --global user.email conao3@gmail.com

clean-recipe:
	-rm -rf $(FETCHER:%=recipe-%.el)
	-rm -rf $(FETCHER:%=recipe-%.json)
