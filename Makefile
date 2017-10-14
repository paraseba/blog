.PHONY: bin-clean site-clean clean exe shell site-build site-rebuild site-watch deploy configure build

CABAL = seba-blog.cabal
INSHELL = nix-shell release.nix -A blog-seba.env --run
SITEBIN = dist/build/site/site

SOURCE = site.hs
SITESOURCE = $(shell find -name '*.markdown' \
	-o -name '*.html' -o -name '*.ico' \
	-o -name '*.html' -o -name '*.ico' \
	-o -name '*.lhs' -o -name '*.css' \
	-o -name '*.jpg' -o -name '*.png')

S3BUCKET = s3://blog.sebastian-galkin.com
S3CMD = s3cmd --verbose --config ~/.s3cfg.blog.sebastian-galkin.com  \
      sync \
      --add-header="Cache-Control:max-age=3600" \
      --no-preserve \
      --delete-removed -M --no-mime-magic       \
      _site/ $(S3BUCKET)


# $(info $$SITESOURCE is [${SITESOURCE}])

site-watch: site-build
	$(SITEBIN) watch

default.nix: $(CABAL)
	cabal2nix . > default.nix

dist: default.nix release.nix
	$(INSHELL) 'cabal configure'

configure: dist

$(SITEBIN): dist $(SOURCE)
	$(INSHELL) 'cabal build'

exe: $(SITEBIN)
build: exe

site-build: $(SITEBIN) $(SITESOURCE)
	$(SITEBIN) build

site-rebuild: $(SITEBIN) $(SITESOURCE)
	$(SITEBIN) rebuild

deploy: site-rebuild
	$(S3CMD)


shell:
	nix-shell release.nix -A blog-seba.env

bin-clean:
	$(INSHELL) 'cabal clean'

site-clean: $(SITEBIN)
	$(INSHELL) '$(SITEBIN) clean'

clean: site-clean bin-clean
