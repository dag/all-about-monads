toc := $(shell <html/index.html perl -ne '/<li>.+href="(.+\.html)"/ && print "html/$$1\n"')

.PHONY: all-about-monads.mediawiki
all-about-monads.mediawiki: ${toc}
	pandoc -f html -t native ${toc} \
	    | runhaskell transform.hs \
	    | pandoc -f native -t mediawiki -o $@ \
		-B before.mediawiki -A after.mediawiki

cabal-dev-all-about-monads.mediawiki: ${toc}
	cabal-dev/bin/pandoc -f html -t native ${toc} \
	    | cabal-dev/bin/all-about-monads \
	    | cabal-dev/bin/pandoc -f native -t mediawiki -o $@ \
		-B before.mediawiki -A after.mediawiki

cabal-all-about-monads.mediawiki: ${toc}
	pandoc -f html -t native ${toc} \
	    | dist/build/all-about-monads/all-about-monads \
	    | pandoc -f native -t mediawiki -o $@ \
		-B before.mediawiki -A after.mediawiki

cabal-dev-all-about-monads:
	cabal-dev install

cabal-all-about-monads:
	cabal install
