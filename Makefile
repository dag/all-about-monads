toc := $(shell <html/index.html perl -ne '/<li>.+href="(.+\.html)"/ && print "html/$$1\n"')

.PHONY: all-about-monads.mediawiki
all-about-monads.mediawiki: ${toc}
	pandoc -f html -t native ${toc} \
	    | runhaskell transform.hs \
	    | pandoc -f native -t mediawiki -o $@ \
		-B before.mediawiki -A after.mediawiki
