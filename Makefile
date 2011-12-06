toc := $(shell <html/index.html perl -ne '/<li>.+href="(.+\.html)"/ && print "html/$$1\n"')

all-about-monads.mediawiki: ${toc}
	pandoc -f html -t mediawiki -o $@ ${toc}
