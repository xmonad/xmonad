.PHONY: all
all: xmonad.1 xmonad.1.html

xmonad.1.markdown: xmonad.1.markdown.in
	(cd .. && util/GenerateManpage.hs) <$< >$@

xmonad.1: xmonad.1.markdown
	pandoc --from=markdown --to=man --standalone --output=$@ $<

xmonad.1.html: xmonad.1.markdown
	pandoc --from=markdown --to=html --standalone --table-of-contents --output=$@ $<
