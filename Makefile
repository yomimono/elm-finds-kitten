ELM=elm
BUILDDIR="build/"
ELM_RUNTIME=~/.cabal/share/Elm-0.12/elm-runtime.js

all : rfk

rfk : 
	$(ELM) --make -r "elm-runtime.js" -b $(BUILDDIR) rfk.elm
	cp $(ELM_RUNTIME) $(BUILDDIR)

clean :
	rm -f $(BUILDDIR)/elm-runtime.js
	rm -f $(BUILDDIR)/rfk.html

