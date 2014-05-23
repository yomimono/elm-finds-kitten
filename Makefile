ELM=elm
ELM_RUNTIME=`$(ELM) -g`
BUILDDIR="build/"

all : rfk

rfk : 
	$(ELM) --make --set-runtime=elm-runtime.js -b $(BUILDDIR) rfk.elm
	cp $(ELM_RUNTIME) $(BUILDDIR)

clean :
	rm -f $(BUILDDIR)/elm-runtime.js
	rm -f $(BUILDDIR)/rfk.html

