ELM=elm
BUILDDIR="build/"
ELM_RUNTIME=`$(ELM) -g`

all : rfk

rfk : 
	$(ELM) --make -b $(BUILDDIR) rfk.elm
	cp $(ELM_RUNTIME) $(BUILDDIR)

clean :
	rm -f $(BUILDDIR)/elm-runtime.js
	rm -f $(BUILDDIR)/rfk.html

