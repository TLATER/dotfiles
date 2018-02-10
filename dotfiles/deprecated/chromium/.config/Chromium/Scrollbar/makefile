SC = stylus
SFLAGS =

SIDIR = src/include/css
SODIR = obj/css
_SOBJ = scrollbar.css

SOBJ = $(addprefix $(SODIR)/,$(_SOBJ))

$(SODIR)/%.css: src/css/%.styl
	$(SC) -I $(SIDIR) $(SFLAGS) < $^ > $@

JIDIR = src/include/js
JODIR = obj/js
_JOBJ = scrollbar.js

JOBJ = $(addprefix $(JODIR)/,$(_JOBJ))

$(JODIR)/%.js: src/js/%.js
	cp $^ $@

EODIR = obj
_EOBJ = manifest.json

EOBJ = $(addprefix $(EODIR)/,$(_EOBJ))

$(EODIR)/%: src/%
	cp $^ $@

extension: $(SOBJ) $(JOBJ) $(EOBJ)
	cp -r obj/* extension/
