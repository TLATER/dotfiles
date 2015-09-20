SC = stylus
SFLAGS =

SIDIR = src/include/css
SODIR = obj
_SOBJ = scrollbar.css

SOBJ = $(addprefix $(SODIR)/,$(_SOBJ))

$(SODIR)/%.css: src/css/%.styl
	$(SC) -I $(SIDIR) $(SFLAGS) < $^ >$@

extension: $(SOBJ)
	cp $^ css
