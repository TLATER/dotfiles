SC = stylus
IDIR = ../include
ODIR = obj

FLAGS =

_OBJ = youtube_main.css youtube_subscriptions.css youtube_playerpage.css\
       youtube_player.css youtube_comments.css youtube_home.css\
       youtube_history.css
OBJ = $(addprefix $(ODIR)/,$(_OBJ))

$(ODIR)/%.css: src/%.styl
	$(SC) -I $(IDIR) $(FLAGS) < $^ > $@

youtube.css: $(OBJ)
	cat $^ > $@

.PHONY: clean

clean:
	rm youtube.css obj/*
