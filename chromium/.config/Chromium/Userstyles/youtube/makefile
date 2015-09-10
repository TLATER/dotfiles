SC = stylus
ODIR = obj

FLAGS =

_OBJ = youtube_main.css youtube_subscriptions.css youtube_playerpage.css\
       youtube_player.css youtube_comments.css
OBJ = $(addprefix $(ODIR)/,$(_OBJ))

$(ODIR)/%.css: src/%.styl
	$(SC) $(FLAGS) < $^ > $@

youtube.css: $(OBJ)
	cat $^ > $@

.PHONY: clean

clean:
	rm youtube.css obj/*
