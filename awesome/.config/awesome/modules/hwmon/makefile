CC = gcc
LIBS = -lsensors
CFLAGS = -Wall -Wstrict-prototypes -ansi -pedantic
LFLAGS = -Wall -shared -fPIC -DLUA_32BITS

IDIR = include
ODIR = obj

_DEPS = read_sensors.h
DEPS = $(addprefix $(IDIR)/,$(_DEPS))

_OBJ = reading.o main.o read_sensors.o
OBJ = $(addprefix $(ODIR)/,$(_OBJ))

_LDEPS = lua_functions.h
LDEPS = $(addprefix $(IDIR)/,$(_LDEPS))

_LOBJ = reading.o lua_functions.o
LOBJ = $(addprefix $(ODIR)/,$(_LOBJ))

$(ODIR)/%.o: src/%.c $(LDEPS)
	$(CC) -c -o $@ $< $(LFLAGS)

lua: $(LOBJ)
	$(CC) -o sensors.so $^ $(LFLAGS) $(LIBS)

$(ODIR)/%.o: src/%.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

read_sensors: $(OBJ)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -rf $(ODIR) read_sensors sensors.so

$(OBJ): | $(ODIR)

$(ODIR):
	mkdir -p obj
