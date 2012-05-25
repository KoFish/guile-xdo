CC	:= gcc
CFLAGS := -fextended-identifiers -Wall -O2 `pkg-config --cflags guile-2.2`
LIBS := `pkg-config --libs guile-2.2` -lxdo

%.o: %.c
	$(CC) $(CFLAGS) -fpic -c $< -o $@ 

MODULES = xdo_guile.o
TARGET = libxdo_guile.so
OTHER = xdo_guile_smobs.h

$(TARGET): $(MODULES) $(OTHER)
	$(CC) $(LIBS) -shared $< -o $@

clean:
	rm -rf $(MODULES) $(TARGET)
