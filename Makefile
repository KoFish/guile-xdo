CC	:= gcc
CFLAGS := -fextended-identifiers -Wall -O2 `guile-config compile`
LIBS := `guile-config link` -lxdo
SITEPATH := `guile-config info sitedir`
LIBPATH := `guile-config info prefix`/lib

SCMMODULE = libxdo.scm
OBJECTS = xdo_guile.o
TARGET = libxdo_guile.so
OTHER = xdo_guile_smobs.h

$(TARGET): $(OBJECTS) $(OTHER)
	$(CC) $(LIBS) -shared $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -fpic -c $< -o $@ 

.PHONY: clean
clean:
	rm -rf $(OBJECTS) $(TARGET)

.PHONY: install
install: $(TARGET)
	install -d $(SITEPATH)/xdo
	install $(SCMMODULE) $(SITEPATH)/xdo/$(SCMMODULE)
	install $(TARGET) $(LIBPATH)/$(TARGET)
	ldconfig

.PHONY: uninstall
uninstall:
	rm -f $(SITEPATH)/xdo/$(SCMMODULE)
	rmdir $(SITEPATH)/xdo
	rm -f $(LIBPATH)/$(TARGET)
