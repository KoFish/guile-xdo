CC	:= gcc
CFLAGS := -fextended-identifiers -Wall -O2 `guile-config compile`
LIBS := `guile-config link` -lxdo
PREFIX := `guile-config info prefix`
SITEPATH := `guile-config info sitedir`
LIBPATH := $(PREFIX)/lib

SCMMODULE = libxdo.scm
MODULES = xdo_guile.o
TARGET = libxdo_guile.so
OTHER = xdo_guile_smobs.h

$(TARGET): $(MODULES) $(OTHER)
	$(CC) $(LIBS) -shared $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -fpic -c $< -o $@ 

.PHONY: clean
clean:
	rm -rf $(MODULES) $(TARGET)

.PHONY: install
install: $(TARGET)
	install -d $(SITEPATH)/xdo
	install $(SCMMODULE) $(SITEPATH)/xdo/$(SCMMODULE)
	install $(TARGET) $(LIBPATH)/$(TARGET)

.PHONY: uninstall
uninstall:
	rm -f $(SITEPATH)/xdo/$(SCMMODULE)
	rmdir $(SITEPATH)/xdo
	rm -f $(LIBPATH)/$(TARGET)

