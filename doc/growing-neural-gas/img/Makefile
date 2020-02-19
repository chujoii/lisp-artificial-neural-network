CC      = gcc -std=gnu99
# march:
# gcc -c -Q -march=native --help=target
# so for beagle correct: -march=armv7-a
# or auto:               -march=native
#
# pipe: has no effect on the generated code,
#       but it makes the compilation process faster
CFLAGS = -Wall -Wextra -pedantic -O2 -pipe -D_GNU_SOURCE

UNAME := $(shell uname -a)
ifneq ($(filter x86%,$(UNAME)),)
	CFLAGS += -march=native
endif
ifneq ($(filter armv7l,$(UNAME)),)
	CFLAGS += -march=armv7-a -mfpu=vfpv3-d16 -mfloat-abi=hard
endif

# Berkeley DB
DB	= -ldb
#DB	= -l:libdb-4.4.so

all : clusterizator


clusterizator :
	# Makefile_basename != GNU_basename
	for i in $(wildcard *.gv) ; do \
		neato -Teps -O $$i ; \
	done
	rename .gv.eps .eps *.gv.eps

# fixme: strange error with path: Package pdftex.def Error: File `../img/01-eps-converted-to.pdf' not found: using draft setting
install :
	mv *.eps ../ru/

# uninstall distribution
#uninstall :

# clean directory
clean :
	rm *.o unit-testing example-gng gng-to-dot_unit-testing

