#
#	@file Makefile		@brief core general purpose array Makefile.
#
#	Copyright (c) 2010 by Lutz Sammer.  All Rights Reserved.
#
#	Contributor(s):
#
#	License: AGPLv3
#
#	This program is free software: you can redistribute it and/or modify
#	it under the terms of the GNU Affero General Public License as
#	published by the Free Software Foundation, either version 3 of the
#	License.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU Affero General Public License for more details.
#
#	$Id$
#----------------------------------------------------------------------------

VERSION := "1.00"
GIT_REV := $(shell git describe --always 2>/dev/null)

CC	:= gcc
#OPTIM	:= -march=native -O2 -fomit-frame-pointer
OPTIM	:= -march=native # -m32
CFLAGS	= $(OPTIM) -W -Wall -Wextra -g -pipe -DARRAY_TEST -DDEBUG_ARRAY\
	-DVERSION='$(VERSION)' $(if $(GIT_REV), -DGIT_REV='"$(GIT_REV)"')
#STATIC= --static
LIBS	= $(STATIC)

HDRS	:= core-array.h
OBJS	:= core-array.o
FILES	:= Makefile README.txt Changelog AGPL-3.0.txt core-array.doxyfile \
	core-array.h core-core.c

all:	array_test

array_test	: $(OBJS)
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^ $(LIBS)

$(OBJS)	: Makefile $(HDRS)

#----------------------------------------------------------------------------
#	Developer tools

doc:	$(SRCS) $(HDRS) core-array.doxyfile
	(cat core-array.doxyfile; \
	echo 'PROJECT_NUMBER=${VERSION} $(if $(GIT_REV), (GIT-$(GIT_REV)))') \
	| doxygen -

indent:
	for i in $(OBJS:.o=.c) $(HDRS); do \
		indent $$i; unexpand -a $$i > $$i.up; mv $$i.up $$i; \
	done

clean:
	-rm *.o *~

clobber:	clean
	-rm -rf array_test man www/html

dist:
	tar cjCf .. wmcpumon-`date +%F-%H`.tar.bz2 \
		$(addprefix core-array/, $(FILES) $(OBJS:.o=.c))

install:
	##strip --strip-unneeded -R .comment binary_file
	#install -s binary_file /usr/local/bin/

help:
	@echo "make all|doc|indent|clean|clobber|dist|install|help"
