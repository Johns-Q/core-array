#
#	@file core-array.mk	@brief core array module makefile.
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

#	include core-array/core-array.mk in the main application Makefile

OBJS+=	core-array/core-array.o
HDRS+=	core-array/core-array.h
FILES+=	core-array/core-array.mk

core-array/core-array.o: core-array/core-array.c

$(OBJS):core-array/core-array.mk

#----------------------------------------------------------------------------
#	Developer tools

.PHONY: core-array-clean core-array-clobber

core-array-clean:
	-rm core-array/core-array.o

clean:	core-array-clean

core-array-clobber:

clobber:	core-array-clobber
