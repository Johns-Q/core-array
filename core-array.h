///
///	@file core-array.h	@brief core array functions header file
///
///	Copyright (c) 2009, 2010 by Lutz Sammer.  All Rights Reserved.
///
///	Contributor(s):
///
///	License: AGPLv3
///
///	This program is free software: you can redistribute it and/or modify
///	it under the terms of the GNU Affero General Public License as
///	published by the Free Software Foundation, either version 3 of the
///	License.
///
///	This program is distributed in the hope that it will be useful,
///	but WITHOUT ANY WARRANTY; without even the implied warranty of
///	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
///	GNU Affero General Public License for more details.
///
///	$Id$
//////////////////////////////////////////////////////////////////////////////

/// @addtogroup CoreArray
/// @{

//////////////////////////////////////////////////////////////////////////////
//	Declares
//////////////////////////////////////////////////////////////////////////////

/**
**	Array typedef.
*/
typedef union _array_ Array;

    /// Error result (sorry, C has problems with const)
#define ArrayError ((const size_t *) ~ 0UL)

//////////////////////////////////////////////////////////////////////////////
//	Prototypes
//////////////////////////////////////////////////////////////////////////////

    /// New array
static inline Array *ArrayNew(void)
{
    return NULL;
}

    /// Insert index/value into array
extern size_t *ArrayIns(Array **, size_t, size_t);

    /// Delete index from array
extern size_t ArrayDel(Array **, size_t);

    /// Lookup index in array
extern size_t *ArrayIdx(const Array *, size_t);

    /// Get index in array
extern size_t ArrayGet(const Array *, size_t);

    /// Lookup nth index in array
extern size_t ArrayNth(const Array *, size_t);

    /// First index/value in array.
extern size_t *ArrayFirst(const Array *, size_t *);

    /// Next index/value in array.
extern size_t *ArrayNext(const Array *, size_t *);

    /// Last index/value in array.
extern size_t *ArrayLast(const Array *, size_t *);

    /// Previous index/value in array.
extern size_t *ArrayPrev(const Array *, size_t *);

    /// First empty index/value in array.
extern size_t *ArrayFirstEmpty(const Array *, size_t *);

    /// Next empty index/value in array.
extern size_t *ArrayNextEmpty(const Array *, size_t *);

    /// Last empty index/value in array.
extern size_t *ArrayLastEmpty(const Array *, size_t *);

    /// Previous empty index/value in array.
extern size_t *ArrayPrevEmpty(const Array *, size_t *);

    /// Free entire array.
extern void ArrayFree(Array *);

    /// Dump array.
extern void ArrayDump(FILE *, const Array *, int);

    /// Get number of indexes between the values in array.
extern size_t ArrayCount(const Array *, size_t, size_t);

    /// Get memory used by array.
extern size_t ArrayMemUsed(const Array *);

/// @}
