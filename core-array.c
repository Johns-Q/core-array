///
///	@file core-array.c	@brief core array functions
///
///	Copyright (c) 2009, 2010, 2015, 2016 by Lutz Sammer.
///		All Rights Reserved.
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

///
///	@defgroup CoreArray The core array module.
///
///	This module handles array functions.  Array are associative array
///	with integer keys.
///	
///
///	Design goal:
///		- small memory use
///		- fast access
///		- sorted access (integer keys)
///
///	@todo this isn't the latest version, the new version is rewritten,
///	but less tested.
///
/// @{

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>

#include "core-array.h"

#ifndef NO_ARRAY_DEBUG
#define NO_ARRAY_DEBUG			///< debug enabled/disabled
#endif

/*

Sizes:
1:	2+4/4*1 10(16)	2+8/8*1	18(32)
2:	2+4/4*2 18(24)	2+8/8*2	34(48)

level0:	8
level1:	7 + 2
level2:	6 + 1 + 2
level3:	5 + 1 + 1 + 2

KEYLEN Reduce 1

Pure node
    256 * Value 32/64 bit	1k/2k
Hybrid node
    128 * Key/Value

NULL	is empty node

inserts
[key-0] 
[val-0]

Bucket:
	1 - 128		key+values
	8 - 1 bytes key

[ID] [KEYS] [VALS]

Insert
Delete
Get
Count
ByCount
FreeArray
MemUsed
First
Next
Last
Prev
FirstEmpty
NextEmpty
LastEmpty
PrevEmpty

Node
	has leafs
Leaf	
	has no leafs
pure
	reduces prefix
hybrid
	keeps prefix
*/

/**
**	define our word size.
*/
#if SIZE_MAX == (18446744073709551615UL)
#define ARRAY64
#elif SIZE_MAX == (4294967295U)
#define ARRAY32
#else
#error the only supported platforms are 32bit or 64bit
#endif

#if 0

/**
**	I could use ArrayWordType, but i prefer just using standard types.
*/
typedef size_t ArrayWordType;
#endif

/**
**	Array kind enumeration typedef.
*/
typedef enum
{
    ArrayNode,				///< bucket is node
    ArrayKeylen1,			///< key length is 1
    ArrayKeylen2,			///< key length is 2
    ArrayKeylen3,			///< key length is 3
    ArrayKeylen4,			///< key length is 4
    ArrayKeylen5,			///< key length is 5
    ArrayKeylen6,			///< key length is 6
    ArrayKeylen7,			///< key length is 7
    ArrayKeylen8,			///< key length is 8
    ArrayLeaf = 16,			///< bucket is leaf
} ArrayKind;

#ifdef ARRAY64
// 1 + 8 + 8 => 17
struct _bucket_b8k1_
{
    uint8_t Count;			///< bucket number of elements - 1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[1][8];			///< 8 byte * 1 key
    size_t Val[1];			///< 1 value
} __attribute__ ((packed));

struct _bucket_b8k2_
{
    uint8_t Count;			///< bucket number of elements - 1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[2][8];			///< 8 byte * 2 keys
    size_t Val[2];			///< 2 values
} __attribute__ ((packed));

struct _bucket_b8k4_
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[4][8];			///< 8 byte * 4 keys
    size_t Val[4];			///< 4 values
} __attribute__ ((packed));

// 1 + 7 + 8 => 16
struct _bucket_b7k1_
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[1][7];			///< 7 byte * 1 key
    size_t Val[1];			///< 1 value
} __attribute__ ((packed));

struct _bucket_b8k256_			///< 256* values 8 bytes key
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[256][8];		///< 4 byte * 256 keys
    size_t Val[256];			///< 1 value
} __attribute__ ((packed));

#endif

struct _bucket_b4k1_			///< 1* values 4 bytes key
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[1][4];			///< 4 byte * 1 key
    size_t Val[1];			///< 1 value
} __attribute__ ((packed));

struct _bucket_b4k2_			///< 2* values 4 bytes key
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[2][4];			///< 4 byte * 2 keys
    size_t Val[2];			///< 2 values
} __attribute__ ((packed));

struct _bucket_b4k4_			///< 4* values 4 bytes key
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[4][4];			///< 4 byte * 4 keys
    size_t Val[4];			///< 4 values
} __attribute__ ((packed));

struct _bucket_b4k256_			///< 256* values 4 bytes key
{
    uint8_t Count;			///< bucket number of elements -1
    uint8_t Kind;			///< kind of bucket
    uint8_t Key[256][4];		///< 4 byte * 256 keys
    size_t Val[256];			///< 256 values
} __attribute__ ((packed));

/**
**	Array data structure.
*/
union _array_
{
    uint8_t Count;			///< bucket number of elements -1
    struct
    {
	uint8_t Count;			///< bucket number of elements -1
	uint8_t Kind;			///< kind of bucket
    } Common;				///< common part of all array nodes
#ifdef ARRAY64
    struct _bucket_b8k1_ B8K1;		///< bucket 8 bytes 1 key
    struct _bucket_b8k2_ B8K2;		///< bucket 8 bytes 2 key
    struct _bucket_b8k4_ B8K4;		///< bucket 8 bytes 4 key
#endif
    struct _bucket_b4k1_ B4K1;		///< bucket 4 bytes 1 key
    struct _bucket_b4k2_ B4K2;		///< bucket 4 bytes 2 key
    struct _bucket_b4k4_ B4K4;		///< bucket 4 bytes 4 key
};

/**
**	Array search stack.
*/
typedef struct _array_stack_
{
    Array *Array;			///< visited node
    int Offset;				///< offset into node
    size_t Key;				///< index base key of node
} ArrayStack;

/**
**	Convert number of keys into bucket size.
**
**	Can be configured to reduce realloc/copy during insert.
*/
static const int ArraySizeTable[256] = {
    0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
    0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F,
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
    0x38, 0x39, 0x3A, 0x3B, 0x3C, 0x3D, 0x3E, 0x3F,
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
    0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
    0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x5D, 0x5E, 0x5F,
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
    0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
    0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8A, 0x8B, 0x8C, 0x8D, 0x8E, 0x8F,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9A, 0x9B, 0x9C, 0x9D, 0x9E, 0x9F,
    0xA0, 0xA1, 0xA2, 0xA3, 0xA4, 0xA5, 0xA6, 0xA7,
    0xA8, 0xA9, 0xAA, 0xAB, 0xAC, 0xAD, 0xAE, 0xAF,
    0xB0, 0xB1, 0xB2, 0xB3, 0xB4, 0xB5, 0xB6, 0xB7,
    0xB8, 0xB9, 0xBA, 0xBB, 0xBC, 0xBD, 0xBE, 0xBF,
    0xC0, 0xC1, 0xC2, 0xC3, 0xC4, 0xC5, 0xC6, 0xC7,
    0xC8, 0xC9, 0xCA, 0xCB, 0xCC, 0xCD, 0xCE, 0xCF,
    0xD0, 0xD1, 0xD2, 0xD3, 0xD4, 0xD5, 0xD6, 0xD7,
    0xD8, 0xD9, 0xDA, 0xDB, 0xDC, 0xDD, 0xDE, 0xDF,
    0xE0, 0xE1, 0xE2, 0xE3, 0xE4, 0xE5, 0xE6, 0xE7,
    0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
    0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
    0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF, 0x100
};

/**
**	Biggest bucket size.
*/
static const int ArrayBurstSize = 256;

// -------------------------------------------------------------------------

#ifndef AllocMem

/**
**	better malloc
**
**	@param size	size of memory area to allocate
*/
static inline void *AllocMem(size_t size)
{
    return malloc(size);
}

#else

extern inline void *AllocMem(size_t);

#endif

#ifndef FreeMem

/**
**	better free
*/
static inline void FreeMem(void *data, __attribute__ ((unused)) size_t size)
{
    free(data);
}

#else

extern inline void FreeMem(void *, size_t);

#endif

// -------------------------------------------------------------------------
// Helper functions

/**
**	Store a key.
**
**	@param adr	address of key in bucket
**	@param cnt	number of bytes of key to store
**	@param key	key to be stored
*/
static inline void ArrayStoreKey(uint8_t * adr, const int cnt, size_t key)
{
    switch (cnt) {
#ifdef ARRAY64
	case 8:
	    adr[7] = key >> 56;
	case 7:
	    adr[6] = key >> 48;
	case 6:
	    adr[5] = key >> 40;
	case 5:
	    adr[4] = key >> 32;
#endif
	case 4:
	    adr[3] = key >> 24;
	case 3:
	    adr[2] = key >> 16;
	case 2:
	    adr[1] = key >> 8;
	case 1:
	    adr[0] = key;
	    break;
    }
}

/**
**	Fetch a key.
**
**	@param adr	address of key in bucket
**	@param cnt	number of bytes of key to fetch
**
**	@returns key fetched from bucket.
*/
static inline size_t ArrayFetchKey(const uint8_t * adr, const int cnt)
{
    register size_t key;

    key = 0;
    switch (cnt) {
#ifdef ARRAY64
	case 8:
	    key |= (size_t) adr[7] << 56;
	case 7:
	    key |= (size_t) adr[6] << 48;
	case 6:
	    key |= (size_t) adr[5] << 40;
	case 5:
	    key |= (size_t) adr[4] << 32;
#endif
	case 4:
	    key |= (size_t) adr[3] << 24;
	case 3:
	    key |= (size_t) adr[2] << 16;
	case 2:
	    key |= (size_t) adr[1] << 8;
	case 1:
	    key |= (size_t) adr[0];
	    break;
    }
    return key;
}

/**
**	Get the address of index at offset.
**
**	@param array	array bucket containing key
**	@param offset	offset into array bucket where key stored
**	@param key_len	length of stored key in this bucket
*/
static inline uint8_t *ArrayKeyAdr(const Array * array, int offset,
    int key_len)
{
    return (uint8_t *) array + 2 * sizeof(uint8_t) + offset * key_len;
}

/**
**	Get the address of value at offset.
*/
static inline void *ArrayValAdr(const Array * array, int offset, int key_len)
{
    return (void *)((uint8_t *) array + 2 * sizeof(uint8_t)
	+ ArraySizeTable[array->Count] * key_len + offset * sizeof(size_t));
}

/**
**	Get the key length of an array bucket.
*/
static inline int ArrayKeylen(const Array * array)
{
    return array->Common.Kind & 0x0F;
}

// -------------------------------------------------------------------------
// Debug functions

#ifdef ARRAY_DEBUG

/**
**	Dump array helper function.
**
**	@param stream	output stream
**	@param array	array to dump
**	@param level	indent/recursion level
*/
static void ArrayDump0(FILE * stream, const Array * array, int level,
    size_t base_key, int key_len)
{
    int count;
    int kind;
    int my_len;
    int i;

    count = array->Count;
    kind = array->Common.Kind;
    // in this version the keysize is stored in kind
    my_len = kind & 0xF;
    fprintf(stream, "%*s%8p: #%d/%d - %d(%d) (%8zx, %d)\n", level, "", array,
	count, ArraySizeTable[count], kind, my_len, base_key, key_len);

    if (kind & ArrayLeaf) {
	for (i = 0; i <= count; ++i) {
	    size_t key;
	    size_t val;

	    // remove bits stored in this bucket
	    if (my_len == sizeof(size_t)) {
		base_key = 0;
	    } else {
		base_key &= ((size_t) ~ 0) << (my_len * 8);
		fprintf(stream, "basekey %zx: \n", base_key);
	    }

	    key = ArrayFetchKey(ArrayKeyAdr(array, i, my_len), my_len);
	    val = *(size_t *) ArrayValAdr(array, i, my_len);

	    fprintf(stream, "%*s %0*zx:%08zx -> %08zx\n", level, "",
		my_len * 2, key, key | base_key, val);
	}
    } else {
	// XX..............  ................ 
	// my_len 8-1
	key_len -= my_len;
	if (key_len < 0) {
	    key_len = 0;
	}
	if (base_key & ~(size_t) 0 << (key_len * 8)) {
	    fprintf(stream, "too many bits\n");
	}
	for (i = 0; i <= count; ++i) {
	    size_t key;
	    size_t val;

	    key = ArrayFetchKey(ArrayKeyAdr(array, i, my_len), my_len);
	    key <<= (key_len * 8);
	    key |= base_key;
	    val = *(size_t *) ArrayValAdr(array, i, my_len);

	    fprintf(stream, "%*s %08zx -> %08zx\n", level, "", key, val);
	    ArrayDump0(stream, (Array *) val, level + 1, key, key_len);
	}
    }
}

/**
**	Dump array.
**
**	@param stream	output stream
**	@param array	array to dump
**	@param level	indent/recursion level
*/
void ArrayDump(FILE * stream, const Array * array, int level)
{
    if (array) {
	ArrayDump0(stream, array, level, 0, sizeof(size_t));
    }
}

#endif

// -------------------------------------------------------------------------
// Search functions

/**
**	Find index in leaf.
**
**	@param array		array leaf object
**	@param index		index key to search
**	@param[out] result	key found
**
**	@returns offset in array of found index key.
*/
static int ArrayFindIndexInLeaf(const Array * array, size_t index,
    size_t * result)
{
    int i;
    int n;
    int key_len;
    size_t key;

    n = array->Count;
    key_len = ArrayKeylen(array);

    // linear search (unroll later, binary later)
    for (i = 0; i <= n; ++i) {
	key = ArrayFetchKey(ArrayKeyAdr(array, i, key_len), key_len);
	if (key >= index) {
	    break;
	}
    }
    *result = key;
    return i;
}

/**
**	Find index in node.
**
**	@todo first key in nodes, is never used, should store biggest key,
**	than I need one lookup less.
*/
static int ArrayFindIndexInNode(const Array * array, size_t index,
    size_t * result)
{
    int i;
    int n;
    int key_len;
    size_t last_key;
    size_t key;

    n = array->Count;
    key_len = ArrayKeylen(array);

    last_key = ArrayFetchKey(ArrayKeyAdr(array, 0, key_len), key_len);
    // linear search (unroll later, binary later)
    for (i = 1; i <= n; ++i) {

	key = ArrayFetchKey(ArrayKeyAdr(array, i, key_len), key_len);
	// printf("%zx - %zx\n", key, index);
	if (key > index) {
	    // printf("found index at %d\n", i);
	    break;
	}
	last_key = key;
    }
    *result = last_key;
    return i - 1;
}

/**
**	Find index in trie.
**
**	Key is compressed, common suffix / prefix removed.
**
**	XX 
**	XX YY or .. YY
*/
static ArrayStack *ArrayFindIndexInTrie(ArrayStack * stack,
    const Array * array, size_t index)
{
    for (;;) {
	stack->Array = (Array *) array;
	if (array->Common.Kind & ArrayLeaf) {
	    stack->Offset = ArrayFindIndexInLeaf(array, index, &stack->Key);
	    return stack;
	}
	// FIXME: remove bits from index
	stack->Offset = ArrayFindIndexInNode(array, index, &stack->Key);
	array =
	    *((Array **) ArrayValAdr(array, stack->Offset,
		ArrayKeylen(array)));
	++stack;
    }
}

/**
**	Get the address of value at index from array.
**
**	@param array	dynamic array
**	@param index	index into array
**
**	@returns pointer to value of index.
*/
size_t *ArrayIdx(const Array * array, size_t index)
{
    if (array) {
	ArrayStack stack[sizeof(size_t)];
	ArrayStack *sp;

	sp = ArrayFindIndexInTrie(stack, array, index);
	if (sp->Key == index) {
	    return ArrayValAdr(sp->Array, sp->Offset, ArrayKeylen(sp->Array));
	}
    }
    return NULL;
}

/**
**	Get a value at index from array.
**
**	Use ArrayIdx() to check if value exists.
**
**	@param array	dynamic array
**	@param index	index into array
**
**	@returns value or 0, if not exisis.
*/
size_t ArrayGet(const Array * array, size_t index)
{
    size_t *val;

    if ((val = ArrayIdx(array, index))) {
	return *val;
    }
    return 0;
}

/**
**	Set a value of index in array.
**
**	@param array	dynamic array
**	@param index	index into array
**	@param value	new value of index
*/
size_t *ArraySet(Array * array, size_t index, size_t value)
{
    size_t *val;

    if ((val = ArrayIdx(array, index))) {
	*val = value;
	return val;
    }
    return NULL;
}

/**
**	First index/value pair in array.
**
**	Search (inclusive) for the first index that is equal to or greater
**	than index.
**
**	@param array	dynamic array
**	@param index	index into array
*/
size_t *ArrayFirst(const Array * array, size_t * index)
{
    if (array) {
	ArrayStack stack[sizeof(size_t)];
	ArrayStack *sp;

	sp = ArrayFindIndexInTrie(stack, array, *index);
	if (sp->Offset <= sp->Array->Count) {
	    *index = sp->Key;
	    return ArrayValAdr(sp->Array, sp->Offset, ArrayKeylen(sp->Array));
	}
	// go up, until we can go down again
	while (sp != stack) {
	    --sp;
	    // printf("up %p(%d)\n", sp->Array, sp->Offset);
	    if (++sp->Offset <= sp->Array->Count) {
		// size_t base_key;
		size_t key;
		int key_len;
		int offset;
		size_t *val;

		// printf("down %p(%d)\n", sp->Array, sp->Offset);
		array = sp->Array;
		offset = sp->Offset;
		// FIXME: base_key = sp->Key;
		// go down, until leaf reached
		for (;;) {
		    key_len = ArrayKeylen(array);
		    if (key_len != sizeof(size_t)) {
			abort();
		    }
		    // base_key = 0;

		    key =
			ArrayFetchKey(ArrayKeyAdr(array, offset, key_len),
			key_len);
		    val = ArrayValAdr(array, offset, key_len);
		    // reached leaf
		    if (array->Common.Kind & ArrayLeaf) {
			*index = key;
			return val;
		    }
		    array = *(Array **) val;
		    offset = 0;
		}
	    }
	}
    }
    return NULL;
}

/**
**	Next index/value in array.
**
**	Search (exclusive) for the next index that is greater than index.
**
**	@param array	dynamic array
**	@param index	index into array
*/
size_t *ArrayNext(const Array * array, size_t * index)
{
    if (*index == SIZE_MAX) {
	return NULL;
    }
    ++*index;
    return ArrayFirst(array, index);
}

/**
**	Last index/value in array.
**
**	Search (inclusive) for the Last index that is equal to or less
**	than index.
**
**	@param array	dynamic array
**	@param index	index into array
*/
size_t *ArrayLast(const Array * array, size_t * index)
{
    if (array) {
	ArrayStack stack[sizeof(size_t)];
	ArrayStack *sp;
	int key_len;

	sp = ArrayFindIndexInTrie(stack, array, *index);
	key_len = ArrayKeylen(sp->Array);
	// printf("\tkey %zd = %zd\n", sp->Key, *index);
	if (sp->Key == *index) {
	    return ArrayValAdr(sp->Array, sp->Offset, key_len);
	}
	// printf("\tfind previous %d/%d\n", sp->Offset, sp->Array->Count);
	if (sp->Offset) {
	    --sp->Offset;
	    *index =
		ArrayFetchKey(ArrayKeyAdr(array, sp->Offset, key_len),
		key_len);
	    return ArrayValAdr(sp->Array, sp->Offset, key_len);
	}
	// go up, until we can go down again
	while (sp != stack) {
	    --sp;
	    // printf("\tup %p(%d)\n", sp->Array, sp->Offset);
	    if (sp->Offset--) {
		// size_t base_key;
		size_t key;
		int offset;
		size_t *val;

		// printf("\tdown %p(%d)\n", sp->Array, sp->Offset);
		array = sp->Array;
		offset = sp->Offset;
		// FIXME: base_key = sp->Key;
		// go down, until leaf reached
		for (;;) {
		    key_len = ArrayKeylen(array);
		    if (key_len != sizeof(size_t)) {
			abort();
		    }
		    // base_key = 0;

		    key =
			ArrayFetchKey(ArrayKeyAdr(array, offset, key_len),
			key_len);
		    val = ArrayValAdr(array, offset, key_len);
		    // reached leaf
		    if (array->Common.Kind & ArrayLeaf) {
			*index = key;
			return val;
		    }
		    array = *(Array **) val;
		    offset = array->Count;
		}
	    }
	}
    }
    return NULL;
}

/**
**	Previous index/value in array.
**
**	Search (exclusive) for the previous index that is less than index.
**
**	@param array	dynamic array
**	@param index	index into array
*/
size_t *ArrayPrev(const Array * array, size_t * index)
{
    if (*index == 0) {
	return NULL;
    }
    --*index;
    return ArrayLast(array, index);
}

/**
**	Insert a key/value into bucket.
**
**	@param array	array bucket
**	@param offset	offset into bucket, where to store key
**	@param key	key (for nodes must be shifted down)
**	@param value	value to store
**
**	@returns new array if reallocate.
*/
static Array *ArrayInsertKey(Array * array, int offset, size_t key,
    size_t value)
{
    int key_len;
    int val_len;
    int count;
    int old_size;
    int new_size;
    uint8_t *src;
    uint8_t *dst;
    Array *new;

    val_len = sizeof(size_t);
    key_len = ArrayKeylen(array);
    count = array->Count;
    old_size = ArraySizeTable[count];
    new_size = ArraySizeTable[++count];

    //
    //	in same size range, just insert key
    //
    if (old_size == new_size) {
	// copy key up
	src = ArrayKeyAdr(array, offset, key_len);
	dst = src + key_len;
	// can be 0 bytes
	memmove(dst, src, (count - offset) * key_len);
	ArrayStoreKey(src, key_len, key);

	// copy val up
	src = ArrayValAdr(array, offset, key_len);
	dst = src + val_len;
	memmove(dst, src, (count - offset) * val_len);
	memcpy(src, &value, val_len);

	array->Count = count;
	return array;
    }
    //
    //	New size range, allocate new array and copy old into new
    //
    new = AllocMem(2 * sizeof(uint8_t) + new_size * (key_len + val_len));
    new->Count = count;
    new->Common.Kind = array->Common.Kind;

    // copy key
    src = (uint8_t *) array + 2 * sizeof(uint8_t);
    dst = (uint8_t *) new + 2 * sizeof(uint8_t);
    memcpy(dst, src, offset * key_len);	// can be 0
    src += offset * key_len;
    dst += offset * key_len;
    ArrayStoreKey(dst, key_len, key);
    dst += key_len;
    memcpy(dst, src, (count - offset) * key_len);	// can be 0

    // copy val
    src = (uint8_t *) array + 2 * sizeof(uint8_t) + old_size * key_len;
    dst = (uint8_t *) new + 2 * sizeof(uint8_t) + new_size * key_len;
    memcpy(dst, src, offset * val_len);	// can be 0
    src += offset * val_len;
    dst += offset * val_len;
    memcpy(dst, &value, val_len);
    dst += val_len;
    memcpy(dst, src, (count - offset) * val_len);	// can be 0

    free(array);

    return new;
}

/**
**	Find position to split bucket into two buckets.
*/
static int ArrayFindSplitPoint(const Array * array, int offset, int key_len,
    size_t index)
{
    int count;
    int split;
    size_t key1;
    size_t key2;

    count = array->Count;

    // xx00 / yy00
    // in the middle, where no group is split
    // 0 ... split split + 1 ... count;

    split = count / 2;
    key1 = ArrayFetchKey(ArrayKeyAdr(array, split, key_len), key_len)
	/ ArrayBurstSize;
    // check upper half for split point
    while (++split <= count) {
	key2 = ArrayFetchKey(ArrayKeyAdr(array, split, key_len), key_len);
	// must be in different buckets
	if (key1 != key2 / ArrayBurstSize) {
	    break;
	}
    }
    // check lower half for split point
    if (split > count) {
	split = count / 2;
	while (--split >= 0) {
	    key2 = ArrayFetchKey(ArrayKeyAdr(array, split, key_len), key_len);
	    // must be in different buckets
	    if (key1 != key2 / ArrayBurstSize) {
		break;
	    }
	}
	// FIXME: key?
	// not enough space
	if (split < 0 && offset) {
	    // printf("start of bucket %d - %d\n", offset, split);
	    split = count;
	}
    } else {
	// now check new key.
	if (split != offset || key1 != index / ArrayBurstSize) {
	    --split;			// don't include it
	}
	// not enough space
	if (offset <= split && split >= ArrayBurstSize - 1) {
	    // printf("end of bucket %d - %d\n", offset, split);
	    split = offset - 1;
	}
    }

    // printf("burst %08zx/%08zx at %d/%d ins %d\n", key1, key2, split, count, offset);

    return split;
}

/**
**	Split bucket.
**
**	Split array into new1/new2 insert value with index at offset.
**
**	@param parent	parent of array
**	@param offset	where to split the bucket
**	@param key_len	key length of bucket
**	@param index	key to insert
**	@param value	value of key
*/
static Array *ArraySplit(Array ** parent, int offset, int key_len,
    size_t index, size_t value)
{
    Array *array;
    int count;
    int split;
    int val_len;
    int count1;
    int key1_len;
    Array *new1;
    int count2;
    int key2_len;
    Array *new2;
    int i;
    uint8_t *src;
    uint8_t *dst;

    array = *parent;
    split = ArrayFindSplitPoint(array, offset, key_len, index);
    count = array->Count;

    val_len = sizeof(size_t);

    // FIXME: need to calculate new key-size

    //
    //	Keys
    //

    count1 = split;
    if (offset <= split) {		// new key inside first
	++count1;
    }
    key1_len = key_len;
    new1 =
	AllocMem(2 * sizeof(uint8_t) + ArraySizeTable[count1] * (key1_len +
	    val_len));
    new1->Count = count1;
    new1->Common.Kind = array->Common.Kind;

    // copy lower half of keys into new bucket
    src = ArrayKeyAdr(array, 0, key_len);
    dst = ArrayKeyAdr(new1, 0, key1_len);
    for (i = 0; i <= count1; ++i) {
	if (i == offset) {
	    ArrayStoreKey(dst, key1_len, index);
	    dst += key1_len;
	    continue;
	}
	ArrayStoreKey(dst, key1_len, ArrayFetchKey(src, key_len));
	src += key_len;
	dst += key1_len;
    }

    count2 = count - split;
    if (offset <= split) {		// new key outside second
	--count2;
    }
    key2_len = key_len;
    new2 =
	AllocMem(2 * sizeof(uint8_t) + ArraySizeTable[count2] * (key2_len +
	    val_len));
    new2->Count = count2;
    new2->Common.Kind = array->Common.Kind;

    // copy upper half of keys into new bucket
    // src from above
    dst = ArrayKeyAdr(new2, 0, key2_len);
    for (i = 0; i <= count2; ++i) {
	if (i == offset - count1 - 1) {
	    ArrayStoreKey(dst, key2_len, index);
	    dst += key2_len;
	    continue;			// could be last
	}
	ArrayStoreKey(dst, key2_len, ArrayFetchKey(src, key_len));
	src += key_len;
	dst += key2_len;
    }

    //
    //	Values
    //

    // copy lower half of values into new bucket
    src = ArrayValAdr(array, 0, key_len);
    dst = ArrayValAdr(new1, 0, key1_len);
    if (offset <= split) {
	memcpy(dst, src, offset * val_len);
	src += offset * val_len;
	dst += offset * val_len;
	memcpy(dst, &value, val_len);
	dst += val_len;
	memcpy(dst, src, (split + 1 - offset) * val_len);
    } else {
	memcpy(dst, src, (count1 + 1) * val_len);
    }

    // copy upper half of values into new bucket
    src = ArrayValAdr(array, split + 1, key_len);
    dst = ArrayValAdr(new2, 0, key2_len);
    if (offset > split) {
	offset -= count1 + 1;
	memcpy(dst, src, offset * val_len);
	src += offset * val_len;
	dst += offset * val_len;
	memcpy(dst, &value, val_len);
	dst += val_len;
	memcpy(dst, src, (count2 - offset) * val_len);
    } else {
	memcpy(dst, src, (count2 + 1) * val_len);
    }

    FreeMem(array,
	2 * sizeof(uint8_t) + ArraySizeTable[count] * (key_len + val_len));

    *parent = new1;
    return new2;
}

/**
**	Array new root.
*/
static Array *ArrayNewRoot(const Array * array1, const Array * array2)
{
    int key_len;
    int val_len;
    size_t key1;
    size_t key2;
    Array *array;
    uint8_t *key;
    Array **val;

    val_len = sizeof(size_t);
    // first key of each bucket, aligned to burst size
    key1 =
	ArrayFetchKey(ArrayKeyAdr(array1, 0, ArrayKeylen(array1)),
	ArrayKeylen(array1));
    key1 -= key1 % ArrayBurstSize;

    key2 =
	ArrayFetchKey(ArrayKeyAdr(array2, 0, ArrayKeylen(array2)),
	ArrayKeylen(array2));
    key2 -= key2 % ArrayBurstSize;

    // FIXME: must calculate new key - length
    key_len = ArrayKeylen(array2);

    array = AllocMem(2 * sizeof(uint8_t)
	+ ArraySizeTable[2] * (key_len + val_len));
    array->Count = 1;
    array->Common.Kind = ArrayNode | key_len;

    key = ArrayKeyAdr(array, 0, key_len);
    ArrayStoreKey(key, key_len, key1);
    key += key_len;
    ArrayStoreKey(key, key_len, key2);

    val = ArrayValAdr(array, 0, key_len);
    val[0] = (Array *) array1;
    val[1] = (Array *) array2;

    //printf("new root:\n");
    //ArrayDump(stdout, array, 2);

    return array;
}

/**
**	Insert a value with index into array.
**
**	Inserts value at index into array, if not already there.
**
**	@param[in,out] root	array root
**	@param index		index key into array
**	@param value		value to insert into array
**
**	@returns address of data of index.
*/
size_t *ArrayIns(Array ** root, size_t index, size_t value)
{
    Array *array;
    ArrayStack stack[sizeof(size_t)];
    ArrayStack *sp;
    size_t *val;

    if (!(array = *root)) {		// empty array
#ifdef ARRAY64
	*root = array = AllocMem(sizeof(array->B8K1));
	array->B8K1.Count = 0;
	array->B8K1.Kind = ArrayLeaf | ArrayKeylen8;
	ArrayStoreKey(array->B8K1.Key[0], sizeof(size_t), index);
	array->B8K1.Val[0] = value;
	return &array->B8K1.Val[0];
#else
	*root = array = AllocMem(sizeof(array->B4K1));
	array->B4K1.Count = 0;
	array->B4K1.Kind = ArrayLeaf | ArrayKeylen4;
	ArrayStoreKey(array->B4K1.Key[0], sizeof(size_t), index);
	array->B4K1.Val[0] = value;
	return &array->B4K1.Val[0];
#endif
    }

    sp = ArrayFindIndexInTrie(stack, array, index);
    // FIXME: sp->Key not correct if key compressed
    if (sp->Key == index) {		// found
	// printf("already in trie\n");
	return ArrayValAdr(sp->Array, sp->Offset, ArrayKeylen(sp->Array));
    }

    val = NULL;
    for (;;) {
	Array *array2;

	array = sp->Array;
	// ckeck if enough space in bucket
	if (array->Count != ArrayBurstSize - 1) {
	    break;
	}

	array2 =
	    ArraySplit(&array, sp->Offset, ArrayKeylen(sp->Array), index,
	    value);

	if (!val) {			// insert point
	    if (sp->Offset <= array->Count) {
		val = ArrayValAdr(array, sp->Offset, ArrayKeylen(array));
	    } else {
		val =
		    ArrayValAdr(array2, sp->Offset - array->Count - 1,
		    ArrayKeylen(array2));
	    }
	}

	if (sp == stack) {		// reached root
	    *root = ArrayNewRoot(array, array2);
	    return val;
	}
	--sp;

	// update first in parent
	*(Array **) ArrayValAdr(sp->Array, sp->Offset,
	    ArrayKeylen(sp->Array)) = array;
	// insert second into parent
	index =
	    ArrayFetchKey(ArrayKeyAdr(array2, 0, ArrayKeylen(array2)),
	    ArrayKeylen(array2));
	index -= index % ArrayBurstSize;
	value = (size_t) array2;
	++sp->Offset;
    }

    array = ArrayInsertKey(array, sp->Offset, index, value);
    if (!val) {
	val = ArrayValAdr(array, sp->Offset, ArrayKeylen(array));
    }
    // update parent
    if (sp == stack) {
	*root = array;
    } else {
	--sp;
	*(Array **) ArrayValAdr(sp->Array, sp->Offset,
	    ArrayKeylen(sp->Array)) = array;
    }

    return val;
}

/**
**	Delete index from array.
**
**	@param[in,out] root	array root
**	@param index		index key into array
**
**	@returns true if index was deleted successful, false otherwise.
*/
size_t ArrayDel(Array ** root, size_t index)
{
    printf("FIXME: delete %p(%zd)\n", root, index);
    return 0;
}

/**
**	Free entire array.  Much faster than using ArrayDel loop.
**
**	@param array	array to free
*/
void ArrayFree(Array * array)
{
    int count;
    int kind;
    int key_len;

    if (!array) {			// empty array
	return;
    }

    count = array->Count;
    kind = array->Common.Kind;
    // in this version the keysize is stored in kind
    key_len = kind & 0xF;

    if (~kind & ArrayLeaf) {
	int i;

	for (i = 0; i <= count; ++i) {
	    ArrayFree(*(Array * *)ArrayValAdr(array, i, key_len));
	}
    }

    FreeMem(array, 2 * sizeof(uint8_t)
	+ ArraySizeTable[count] * (key_len + sizeof(size_t)));
}

/**
**	Get memory used by array.
**
**	@param array	array to analyse
*/
size_t ArrayMemUsed(const Array * array)
{
    // int total;

    // total = 0;
    printf("mem-used %p\n", array);
    return 0;
}

// -------------------------------------------------------------------------

#ifdef ARRAY_TEST			// {

#include <getopt.h>
#include <errno.h>
#include <malloc.h>
#include <sys/time.h>

typedef size_t ArrayValue;
typedef size_t ArrayIndex;

// -------------------------------------------------------------------------

#ifdef AllocMem

/**
**	better malloc
**
**	@param size	size of memory area to allocate
*/
inline void *AllocMem(size_t size)
{
    return malloc(size);
}

#endif

#ifdef FreeMem

/**
**	better free
*/
inline void FreeMem(void *data, __attribute__ ((unused)) size_t size)
{
    free(data);
}

#endif

// -------------------------------------------------------------------------
//	64 bit pseudo random generator

/**
**	State type for the lfsr258 pseudo random generator.
**
**	The seeds must be set before calling the generator and,
**	**must** be larger than 1, 511, 4095, 131071 and 8388607 respectively.
**
**	@see lfsr258() @see lfsr258_init()
*/
typedef struct __lfsr258__
{
    uint64_t S[5];			///< 320bit pool
} lfsr258_t;

/**
**	64-bit LFSR generator of L'Ecuyer.
**
**	@param state	generator state pool
*/
uint64_t lfsr258(lfsr258_t * state)
{
    state->S[0] =
	((state->S[0] & UINT64_C(0xFFFFFFFFFFFFFFFE)) << 10) ^ (((state->
		S[0] << 1)
	    ^ state->S[0]) >> 53);
    state->S[1] =
	((state->S[1] & UINT64_C(0xFFFFFFFFFFFFFE00)) << 5) ^ (((state->
		S[1] << 24)
	    ^ state->S[1]) >> 50);
    state->S[2] =
	((state->S[2] & UINT64_C(0xFFFFFFFFFFFFF000)) << 29) ^ (((state->
		S[2] << 3)
	    ^ state->S[2]) >> 23);
    state->S[3] =
	((state->S[3] & UINT64_C(0xFFFFFFFFFFFE0000)) << 23) ^ (((state->
		S[3] << 5)
	    ^ state->S[3]) >> 24);
    state->S[4] =
	((state->S[4] & UINT64_C(0xFFFFFFFFFF800000)) << 8) ^ (((state->
		S[4] << 3) ^ state->S[4]) >> 33);

    return state->S[0] ^ state->S[1] ^ state->S[2] ^ state->S[3] ^ state->S[4];
}

/**
**	Initialize the state of the lfsr258 random generator with seed.
**
**	@param state	generator state pool
*/
void lfsr258_init(unsigned seed, lfsr258_t * state)
{
    state->S[0] = (seed ? seed : 1) * UINT64_C(0x5851F42D4C957F2D)
	+ UINT64_C(0x14057B7EF767814F);
    state->S[1] = state->S[0] * UINT64_C(0x5851F42D4C957F2D)
	+ UINT64_C(0x14057B7EF767814F);
    state->S[2] = state->S[1] * UINT64_C(0x5851F42D4C957F2D)
	+ UINT64_C(0x14057B7EF767814F);
    state->S[3] = state->S[2] * UINT64_C(0x5851F42D4C957F2D)
	+ UINT64_C(0x14057B7EF767814F);
    state->S[4] = state->S[3] * UINT64_C(0x5851F42D4C957F2D)
	+ UINT64_C(0x14057B7EF767814F);

    if (!(state->S[0] & UINT64_C(0xFFFFFFFFFFFFFFFE))) {
	state->S[0] |= 1 + ~UINT64_C(0xFFFFFFFFFFFFFFFE);
    }
    if (!(state->S[1] & UINT64_C(0xFFFFFFFFFFFFFE00))) {
	state->S[1] |= 1 + ~UINT64_C(0xFFFFFFFFFFFFFE00);
    }
    if (!(state->S[2] & UINT64_C(0xFFFFFFFFFFFFF000))) {
	state->S[2] |= 1 + ~UINT64_C(0xFFFFFFFFFFFFF000);
    }
    if (!(state->S[3] & UINT64_C(0xFFFFFFFFFFFE0000))) {
	state->S[3] |= 1 + ~UINT64_C(0xFFFFFFFFFFFE0000);
    }
    if (!(state->S[4] & UINT64_C(0xFFFFFFFFFF800000))) {
	state->S[4] |= 1 + ~UINT64_C(0xFFFFFFFFFF800000);
    }
}

// -------------------------------------------------------------------------

/**
**	Test the array.
**
**	@param debug	enable debug
**	@param loops	run the test 'loops' times
*/
static void ArrayTest(int debug, int loops)
{
    lfsr258_t lfsr_state[1];
    int i;
    int j;
    size_t *value;
    Array *array;

    lfsr258_init(314159265, lfsr_state);

    array = ArrayNew();
    for (i = 0; i < loops; ++i) {
	size_t v;
	size_t old;

	// v = lfsr258(lfsr_state) * 13;
	v = lfsr258(lfsr_state) >> 4;
	// v = -1 - i;
	// v = -1 - i * 3;
	// v = i;
	// v = i * 3;
	// v = i + 5;
	value = ArrayIns(&array, (size_t) v, (size_t) v);
	if (!value) {
	    printf("null value\n");
	    abort();
	}
	if (debug) {
	    printf("\n");
	    ArrayDump(stdout, array, 0);
	}
	if (*value != v) {
	    printf("not value %zx!=%zx\n", *value, v);
	    abort();
	}
	if (v != ArrayGet(array, (size_t) v)) {
	    printf("ArrayGet failed %zx!=%zx\n", ArrayGet(array, (size_t) v),
		v);
	    abort();
	}

	j = 0;
	v = 0;
	value = ArrayFirst(array, &v);
	if (value) {
	    old = *value;
	    // printf("first %zx\n", *value);
	    while (value) {
		++j;
		if (*value != v) {
		    printf("internal error %zx!=%zx\n", *value, v);
		    abort();
		}
		value = ArrayNext(array, &v);
		if (value) {
		    if (*value <= old) {
			printf("internal error %zx<=%zx\n", *value, old);
			abort();
		    }
		    old = *value;
		    // printf("next %zx\n", *value);
		}
	    }
	}
	if (j > i + 1) {
	    printf("internal error count %d>%d\n", j, i);
	    abort();
	}
	if (debug) {
	    printf("\n");
	}
    }
    printf("After %d loops\n", loops);
    ArrayDump(stdout, array, 0);
}

/**
**	Benchmark.
*/
static void ArrayBenchmark(int debug, const char *bench, const char *query,
    __attribute__ ((unused))
    int loops)
{
    FILE *stream;
    ArrayIndex index;
    Array *array;
    int c;
    unsigned insert_n;
    unsigned query_n;
    struct timeval start;
    struct timeval stop;
    double insert_real_time;
    double query_real_time;

    if (!(stream = fopen(bench ? bench : "keys3", "rb"))) {
	fprintf(stderr, "can't open file '%s': %s\n", bench ? bench : "keys3",
	    strerror(errno));
	return;
    }

    array = ArrayNew();
    insert_n = 0;
    index = 0;
    gettimeofday(&start, NULL);
    while ((c = fgetc(stream)) != EOF) {
	if (c) {
	    index *= 256;
	    index += c;
	} else {
	    //index *= 256;
	    //index += c;
	    // printf("%jx\n", index);
	    ArrayIns(&array, index, index);
	    insert_n++;
	    index = 0;
	}
    }
    gettimeofday(&stop, NULL);
    insert_real_time = 1000000.0 * (stop.tv_sec - start.tv_sec)
	+ (stop.tv_usec - start.tv_usec);
    fclose(stream);

    if (query) {
	if (!(stream = fopen(bench ? query : "query3", "rb"))) {
	    fprintf(stderr, "can't open file '%s': %s\n",
		bench ? bench : "query3", strerror(errno));
	    return;
	}

	query_n = 0;
	index = 0;
	gettimeofday(&start, NULL);
	while ((c = fgetc(stream)) != EOF) {
	    if (c) {
		index *= 256;
		index += c;
	    } else {
		ArrayValue value;

		value = ArrayGet(array, index);
		if (value && value != index) {
		    fprintf(stderr, "internal error %jx\n", (uintmax_t) index);
		}
		query_n++;
		index = 0;
	    }
	}
	gettimeofday(&stop, NULL);
	query_real_time = 1000000.0 * (stop.tv_sec - start.tv_sec)
	    + (stop.tv_usec - start.tv_usec);
	fclose(stream);
    }

    if (debug) {
	ArrayDump(stdout, array, 0);
    }

    printf("%.2fs time to insert %d keys, %.2fus/key\n",
	insert_real_time / 1000000, insert_n, insert_real_time / insert_n);
    if (query) {
	printf("%.2fs time to query %d keys, %.2fus/key\n",
	    query_real_time / 1000000, query_n, query_real_time / query_n);
    }

    malloc_stats();
}

/**
**	Print usage.
*/
static void PrintUsage(void)
{
    printf("core-array Version " VERSION
#ifdef GIT_REV
	"(GIT-" GIT_REV ")"
#endif
	", (c) 2009, 2010, 2015 by Lutz Sammer\n"
	"\tLicense AGPLv3: GNU Affero General Public License version 3\n");
    printf
	("Usage: array_test [-?|-h] [-v] [-b keys] [-d] [-l loops] [-q query]\n"
	"\t-d\t\tuse debug, more increase debug\n"
	"\t-b keys\trun benchmark inserting 'keys'\n"
	"\t-l loops\trun debug/benchmark 'loops' times\n"
	"\t-q query\trun benchmark quering 'query'\n"
	"\t-? -h\t\tdisplay this message\n"
	"\t-v\t\tdisplay version information\n"
	"Only idiots print usage on stderr!\n");
}

/**
**	Main entry point.
**
**	@param argc	number of arguments
**	@param argv	arguments vector
**
**	@returns -1 on failures, 0 clean exit.
*/
int main(int argc, char *const argv[])
{
    const char *bench;
    const char *query;
    int debug;
    int loops;

    bench = NULL;
    query = NULL;
    debug = 0;
    loops = 100;

    //
    //	Parse command line arguments
    //
    for (;;) {
	switch (getopt(argc, argv, "hv?-b:dl:q:")) {

	    case EOF:
		break;
	    case 'b':			// enabled benchmark
		bench = optarg;
		continue;
	    case 'd':			// enabled debug
		debug = 1;
		continue;
	    case 'l':			// test loops
		loops = strtol(optarg, NULL, 0);
		continue;
	    case 'q':			// enabled benchmark
		query = optarg;
		continue;
	    case 'v':			// print version
		// PrintVersion();
		PrintUsage();
		return 0;
	    case '?':
	    case 'h':			// help usage
		// PrintVersion();
		PrintUsage();
		return 0;
	    case '-':
		// PrintVersion();
		PrintUsage();
		fprintf(stderr, "\nWe need no long options\n");
		return -1;
	    case ':':
		// PrintVersion();
		fprintf(stderr, "Missing argument for option '%c'\n", optopt);
		return -1;
	    default:
		// PrintVersion();
		fprintf(stderr, "Unkown option '%c'\n", optopt);
		return -1;
	}
	break;
    }
    if (optind < argc) {
	// PrintVersion();
	while (optind < argc) {
	    fprintf(stderr, "Unhandled argument '%s'\n", argv[optind++]);
	}
	return -1;
    }
    //
    //	  main loop
    //
    if (0) {
	int i;
	char *p;

	for (i = 0; i < 3; ++i) {
	    p = malloc(1);
	    printf("%p %d\n", p, (int)(((ptrdiff_t) p) & 0xF));
	}
    }
    if (0) {
	printf("void* %zd\n", sizeof(void *));
	printf("double %zd\n", sizeof(double));
	printf("ptrdiff_t %zd\n", sizeof(ptrdiff_t));
	printf("size_t %zd\n", sizeof(size_t));
	printf("4/1 %zd\n", sizeof(struct _bucket_b4k1_));
	printf("4/2 %zd\n", sizeof(struct _bucket_b4k2_));
#ifdef ARRAY64
	printf("8/1 %zd\n", sizeof(struct _bucket_b8k1_));
	printf("7/1 %zd\n", sizeof(struct _bucket_b7k1_));
#endif
    }

    if (bench || query) {
	ArrayBenchmark(debug, bench, query, loops);
    } else {
	ArrayTest(debug, loops);
    }

    return 0;
}

#endif // } ARRAY_TEST

/// @}
