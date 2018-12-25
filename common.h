#ifndef __LINUX_COMMON_H
#define __LINUX_COMMON_H
#include <math.h>
#include <ctype.h>
#include <unitypes.h>

#define BITMAP_FIRST_WORD_MASK(start) (~0UL << ((start) & (BITS_PER_LONG - 1)))
#define BITMAP_LAST_WORD_MASK(nbits) (~0UL >> (-(nbits) & (BITS_PER_LONG - 1)))
#define BITS_PER_LONG 64
#define BITS_PER_BYTE 8
#define DIV_ROUND_UP(n, d) (((n) + (d)-1) / (d))
#define BITS_TO_LONGS(nr) DIV_ROUND_UP(nr, BITS_PER_BYTE * sizeof(long))
#define BITOP_WORD(nr) ((nr) / BITS_PER_LONG)
#define BIT_WORD(nr) ((nr) / BITS_PER_LONG)
#define BIT_MASK(nr) (1UL << ((nr) % BITS_PER_LONG))
#define min(a, b) (((a) < (b)) ? (a) : (b))
#define max(a, b) (((a) > (b)) ? (a) : (b))
#define __ALIGN_MASK(x, mask) (((x) + (mask)) & ~(mask))
#define __round_mask(x, y) ((typeof(x))((y)-1))
#define round_down(x, y) ((x) & ~__round_mask(x, y))
#define IS_ALIGNED(x, a) (((x) & ((typeof(x))(a)-1)) == 0)
#define ffz(x) __ffs(~(x))
#define PAGE_SIZE 4096
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))

#define container_of(ptr, type, member)                                        \
	({                                                                     \
		const typeof(((type *)0)->member) *__mptr = (ptr);             \
		(type *)((char *)__mptr - offsetof(type, member));             \
	})

static __always_inline unsigned long __fls(unsigned long word)
{
	int num = BITS_PER_LONG - 1;

#if BITS_PER_LONG == 64
	if (!(word & (~0ul << 32))) {
		num -= 32;
		word <<= 32;
	}
#endif
	if (!(word & (~0ul << (BITS_PER_LONG - 16)))) {
		num -= 16;
		word <<= 16;
	}
	if (!(word & (~0ul << (BITS_PER_LONG - 8)))) {
		num -= 8;
		word <<= 8;
	}
	if (!(word & (~0ul << (BITS_PER_LONG - 4)))) {
		num -= 4;
		word <<= 4;
	}
	if (!(word & (~0ul << (BITS_PER_LONG - 2)))) {
		num -= 2;
		word <<= 2;
	}
	if (!(word & (~0ul << (BITS_PER_LONG - 1))))
		num -= 1;
	return num;
}

static __always_inline unsigned long __ffs(unsigned long word)
{
	int num = 0;
#if BITS_PER_LONG == 64
	if ((word & 0xffffffff) == 0) {
		num += 32;
		word >>= 32;
	}
#endif
	if ((word & 0xffff) == 0) {
		num += 16;
		word >>= 16;
	}
	if ((word & 0xff) == 0) {
		num += 8;
		word >>= 8;
	}
	if ((word & 0xf) == 0) {
		num += 4;
		word >>= 4;
	}
	if ((word & 0x3) == 0) {
		num += 2;
		word >>= 2;
	}
	if ((word & 0x1) == 0)
		num += 1;
	return num;
}

static __always_inline int hex_to_bin(char ch)
{
	if ((ch >= '0') && (ch <= '9'))
		return ch - '0';
	ch = tolower(ch);
	if ((ch >= 'a') && (ch <= 'f'))
		return ch - 'a' + 10;
	return -1;
}

static __always_inline unsigned long
_find_next_bit(const unsigned long *addr1, const unsigned long *addr2,
	       unsigned long nbits, unsigned long start, unsigned long invert)
{
	unsigned long tmp;

	if (start >= nbits)
		return nbits;

	tmp = addr1[start / BITS_PER_LONG];
	if (addr2)
		tmp &= addr2[start / BITS_PER_LONG];
	tmp ^= invert;

	/* Handle 1st word. */
	tmp &= BITMAP_FIRST_WORD_MASK(start);
	start = round_down(start, BITS_PER_LONG);

	while (!tmp) {
		start += BITS_PER_LONG;
		if (start >= nbits)
			return nbits;

		tmp = addr1[start / BITS_PER_LONG];
		if (addr2)
			tmp &= addr2[start / BITS_PER_LONG];
		tmp ^= invert;
	}

	return min(start + __ffs(tmp), nbits);
}

/*
 * Find the next set bit in a memory region.
 */
static __always_inline unsigned long find_next_bit(const unsigned long *addr,
						   unsigned long size,
						   unsigned long offset)
{
	return _find_next_bit(addr, NULL, size, offset, 0UL);
}

static __always_inline unsigned long
find_next_zero_bit(const unsigned long *addr, unsigned long size,
		   unsigned long offset)
{
	return _find_next_bit(addr, NULL, size, offset, ~0UL);
}

static __always_inline unsigned long
find_next_and_bit(const unsigned long *addr1, const unsigned long *addr2,
		  unsigned long size, unsigned long offset)
{
	return _find_next_bit(addr1, addr2, size, offset, 0UL);
}

/*
 * Find the first set bit in a memory region.
 */
static __always_inline unsigned long find_first_bit(const unsigned long *addr,
						    unsigned long size)
{
	unsigned long idx;

	for (idx = 0; idx * BITS_PER_LONG < size; idx++) {
		if (addr[idx])
			return min(idx * BITS_PER_LONG + __ffs(addr[idx]),
				   size);
	}

	return size;
}

/*
 * Find the first cleared bit in a memory region.
 */
static __always_inline unsigned long
find_first_zero_bit(const unsigned long *addr, unsigned long size)
{
	unsigned long idx;

	for (idx = 0; idx * BITS_PER_LONG < size; idx++) {
		if (addr[idx] != ~0UL)
			return min(idx * BITS_PER_LONG + ffz(addr[idx]), size);
	}

	return size;
}

static __always_inline int test_bit(int nr, const volatile unsigned long *addr)
{
	return 1UL & (addr[BIT_WORD(nr)] >> (nr & (BITS_PER_LONG - 1)));
}

static __always_inline void set_bit(int nr, volatile unsigned long *addr)
{
	unsigned long mask = BIT_MASK(nr);
	unsigned long *p = ((unsigned long *)addr) + BIT_WORD(nr);

	*p |= mask;
}

static __always_inline void clear_bit(int nr, volatile unsigned long *addr)
{
	unsigned long mask = BIT_MASK(nr);
	unsigned long *p = ((unsigned long *)addr) + BIT_WORD(nr);

	*p &= ~mask;
}

static __always_inline unsigned int hweight32(uint32_t w)
{
	unsigned int res = w - ((w >> 1) & 0x55555555);
	res = (res & 0x33333333) + ((res >> 2) & 0x33333333);
	res = (res + (res >> 4)) & 0x0F0F0F0F;
	res = res + (res >> 8);
	return (res + (res >> 16)) & 0x000000FF;
}

static __always_inline unsigned long hweight64(uint64_t w)
{
	uint64_t res = w - ((w >> 1) & 0x5555555555555555ul);
	res = (res & 0x3333333333333333ul) +
	      ((res >> 2) & 0x3333333333333333ul);
	res = (res + (res >> 4)) & 0x0F0F0F0F0F0F0F0Ful;
	res = res + (res >> 8);
	res = res + (res >> 16);
	return (res + (res >> 32)) & 0x00000000000000FFul;
}

static __always_inline unsigned long hweight_long(unsigned long w)
{
	return sizeof(w) == 4 ? hweight32(w) : hweight64(w);
}

#define for_each_set_bit(bit, addr, size)                                      \
	for ((bit) = find_first_bit((addr), (size)); (bit) < (size);           \
	     (bit) = find_next_bit((addr), (size), (bit) + 1))

static __always_inline int fls64(uint64_t x)
{
	if (x == 0)
		return 0;
	return __fls(x) + 1;
}

static __always_inline int fls(int x)
{
	int r = 32;

	if (!x)
		return 0;
	if (!(x & 0xffff0000u)) {
		x <<= 16;
		r -= 16;
	}
	if (!(x & 0xff000000u)) {
		x <<= 8;
		r -= 8;
	}
	if (!(x & 0xf0000000u)) {
		x <<= 4;
		r -= 4;
	}
	if (!(x & 0xc0000000u)) {
		x <<= 2;
		r -= 2;
	}
	if (!(x & 0x80000000u)) {
		x <<= 1;
		r -= 1;
	}
	return r;
}

static __always_inline unsigned fls_long(unsigned long l)
{
	if (sizeof(l) == 4)
		return fls(l);
	return fls64(l);
}

static __always_inline unsigned long roundup_pow_of_two(unsigned long n)
{
	return 1UL << fls_long(n - 1);
}

static __always_inline uint64_t hash_64(uint64_t val, unsigned int bits)
{
	uint64_t hash = val;

	/*  Sigh, gcc can't optimise this alone like it does for 32 bits. */
	uint64_t n = hash;
	n <<= 18;
	hash -= n;
	n <<= 33;
	hash -= n;
	n <<= 3;
	hash += n;
	n <<= 3;
	hash -= n;
	n <<= 4;
	hash += n;
	n <<= 2;
	hash += n;
	/* High bits are more random, so use them. */
	return hash >> (64 - bits);
}
/* Use hash_32 when possible to allow for fast 32bit hashing in 64bit kernels. */
static __always_inline uint32_t hash_32(uint32_t val, unsigned int bits)
{
	/* On some cpus multiply is faster, on others gcc will do shifts */
	uint32_t hash = val * 0x9e370001UL;
	/* High bits are more random, so use them. */
	return hash >> (32 - bits);
}

#endif /* ifndef SYMBOL */
