#include "bitmap.h"
#include <stdio.h>

#define BIT_CNT 65
int test_bitmap()
{
	long unsigned int *my_bitmap;
	int arr_size = (BIT_CNT - 1) / 64 + 1;
	int i;

	printf("init %d bit, %d long int\n", BIT_CNT, arr_size);
	my_bitmap = bitmap_alloc(arr_size);

	for (i = 0; i < arr_size; i++) {
		printf("bitmap[%d] = %lx\n", i, my_bitmap[i]);
	}

	printf("set_bit(64, my_bitmap)\n");
	set_bit(64, my_bitmap);
	for (i = 0; i < arr_size; i++) {
		printf("bitmap[%d] = %lx\n", i, my_bitmap[i]);
	}

	printf("test_bit(64, my_bitmap)\n");
	if (test_bit(64, my_bitmap) == 1)
		printf("test bit is true\n");
	else
		printf("test bit is false\n");

	printf("clear_bit(64, my_bitmap)\n");
	clear_bit(64, my_bitmap);
	for (i = 0; i < arr_size; i++) {
		printf("bitmap[%d] = %lx\n", i, my_bitmap[i]);
	}

	printf("test_bit(64, my_bitmap)\n");
	if (test_bit(64, my_bitmap) == 1)
		printf("test bit is true\n");
	else
		printf("test bit is false\n");
	i = bitmap_find_next_zero_area(my_bitmap, BIT_CNT, 0, 1, 0);
	printf("find zero zrea:%d\n", i);
	bitmap_free(my_bitmap);
	return 0;
}
