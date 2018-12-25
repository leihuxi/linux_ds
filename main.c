#include <stdio.h>
#include <stdlib.h>
/* #include "test_rbtree.h" */
#include "list.h"

extern int test_rbtree();
extern void test_hashtable();
extern int test_bitmap();
extern void test_list();
extern int test_kfifo();

int main()
{
    test_list();
    test_rbtree();
    test_hashtable();
    test_bitmap();
    test_kfifo();
	return 0;
}
