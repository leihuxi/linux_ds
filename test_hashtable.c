#include "hashtable.h"
#include <stdio.h>

struct object {
	int id;
	char name[16];
	struct hlist_node node;
};

void test_hashtable()
{
	// define a hash table with 2^3(=8) buckets
	DEFINE_HASHTABLE(htable, 3);
	// => struct hlist_head htable[8] = { [0 ... 7] = HLIST_HEAD_INIT };

	struct object obj1 = {
		.id = 1,
		.name = "obj1",
	};

	hash_add(htable, &obj1.node, obj1.id);

	struct object obj2 = {
		.id = 2,
		.name = "obj2",
	};
	hash_add(htable, &obj2.node, obj2.id);

	struct object obj3 = {
		.id = 3,
		.name = "obj3",
	};
	hash_add(htable, &obj3.node, obj3.id);

	struct object obj9 = {
		.id = 9,
		.name = "obj9",
	};
	hash_add(htable, &obj9.node, obj9.id);

	int key = 1;
	struct object *obj;
	hash_for_each_possible (htable, obj, node, key) {
		if (obj->id == key) {
			printf("hash key=%d => %s\n", key, obj->name);
		}
	}
}
