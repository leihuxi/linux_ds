#include "list.h"
#include "list_sort.h"
#include <stdlib.h>
#include <stdio.h>

struct a_list {
	struct list_head list;
	int val;
	const char *str;
};

int compare(void *priv, const struct list_head *a, const struct list_head *b)
{
	if (a != NULL && b != NULL) {
        struct a_list* A = container_of(a, struct a_list, list);
        struct a_list* B = container_of(b, struct a_list, list);
		return A->val > B->val;
	}
	return -1;
}

static void append(struct a_list *ptr, const char *str, int val)
{
	struct a_list *tmp;
	tmp = (struct a_list *)malloc(sizeof(struct a_list));
	if (!tmp) {
		perror("malloc");
		exit(1);
	}
	tmp->str = str;
	tmp->val = val;
	list_add_tail(&(tmp->list), &(ptr->list));
}

void test_list()
{
	struct a_list blist;
	struct a_list *iter;

	INIT_LIST_HEAD(&blist.list);

	/* add item to list */
	append(&blist, "NM", 87501);
	append(&blist, "CA", 94041);
	append(&blist, "IL", 60561);
	append(&blist, "IB", 60562);
	append(&blist, "IC", 60563);
	append(&blist, "ID", 60564);
	append(&blist, "IE", 60565);
	append(&blist, "IF", 60566);
	append(&blist, "IG", 60567);
	append(&blist, "IH", 60568);
	append(&blist, "II", 60569);
	append(&blist, "IJ", 60570);
	append(&blist, "IK", 60571);
	append(&blist, "IM", 60572);
	append(&blist, "IN", 60573);
	append(&blist, "IO", 60574);
	append(&blist, "IP", 60575);

	/* iterates list */
	list_for_each_entry (iter, &blist.list, list) {
		printf("%s %d\n", iter->str, iter->val);
	}

	list_sort(NULL, &blist.list, compare);

	list_for_each_entry (iter, &blist.list, list) {
		printf("%s %d\n", iter->str, iter->val);
	}

	/* remove all items in the list */
	while (!list_empty(&blist.list)) {
		iter = list_entry(blist.list.next, struct a_list, list);
		list_del(&iter->list);
		free(iter);
	}
}
