/*
 * Tests to ensure that the ram_tree works as indicated.
 */

#include "libtcc.h"
#include "tcc.h"
#include "tap.h"
#include <stdlib.h>

void add_some_data(ram_tree * rt) {
    /* Add some data to semi-random slots */
    void ** data_ref = ram_tree_get_ref(my_rt, (void*)0x0010);
    *data_ref = (void*) 1;
    data_ref = ram_tree_get_ref(my_rt, (void*)0x1010);
    *data_ref = (void*) 2;
    data_ref = ram_tree_get_ref(my_rt, (void*)0x1100);
    *data_ref = (void*) 3;
    data_ref = ram_tree_get_ref(my_rt, (void*)0x1111);
    *data_ref = (void*) 4;
}

void check_retrieve(ram_tree rt, int old, int expected, char * message) {
    void ** data_ref = ram_tree_get_ref(my_rt, (void*)old);
    is((int)*data_ref, expected, message);
}

int main(int argc, char **argv) {
    /* create a ram_tree */
    ram_tree * my_rt = ram_tree_new();
    if (my_rt == NULL) return 1;
    pass("Built empty ram tree");
    
    /* Add some data */
    add_some_data(my_rt);
    pass("Added data without crashing");
    
    /* Clean up */
    ram_tree_free(my_rt);
    pass("Freed memory without crashing");
    
    /* At this point, I trust those basic commands. Now we make sure
     * that the data is accessible again. */
    my_rt = ram_tree_new();
    add_some_data(my_rt);
    check_retrieve(my_rt, 0x0010, 1, "First element can be retrieved by pointer address");
    check_retrieve(my_rt, 0x1111, 4, "Fourth element can be retrieved by pointer address");
    check_retrieve(my_rt, 0x1100, 3, "Third element can be retrieved by pointer address");
    check_retrieve(my_rt, 0x1010, 2, "Second element can be retrieved by pointer address");
    
    /* How about iteration? */
    int found[4] = {0, 0, 0, 0};
    void * iterator_data = NULL;
    do {
		void ** data_ref = ram_tree_iterate(my_rt, &iterator_data);
		int value = (int)*data_ref;
		found[value - 1]++;
	} while (iterator_data != NULL);
    
	is(found[0], 1, "One reference to first element");
    is(found[1], 1, "One reference to second element");
    is(found[2], 1, "One reference to third element");
    is(found[3], 1, "One reference to fourth element");
    
    /* clean up */
    ram_tree_free(my_rt);
	
	done_testing();
	
	return 0;
}
