/* compile with tcc test.c -dump-identifier-names=names.txt */

#include <stdio.h>

enum {
	first,
	second,
} my_list;
static const int foo = 5;
int * p_val;
extern int some_value;
struct {
	int one;
	double two;
} thing;
static int blah;
int cabinet;
const int ruler = 12;

typedef int my_type;

void main() {
	int i;
	i = 5;
	if(__builtin_expect(i, 5))
		printf("Value is %d\n", i);
	else
		printf("Weird!\n");
}
