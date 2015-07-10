/* compile with tcc test.c -dump-identifier-names=names.txt */

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

typedef int my_type;

void main() {
	int i;
}
