/*
 * Share struct definitions between two contexts in which the struct name
 * is a semi-reserved word, i.e. something that comes pre-declared in all
 * compiler contexts. These are the sorts of things declared in tcctok.h
 * that are not actually keywords, like "file".
 */

/* uncomment to enable diagnostic output */
//	#define DIAG(...) diag(__VA_ARGS__)

#define INCLUDE_MALLOC
#include "test_setup.h"

char first_code[] =
"void * malloc(int);\n"
"void free(void *);\n"
"struct file {\n"
"    int length;\n"
"    char string[0];\n"
"};\n"
"void * new_filename(char * word) {\n"
"    int i, length;\n"
"    struct file * f;\n"

     /* Get length of word */
"    for (length = 0; word[length] != 0; length++);\n"
"    length--;\n"

"    f = malloc(sizeof(struct file) + length);\n"
"    f->length = length;\n"
"    for (i = 0; i <= length; i++) f->string[i] = word[i];\n"
"    return f;\n"
"}\n"
;

char second_code[] =
"int get_file_length(struct file * f) {\n"
"    return f->length;\n"
"}\n"
;

int main(int argc, char **argv) {
	
	/* ---- Compile the first code string and setup the callback data ---- */
	
	TCCState *s1 = tcc_new();
	extended_symtab_p my_symtab;
	setup_and_compile_s1(my_symtab, first_code);
	SETUP_SECOND_CALLBACK_DATA();
	
	/* ---- Allocate a point and manually unpack it ---- */
	void* (*allocate_ptr)(char*) = tcc_get_symbol(s1, "new_filename");
	if (allocate_ptr == NULL) return 1;
	void * file_p = allocate_ptr("foobar.txt");
	if (file_p == NULL) {
		fail("Unable to allocate file struct");
		return 1;
	}
	else pass("Allocated file");
	int * manual_unpack_size = file_p;
	is_i(*manual_unpack_size, 9, "manually unpacked length");
	char * manual_unpack_name = (char*) file_p + sizeof(int);
	is_s(manual_unpack_name, "foobar.txt", "manually unpacked file name");
	
	/* ---- Compile the second compiler context ---- */
	TCCState * s_second = tcc_new();
	setup_and_relocate_second_state(s_second, second_code);
	int (*get_file_length)(void*) = tcc_get_symbol(s_second, "get_file_length");
	if (get_file_length == NULL) return 1;
	is_i(get_file_length(file_p), 9, "Second context able to unpack struct");
	
	/* ---- clean up the memory ---- */
	tcc_delete_extended_symbol_table(my_symtab);
	tcc_delete(s1);
	tcc_delete(s_second);
	free(file_p);
	pass("cleanup");
	
	done_testing();
	
	return 0;
}
