#ifndef LIBTCC_H
#define LIBTCC_H

#ifndef LIBTCCAPI
# define LIBTCCAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct TCCState;

typedef struct TCCState TCCState;

/* create a new TCC compilation context */
LIBTCCAPI TCCState *tcc_new(void);

/* free a TCC compilation context */
LIBTCCAPI void tcc_delete(TCCState *s);

/* set CONFIG_TCCDIR at runtime */
LIBTCCAPI void tcc_set_lib_path(TCCState *s, const char *path);

/* set error/warning display callback */
LIBTCCAPI void tcc_set_error_func(TCCState *s, void *error_opaque,
    void (*error_func)(void *opaque, const char *msg));

/* set options as from command line (multiple supported) */
LIBTCCAPI int tcc_set_options(TCCState *s, const char *str);

/*****************************/
/* preprocessor */

/* add include path */
LIBTCCAPI int tcc_add_include_path(TCCState *s, const char *pathname);

/* add in system include path */
LIBTCCAPI int tcc_add_sysinclude_path(TCCState *s, const char *pathname);

/* define preprocessor symbol 'sym'. Can put optional value */
LIBTCCAPI void tcc_define_symbol(TCCState *s, const char *sym, const char *value);

/* undefine preprocess symbol 'sym' */
LIBTCCAPI void tcc_undefine_symbol(TCCState *s, const char *sym);

/*****************************/
/* compiling */

/* add a file (C file, dll, object, library, ld script). Return -1 if error. */
LIBTCCAPI int tcc_add_file(TCCState *s, const char *filename);

/* compile a string containing a C source. Return -1 if error. */
LIBTCCAPI int tcc_compile_string(TCCState *s, const char *buf);

/* like tcc_compile_string, but also lets you specify the length of the
 * string to compile, the filename, and the line number from whence this
 * code came. */
LIBTCCAPI int tcc_compile_string_ex(TCCState *s, const char *str, int len, const char * filename, int line_num);

/*****************************/
/* linking commands */

/* set output type. MUST BE CALLED before any compilation */
LIBTCCAPI int tcc_set_output_type(TCCState *s, int output_type);
#define TCC_OUTPUT_MEMORY   0 /* output will be run in memory (default) */
#define TCC_OUTPUT_EXE      1 /* executable file */
#define TCC_OUTPUT_DLL      2 /* dynamic library */
#define TCC_OUTPUT_OBJ      3 /* object file */
#define TCC_OUTPUT_PREPROCESS 4 /* only preprocess (used internally) */

/* equivalent to -Lpath option */
LIBTCCAPI int tcc_add_library_path(TCCState *s, const char *pathname);

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI int tcc_add_library(TCCState *s, const char *libraryname);

/* add a symbol to the compiled program */
LIBTCCAPI int tcc_add_symbol(TCCState *s, const char *name, const void *val);

/* output an executable, library or object file. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI int tcc_output_file(TCCState *s, const char *filename);

/* link and run main() function and return its value. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI int tcc_run(TCCState *s, int argc, char **argv);

/* do all relocations (needed before using tcc_get_symbol()) */
LIBTCCAPI int tcc_relocate(TCCState *s1, void *ptr);
/* possible values for 'ptr':
   - TCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. */
#define TCC_RELOCATE_AUTO (void*)1

/* return symbol value or NULL if not found */
LIBTCCAPI void *tcc_get_symbol(TCCState *s, const char *name);

/* ---- Extended symbol table management ---- */
typedef struct TokenSym* TokenSym_p;
typedef TokenSym_p (*extended_symtab_lookup_by_name_callback)(char * name, int len, void * data, int is_identifier);
typedef TokenSym_p (*extended_symtab_lookup_by_number_callback)(int tok_id, void * data, int is_identifier);
typedef void (*extended_symtab_copy_callback)(TokenSym_p* new_symtab, void * data);
LIBTCCAPI void tcc_set_extended_symtab_callbacks (
	TCCState * compiler_state,
	extended_symtab_copy_callback new_copy_callback,
	extended_symtab_lookup_by_name_callback new_name_callback,
	extended_symtab_lookup_by_number_callback new_number_callback,
	void * data
);

LIBTCCAPI int tcc_tokensym_list_length (TokenSym_p* list);
LIBTCCAPI TokenSym_p tcc_tokensym_by_tok(int tok, TokenSym_p* list);
LIBTCCAPI void tcc_delete_extended_symbol_table (TokenSym_p* my_extended_symtab);
LIBTCCAPI char * tcc_tokensym_name (TokenSym_p tokensym);
LIBTCCAPI int tcc_tokensym_tok (TokenSym_p tokensym);
LIBTCCAPI long tcc_tokensym_get_id_c(TokenSym_p tokensym);
LIBTCCAPI void tcc_tokensym_set_id_c(TokenSym_p tokensym, long new_c);
LIBTCCAPI int tcc_tokensym_no_extra_bits(int tok);
LIBTCCAPI int tcc_tokensym_has_define (TokenSym_p tokensym);
LIBTCCAPI int tcc_tokensym_has_struct (TokenSym_p tokensym);
LIBTCCAPI int tcc_tokensym_has_identifier (TokenSym_p tokensym);
LIBTCCAPI int tcc_tokensym_is_shareable (TokenSym_p tokensym);

#ifndef SYM_EXTENDED
	#define SYM_EXTENDED   0x40000000 /* extended symbol space */
#endif

#ifdef __cplusplus
}
#endif

#endif
