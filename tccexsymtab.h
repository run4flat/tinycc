/*
 *  TCC - Tiny C Compiler
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "tcc.h"

/* increment this whenever the cache file format changes */
#define EXSYMTAB_CACHE_FORMAT_VERSION 1

/*****************************************************************************/
/*                            exsymtab_token_hash                            */
/*****************************************************************************/

typedef struct token_string_hash_linked_list {
    struct token_string_hash_linked_list * next;
    void * data;
    char name[1];
} token_string_hash_linked_list;

typedef struct {
    unsigned int N;
    unsigned int N_buckets;
    token_string_hash_linked_list ** buckets;
} token_string_hash;

token_string_hash * token_string_hash_new();
void ** token_string_hash_get_ref(token_string_hash * tsh, const char * name);
void token_string_hash_free(token_string_hash * tsh);

/****************************************************************************/
/*                                 ram hash                                 */
/****************************************************************************/

struct ram_hash_linked_list {
    struct ram_hash_linked_list * next;
    void * key;
    void * value;
};
typedef struct ram_hash_linked_list ram_hash_linked_list;

typedef struct {
    unsigned int N;
    unsigned int N_buckets;
    ram_hash_linked_list * buckets;
} ram_hash;

ram_hash * ram_hash_new();
void ** ram_hash_get_ref(ram_hash * rh, void * old);
void ** ram_hash_iterate(ram_hash * rh, void ** p_next_data);
void ram_hash_free(ram_hash * rh);

/*****************************************************************************/
/*                     exsymtab Sym and TokenSym Structs                     */
/*****************************************************************************/

/* Identical to struct Sym in tcc.h, but trimmed for identifier and struct
 * Syms: does not contain the prev and prev_tok fields; removes some union
 * members. */
struct exsymtabSymBase; /* for type-punning both exsymtabSym and exsymtabDef */
typedef struct exsymtabSymBase {
	struct exsymtabSym *next; /* next related symbol (for fields and anoms) */
    int v; /* symbol token */
} exsymtabSymBase;

struct exsymtabSym;
typedef struct exsymtabCType {
    int t;
    struct exsymtabSym *ref;
} exsymtabCType;
typedef struct exsymtabSym {
	struct exsymtabSym *next; /* next related symbol (for fields and anoms) */
    int v; /* symbol token */
    unsigned short r; /* associated register or VT_CONST/VT_LOCAL and LVAL type */
    struct SymAttr a; /* symbol attributes */
    union {
        struct {
            int c; /* associated number or Elf symbol index */
			struct FuncAttr f; /* function attributes */
        };
        long long enum_val; /* enum constant if IS_ENUM_VAL */
    };
    struct exsymtabCType type; /* associated type */
} exsymtabSym;


/* Identical to struct Sym in tcc.h, but trimmed for define symbols:
 * does not contain the prev and prev_tok fields; removes some union
 * members. */
struct exsymtabDef;
typedef struct exsymtabDef {
    struct exsymtabDef *next; /* tokens for arguments for function-like macros */
    int v; /* symbol token */
    int type_t; /* associated type */
	int *d; /* define token stream */
} exsymtabDef;

/* Should be identical to struct TokenSym in tcc.h, except it uses the special
 * Sym structs declared above. */
typedef struct exsymtabTokenSym {
    void *global_sym;            /* pointer to global identifier's memory */
    exsymtabDef *sym_define;      /* direct pointer to define */
    exsymtabSym *sym_struct;     /* direct pointer to structure */
    exsymtabSym *sym_identifier; /* direct pointer to identifier */
    int tok; /* token number */
    int len;
    char str[1];
} exsymtabTokenSym;

typedef struct exsymtabInlineFunc {
    TokenString *func_str;
    exsymtabSym *sym;
    char filename[1];
} exsymtabInlineFunc;

/******************************************************************************/
/*                           extended symtab struct                           */
/******************************************************************************/

typedef struct extended_symtab {
    union {
        ram_hash * sym_rh;
        exsymtabSym * sym_list;
    };
    union {
        ram_hash * def_rh;
        exsymtabDef * def_list;
    };
    token_string_hash * tsh;
    int N_syms; /* zero for Sym collections stored in ram_hash */
    int N_defs; /* zero for Sym collections stored in ram_hash */
    int tok_start;
    int tok_start_offset;
    int N_inline_funcs;
    exsymtabInlineFunc ** inline_funcs;
    exsymtabTokenSym ** tokenSym_last;
    exsymtabTokenSym * tokenSym_list [1];
} extended_symtab;

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state);
void copy_global_identifiers_to_exsymtab(TCCState *state);
/* tcc_get_extended_symbol_table in libtcc.h */
/* tcc_get_extended_tokensym in libtcc.h */
/* tcc_get_extended_symbol in libtcc.h */
/* tcc_get_next_extended_symbol_name in libtcc.h */

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

/* tcc_set_extended_symtab_callbacks is in libtcc.h */
/* tcc_save_extended_symtab is in libtcc.h */

exsymtabSym * get_new_symtab_pointer (Sym * old, ram_hash * rh);
exsymtabDef * get_new_deftab_pointer (Sym * old, ram_hash * rh);
int tokenstream_copy (int * stream, int * to_stream, extended_symtab * symtab);
#define tokenstream_len(stream) tokenstream_copy(stream, 0, 0)
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start);
LIBTCCAPI void tcc_delete_extended_symbol_table (extended_symtab * symtab);
LIBTCCAPI int tcc_extended_symtab_test(extended_symtab * symtab, int to_test, const char * name);

/* tcc_get_extended_tokensym declared in libtcc.h */
/* tcc_get_extended_symbol declared in libtcc.h */

/*****************************************************************************/
/*                      Pre-compilation TokenSym Prep                        */
/*****************************************************************************/

/* Most TokenSyms are only applied from an extended symbol table when
 * they are explicitly used in the consuming compiler. The use-on-demand
 * behavior works by adding a default unknown-token handler to
 * next_nomacro1 which queries the extended symbol tables when an
 * unknown token is encountered. There are some symbols with token ids
 * that are not part of the C language spec and which are common words.
 * These include "push" and "pop", among others. These can be used as
 * valid identifiers, but will never trip the unknown-token handler code
 * because the token is known. In order for extended symbol tables to
 * provide identifiers with these tokens, they have to be applied at the
 * beginning of the compilation process. This function does that. */
LIBTCCAPI void tcc_prep_tokensym_list(extended_symtab * symtab);

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

void local_stack_off();
void local_stack_on();
ST_FUNC TokenSym** symtab_tok_find(const char *str, int len); /* in tccpp.c */
TokenSym * get_local_ts_for_extended_ts(exsymtabTokenSym* orig_symtab_ts, extended_symtab* orig_symtab);
Sym * copy_extended_sym (extended_symtab* symtab, exsymtabSym * from, int to_tok);
void copy_extended_tokensym (extended_symtab* symtab, exsymtabTokenSym * from, TokenSym * to);
int get_local_tok_for_extended_tok(int orig_tok, extended_symtab* symtab);
TokenSym * get_local_tokensym_for_extended_tok(int tok, extended_symtab * symtab);

/*****************************************************************************/
/*                      Extended Symbol Table Caching                        */
/*****************************************************************************/

/* tcc_set_extended_symbol is in libtcc.h; this assumes that the token exists in the tsh */
/* tcc_deserialize_extended_symtab declared in libtcc.h */
/* tcc_serialize_extended_symtab declared in libtcc.h */
