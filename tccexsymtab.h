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

/******************************************************************************/
/*                           extended symtab struct                           */
/******************************************************************************/

typedef struct extended_symtab {
    union {
        ram_hash * sym_rh;
        Sym * sym_list;
    };
    union {
        ram_hash * def_rh;
        Sym * def_list;
    };
    token_string_hash * tsh;
    int N_syms; /* zero for Sym collections stored in ram_hash */
    int N_defs; /* zero for Sym collections stored in ram_hash */
    int tok_start;
    int tok_start_offset;
    int N_inline_funcs;
    InlineFunc ** inline_funcs;
    TokenSym ** tokenSym_last;
    TokenSym * tokenSym_list [1];
} extended_symtab;

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state);
void copy_extended_symbols_to_exsymtab(TCCState *state);
/* tcc_get_extended_symbol_table in libtcc.h */
/* tcc_get_extended_tokensym in libtcc.h */
/* tcc_get_extended_symbol in libtcc.h */
/* tcc_get_next_extended_symbol_name in libtcc.h */

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

/* tcc_set_extended_symtab_callbacks is in libtcc.h */
/* tcc_save_extended_symtab is in libtcc.h */

Sym * get_new_symtab_pointer (Sym * old, ram_hash * rh);
Sym * get_new_deftab_pointer (Sym * old, ram_hash * rh);
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

LIBTCCAPI void tcc_prep_tokensym_list(extended_symtab * symtab);

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

void local_stack_off();
void local_stack_on();
ST_FUNC TokenSym** symtab_tok_find(const char *str, int len); /* in tccpp.c */
TokenSym * get_local_ts_for_extended_ts(TokenSym* orig_symtab_ts, extended_symtab* orig_symtab);
Sym * copy_extended_sym (extended_symtab* symtab, Sym * from, int to_tok);
void copy_extended_tokensym (extended_symtab* symtab, TokenSym * from, TokenSym * to);
void copy_ctype(CType * to_type, Sym * from, extended_symtab*symtab);
int get_local_tok_for_extended_tok(int orig_tok, extended_symtab* symtab);

/*****************************************************************************/
/*                      Extended Symbol Table Caching                        */
/*****************************************************************************/

/* tcc_set_extended_symbol is in libtcc.h; this assumes that the token exists in the tsh */
/* tcc_deserialize_extended_symtab declared in libtcc.h */
/* tcc_serialize_extended_symtab declared in libtcc.h */
