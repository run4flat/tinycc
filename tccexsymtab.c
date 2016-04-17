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

/* tccgen: asm_label cleanu */
//#define BEFORE_grishka
//#define BEFORE_hash_opt

#ifdef BEFORE_grishka
  #define FUNC_STR token_str
#else
  #define FUNC_STR func_str.str
#endif

/*****************************************************************************/
/*                            exsymtab_token_hash                            */
/*****************************************************************************/

/* algorithm djb2 from http://www.cse.yorku.ca/~oz/hash.html */
unsigned long _token_hash_hash_string(token_string_hash * tsh, const char *str)
{
    unsigned long hash = 5381;
    int c;

    while ((c = *str++))
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash & (tsh->N_buckets - 1);
}

token_string_hash * token_string_hash_new()
{
    /* Allocate enough room for the hash's internal data, and four
     * buckets (which means three extra buckets) */

    token_string_hash * to_return = tcc_mallocz(sizeof(token_string_hash));
    to_return->buckets = tcc_mallocz(4 * sizeof(void*));
    to_return->N_buckets = 4;
    return to_return;
}

/* token_string_hash_find: internal function. Returns a reference
 * (pointer) to the address of the linked list element associated with
 * the given name. It may be that the element does not exist, in which
 * case dereferencing will yeild a null pointer. The memory location
 * itself is guaranteed to exist, so you can allocate new memory and
 * store a new value if so desired. */

token_string_hash_linked_list** _token_string_hash_get_ll_ref(
    token_string_hash * tsh, const char * name
) {
    /* find the associated bucket */
    token_string_hash_linked_list ** to_return
        = tsh->buckets + _token_hash_hash_string(tsh, name);

    /* Check the names of all elements in the bucket until we find it */
    while(*to_return) {
        if (strcmp((*to_return)->name, name) == 0) return to_return;
        to_return = &((*to_return)->next);
    }
    return to_return;
}

void _token_string_hash_extend(token_string_hash * tsh)
{
    /* Back up the old buckets */
    int n;
    token_string_hash_linked_list ** old_buckets = tsh->buckets;
    int old_N_buckets = tsh->N_buckets;

    /* Extend the new buckets by a factor of 2 */
    tsh->N_buckets <<= 1;
    tsh->buckets = tcc_mallocz(tsh->N_buckets * sizeof(void*));

    /* rehash the data */
    for (n = 0; n < old_N_buckets; n++) {
        token_string_hash_linked_list * curr_ll = old_buckets[n];
        while(curr_ll != NULL) {
            token_string_hash_linked_list * tmp;

            /* Make the new slot point to this linked list object */
            *(_token_string_hash_get_ll_ref(tsh, curr_ll->name)) = curr_ll;

            /* Make sure the old "next" is cleared, since it probably
             * won't be in this bucket after rehashing. */
            tmp = curr_ll->next;
            curr_ll->next = NULL;

            /* On to the old "next" */
            curr_ll = tmp;
        }
    }
    tcc_free(old_buckets);
}

/* Returns a reference to the data slot of the token string hash entry
 * for the given name, creating the hash entry if necessary. */

void ** token_string_hash_get_ref(token_string_hash * tsh, const char * name)
{
    token_string_hash_linked_list** ll_slot = _token_string_hash_get_ll_ref(tsh, name);
    token_string_hash_linked_list* return_container = *ll_slot;

    /* create a new entry if necessary */
    if (return_container == NULL)
    {
        return_container = tcc_mallocz(sizeof(token_string_hash_linked_list) + strlen(name));
        strcpy(return_container->name, name);
        *ll_slot = return_container;

        /* Rehash if too big; note rehashing does not invalidate return_container */
        if (++tsh->N > tsh->N_buckets) _token_string_hash_extend(tsh);
    }
    return &(return_container->data);
}

/* string_string_hash_count: returns the number of elements in the hash table */
int token_string_hash_count(token_string_hash * tsh) {
    return tsh->N;
}

void token_string_hash_free(token_string_hash * tsh)
{
    int n;
    if (tsh == NULL) return;
    for (n = 0; n < tsh->N_buckets; n++) {
        token_string_hash_linked_list * curr_ll = tsh->buckets[n];
        while(curr_ll != NULL) {
            token_string_hash_linked_list * tmp = curr_ll->next;
            tcc_free(curr_ll);
            curr_ll = tmp;
        }
    }
    tcc_free(tsh->buckets);
    tcc_free(tsh);
}

/****************************************************************************/
/*                                 ram hash                                 */
/****************************************************************************/

/* This provides a mechanism for mapping a set of old pointers to a set of
 * new pointers. Assuming that a collection of data structures are being
 * copied, this basically provides an interface to say, "What is the new
 * address for this old address?"
 * 
 * The current hashing function is pretty basic, taken from this discussion:
 * http://stackoverflow.com/questions/20953390/what-is-the-fastest-hash-function-for-pointers
 * However, it performs pretty well. With perl.h, the maximum bucket depth
 * is 6, and on average each bucket has about 1.3 entries.
 * 
 * As currently implemented, you should create a new ram_hash with
 * ram_hash_new and free the memory associated with your ram_hash with
 * ram_hash_free:
 * 
 *  ram_hash * my_ram_hash = ram_hash_new();
 * 
 */

ram_hash_linked_list * ram_hash_get(ram_hash * rh, void * key);

ram_hash * ram_hash_new()
{
    ram_hash * to_return = tcc_mallocz(sizeof(ram_hash));
    to_return->N_buckets = 4;
    to_return->buckets = tcc_mallocz(4*sizeof(ram_hash_linked_list));

    /* Add entry for null pointer */
    ram_hash_get(to_return, NULL)->value = NULL;
    return to_return;
}

/* ram_hash_hash_ptr: internal function. Returns the bucket offset
 * for a given pointer, i.e. it hashes the pointer value. */

uintptr_t ram_hash_hash_ptr(ram_hash * rh, void * old)
{
    uintptr_t hashed = (uintptr_t)old;

    /* shift and mask out bits we don't want */
    return (hashed >> 5) & (rh->N_buckets - 1);
}

/* ram_hash_find: internal function. Returns the ram_hash_linked_list
 * element for a given pointer, if the pointer is already in the hash. */

ram_hash_linked_list* ram_hash_find(ram_hash * rh, void * old)
{
    /* find the associated bucket */
    ram_hash_linked_list * to_return
        = rh->buckets + ram_hash_hash_ptr(rh, old);

    while(to_return) {
        if (to_return->key == old) return to_return;
        to_return = to_return->next;
    }
    return NULL;
}

/* ram_hash_get: Internal function. Returns the ram_hash_linked_list
 * element for the given key, creating if necessary. This does not
 * check if the hash has to be rehashed, which is why it is internal
 * only. */

ram_hash_linked_list * ram_hash_get(ram_hash * rh, void * key)
{
    /* find the associated bucket */
    ram_hash_linked_list * curr_el
        = rh->buckets + ram_hash_hash_ptr(rh, key);

    if (curr_el->key == NULL || curr_el->key == key) {
        curr_el->key = key;
        return curr_el;
    }
    while(curr_el->next != NULL) {
        curr_el = curr_el->next;
        if (curr_el->key == key)
            return curr_el;
    }
    /* out here, curr_el->next is null, so allocate a new element */
    curr_el->next = tcc_mallocz(sizeof(ram_hash_linked_list));
    curr_el->next->key = key;
    return curr_el->next;
}

/* ram_hash_rehash: internal function. Given a ram_hash, increments the
 * number of log_buckets and rehashes the contents. */

void ram_hash_rehash(ram_hash * rh)
{
    /* back up old-bucket data */
    int i;
    int old_N_buckets = rh->N_buckets;
    ram_hash_linked_list * old_buckets = rh->buckets;

    /* Allocate new buckets */
    rh->N_buckets <<= 1;
    rh->buckets
        = tcc_mallocz(rh->N_buckets * sizeof(ram_hash_linked_list));

    /* Add everything */
    for (i = 0; i < old_N_buckets; i++) {
        ram_hash_linked_list * curr;
        ram_hash_linked_list * next;
        ram_hash_linked_list * bucket_head = old_buckets + i;

        /* add the head, or move on if empty */
        if (bucket_head->key == NULL) continue;
        ram_hash_get(rh, bucket_head->key)->value = bucket_head->value;

        /* move on if there are no non-head elements */
        if (bucket_head->next == NULL) continue;

        /* process non-head elements */
        curr = bucket_head->next;
        do {
            next = curr->next;
            ram_hash_get(rh, curr->key)->value = curr->value;
            tcc_free(curr);
            curr = next;
        } while(curr != NULL);
    }
    tcc_free(old_buckets);
}

/* ram_hash_get_ref: returns a *reference* to the data slot for the
 * given a pointer address that you want mapped, creating the slot if
 * necessary. The reason this returns a reference to the pointer rather
 * than the pointer itself is so that you can work with the result as an
 * lvalue.
 * 
 *  void ** p_data = ram_hash_get_ref(my_ram_hash, old_ptr);
 *  if (*p_data == NULL) {
 *      *p_data = create_new_data();
 *  }
*/

void ** ram_hash_get_ref(ram_hash * rh, void * old)
{
    /* Does it already exist? */
    ram_hash_linked_list * container = ram_hash_find(rh, old);
    if (container != NULL) return &(container->value);

    /* No. Rehash if the buckets are full */
    if (rh->N == rh->N_buckets - 1) ram_hash_rehash(rh);
    rh->N++;

    /* Add the element and return the result */
    container = ram_hash_get(rh, old);
    return &(container->value);
}

/* ram_hash_describe: semi-internal, describes hash table statistics */

void ram_hash_describe(ram_hash * rh)
{
    int N_filled = 0;
    int max_occupancy = 0;
    int i;

    printf("Ram tree has %d buckets for %d elements\n", rh->N_buckets, rh->N);
    for (i = 0; i < rh->N_buckets; i++) if(rh->buckets[i].key != NULL)
    {
        ram_hash_linked_list * curr;
        int this_occupancy = 1;

        N_filled++;
        curr = rh->buckets + i;
        while(curr->next != NULL) {
            this_occupancy++;
            curr = curr->next;
        }
        if (max_occupancy < this_occupancy) max_occupancy = this_occupancy;
    }
    printf("%d buckets are filled, with an average occupancy of %f\n",
        N_filled, (float)rh->N / (float)N_filled);

    printf("Maximum occupancy is %d\n", max_occupancy);
}

/* ram_hash_count: returns the number of elements in the hash table */
int ram_hash_count(ram_hash * rh) {
    return rh->N;
}

/* void ** ram_hash_iterate(void * ram_hash, void ** iter_data)
 * Iterates through the ram_hash data, returning a reference to new leaf
 * with each call. The void ** iter_data is a reference to a void pointer
 * that is used by the iterator to store state between calls. You should
 * call this function like so:
 * 
 *  void * iter_data = NULL;
 *  do {
 *      void ** data_ref = ram_hash_iterate(ram_hash, &iter_data);
 *      ...
 *  } while (iter_data != NULL);
 * 
 * For example, to count the number of entries, you could do this
 * 
 *  void * iter_data = NULL;
 *  int count = 0;
 *  do {
 *      count++;
 *      ram_hash_iterate(ram_hash, &iter_data);
 *  } while (iter_data != NULL;
 * 
 * To free data referenced by all leaf pointers, use this
 * 
 *  void * iter_data = NULL;
 *  void ** ptr_ref;
 *  do {
 *      ptr_ref = ram_hash_iterate(ram_hash, &iter_data);
 *      free(*ptr_ref);
 *  } while (iter_data != NULL;
 *
 * State is allocated on the heap. For the moment, the only way to free
 * the state information is to iterate through all of the data.
 */

/* points to the *next* element, or is set to null */
typedef struct {
    int bucket;
    ram_hash_linked_list * next;
} rt_next_data;

void ** ram_hash_iterate(ram_hash * rh, void ** p_next_data)
{
    int i;
    rt_next_data * next_data;
    void ** to_return;

    if (rh == NULL) return NULL;
    if (rh->N == 0) return NULL;

    /* dereference the pointer they passed in */
    next_data = *p_next_data;

    /* If the next data is not initialized, then we allocate memory
     * and point it to the first element. */

    if (next_data == NULL) {
        next_data = tcc_mallocz(sizeof(rt_next_data));
        *p_next_data = next_data;
        for (i = 0; i < rh->N_buckets; i++) {
            if (rh->buckets[i].key != NULL) {
                next_data->bucket = i;
                next_data->next = rh->buckets + i;
                break;
            }
        }
    }

    /* hold on to the address to return */
    to_return = &(next_data->next->value);

    /* move next_data forward */
    if (next_data->next->next != NULL) {
        next_data->next = next_data->next->next;
        return to_return;
    }

    for (i = next_data->bucket + 1; i < rh->N_buckets; i++) {
        if (rh->buckets[i].key != NULL) {
            next_data->bucket = i;
            next_data->next = rh->buckets + i;
            return to_return;
        }
    }

    /* out here means we need to free the next_data */
    tcc_free(next_data);
    *p_next_data = NULL;
    return to_return;
}

/* ram_hash_free(void * ram_hash)
 * Frees memory associated with a ram_hash. Does not do anything with
 * the leaves. Use ram_hash_iterate to go through the leaves and take
 * care of memory allocations stored there.
 */

void ram_hash_free(ram_hash * rh)
{
    int i;

    if (rh == NULL) return;
    for (i = 0; i < rh->N_buckets; i++) {
        ram_hash_linked_list * curr;
        if (rh->buckets[i].next == NULL) continue;

        curr = rh->buckets[i].next;
        do {
            ram_hash_linked_list * next = curr->next;
            tcc_free(curr);
            curr = next;
        } while(curr != NULL);
    }
    tcc_free(rh->buckets);
    tcc_free(rh);
}

/******************************************************************************/
/*                           compiled symbol lookup                           */
/******************************************************************************/

void dump_sym_names(TCCState *state)
{
    Section * s;
    ElfW(Sym) *sym;
    int sym_index;
    const char *name;

    s = state->symtab;
    sym_index = 2;
    sym = &((ElfW(Sym) *)s->data)[sym_index];
    name = s->link->data + sym->st_name;
    while (strcmp("_etext", name) != 0) {
        printf("%s: sym_index = %d, st_shndx is %x, address is %p\n", name, sym_index, sym->st_shndx, (void*)sym->st_value);
        sym_index++;
        sym = &((ElfW(Sym) *)s->data)[sym_index];
        name = s->link->data + sym->st_name;
    }
}

char * type_lookup_table[16] = {
    "int", "char", "short", "void",
    "pointer", "enum", "func", "struct",
    "float", "double", "long double", "bool",
    "long long", "long", "qlong", "qfloat"
};

void tcc_dump_identifier_names(extended_symtab * symtab, char * outfile)
{
    int i;
    FILE * out_fh = fopen(outfile, "w");

    /* report error? I think a lack of file will probably be sufficient */
    if (!out_fh) return;

    for (i = 0; symtab->tokenSym_list + i < symtab->tokenSym_last; i++) {
        int btype;
        TokenSym * ts;
        Sym * curr_sym;

        ts = symtab->tokenSym_list[i];
        if (!ts->sym_identifier) continue;
        curr_sym = ts->sym_identifier;

        /* only indicate the things that have external linkage */
        if ((curr_sym->type.t & (VT_EXTERN | VT_STATIC)) != VT_EXTERN) continue;
        if (curr_sym->type.t & VT_TYPEDEF) continue;

        /* name */
        fprintf(out_fh, "%s ", ts->str);

        /* qualifiers */
        if (curr_sym->type.t & VT_CONSTANT) fprintf(out_fh, "constant ");

        /* type */
        btype = curr_sym->type.t & VT_BTYPE;
        fprintf(out_fh, "%s\n", type_lookup_table[btype]);
    }
    fclose(out_fh);
}

/* tcc_get_next_extended_symbol_name: a simple mechanism for getting the names
 * of all of the global symbols known to the extended symbol table. */

char * tcc_get_next_extended_symbol_name(extended_symtab * symtab, int * poffset)
{
    /* Increment the counter to get to the next TokenSym */
    for ((*poffset)++; symtab->tokenSym_list + *poffset < symtab->tokenSym_last; (*poffset)++)
    {
        TokenSym * ts = symtab->tokenSym_list[*poffset];
        if (ts->sym_identifier) return ts->str;
    }

    /* Reached end of list. Reset the counter and return null */
    *poffset = -1;
    return NULL;
}

void copy_extended_symbols_to_exsymtab(TCCState *state)
{
    Section * s;
    ElfW(Sym) *sym;
    int sym_index;
    const char *name;
    extended_symtab* exsymtab;

    exsymtab = state->exsymtab;
    s = state->symtab;
    sym_index = 2;
    sym = &((ElfW(Sym) *)s->data)[sym_index];
    name = s->link->data + sym->st_name;
    while (strcmp("_etext", name) != 0) {
        if (name[0] == 'L' && name[1] == '.') {
            /* Skip constants */
        }
        else {
            /* Copy the symbol's pointer into the hash_next field of the TokenSym */
            TokenSym * ts = tcc_get_extended_tokensym(exsymtab, name);
            if (ts == NULL) {
                tcc_warning("Global symbol %s does not exist in extended symbol table; not copying\n",
                    name);
            }
            else {
                ts->hash_next = (void*)sym->st_value;
            }
        }
        /* Next iteration */
        sym_index++;
        sym = &((ElfW(Sym) *)s->data)[sym_index];
        name = s->link->data + sym->st_name;
    }
}

/* A value of NULL for exsymtab means that the extended symtab was not supposed
 * to be generated in the first place. A value of 1 means that it is supposed to
 * be created, but the state hasn't compiled yet. Otherwise, we have a fully
 * formed extended symbol table, which we can return. In that case, we assume
 * that the user takes responsibility for cleaning it up. */

LIBTCCAPI extended_symtab * tcc_get_extended_symbol_table(TCCState * s)
{
    extended_symtab * to_return;
    if (s->exsymtab <= (extended_symtab*)1) return NULL;

    /* clear the pointer value; otherwise we would free it, leading to a
     * double-free situation when the user also frees it. */

    to_return = s->exsymtab;
    s->exsymtab = (extended_symtab*)1;
    return to_return;
}

LIBTCCAPI TokenSym* tcc_get_extended_tokensym(extended_symtab* symtab, const char * name)
{
    /* delegate to the symtab's trie */
    return (TokenSym*)(*token_string_hash_get_ref(symtab->tsh, name));
}

LIBTCCAPI void * tcc_get_extended_symbol(extended_symtab * symtab, const char * name)
{
    TokenSym * ts = tcc_get_extended_tokensym(symtab, name);
    if (ts == NULL) return NULL;
    return (void*) ts->hash_next;
}

/******************************************************************************/
/*                            extended symtab copy                            */
/******************************************************************************/

/* The user may want fine-grained control over the order of symbol table lookup.
 * Thus, I provide a set of callbacks to look for names, add symbols to compiler
 * contexts, and prep the compiler state before things get started. */

LIBTCCAPI void tcc_set_extended_symtab_callbacks (
    TCCState * s,
    extended_symtab_lookup_by_name_callback new_name_callback,
    extended_symtab_sym_used_callback new_sym_used_callback,
    extended_symtab_prep_callback new_prep_callback,
    void * data
) {
    s->symtab_name_callback = new_name_callback;
    s->symtab_sym_used_callback = new_sym_used_callback;
    s->symtab_prep_callback = new_prep_callback;
    s->symtab_callback_data = data;
}

LIBTCCAPI void tcc_save_extended_symtab(TCCState * s) {
    if (s->exsymtab == NULL) s->exsymtab = (extended_symtab*)1;
}

Sym * get_new_symtab_pointer (Sym * old, ram_hash * rh)
{
    void ** Sym_ref;
    Sym * to_return;
    int btype;

    /* Handle the null case up-front */
    if (old == NULL) return NULL;

    /* Check the global symbol stack. */
    Sym_ref = ram_hash_get_ref(rh, old);
    to_return = *Sym_ref;
    if (NULL != to_return) return to_return;

    /* Create new sym. Note that mallocz sets lots of things to null
     * for me. :-) */
    to_return = *Sym_ref = tcc_mallocz(sizeof(Sym));

    /* See tcc.h around line 425 for descriptions of some of the fields.
     * See also tccgen.c line 5987 to see what needs to happen for function
     * declarations to work properly (and, in turn, line 446 for how to
     * push a forward reference). */

    /* Copy the v value (token id). This will not be copied later, so keep
     * things simple for now and simply strip out the extended flag. */
    to_return->v = old->v & ~SYM_EXTENDED;
 
#ifdef BEFORE_grishka
    /* Copy the assembler label */
    if (old->asm_label != NULL) {
        int asm_label_len = strlen(old->asm_label) + 1;
        to_return->asm_label = tcc_malloc(asm_label_len);
        memcpy(to_return->asm_label, old->asm_label,
            asm_label_len);
    }
#else
    /* Copy the assembler label */
    if (old->asm_label != 0) {
        to_return->asm_label = old->asm_label;
    }
#endif

    /* associated register. For variables, I believe that the low bits
     * specify the register size that can hold the value while high bits
     * indicate storage details (VT_SYM, VT_LVAL, etc). For function types,
     * however, this gets cast as an AttributeDef and queried for function
     * attributes; so far, I have only seen the .r field queried for the
     * FUNC_CALL field. It matters little; copying the whole long is easy
     * and it seems that everything works fine when it is the same for
     * consuming contexts as for the original compilation context. */
    to_return->r = old->r;

    /* Set the type. Judging by the constants in tcc.h and code that
     * uses this field, I'm pretty sure that the low bits in the .t field
     * tells tcc how to load the data into a register. The high bits seem to
     * indicate storage details, such as VT_EXTERN. Since that is not
     * something that can be extended at runtime, I should be able to copy
     * the value as-is and add an extern flag for variables and functions. */
    to_return->type.t = old->type.t;

    /* After compilation, functions and global variables point to hard
     * locations in memory. Consuming contexts should think of these as
     * having external storage, which is reflected in the VT_EXTERN bit of
     * the type.t field. */
    btype = old->type.t & VT_BTYPE;
    if (btype == VT_FUNC || to_return->r & (VT_SYM | VT_LVAL))
        to_return->type.t |= VT_EXTERN;

    /* Remove static indicator from functions */
    if ((btype == VT_FUNC) && (old->type.t & VT_STATIC))
        to_return->type.t &= ~VT_STATIC;

    /* Static inline functions are the exception to this rule, so undo
     * the above work for them. */
    if ((old->type.t & (VT_INLINE | VT_STATIC)) == (VT_INLINE | VT_STATIC))
        to_return->type.t = old->type.t;

    /* The type.ref field contains something useful only if the basic type
     * is a pointer, struct, or function. See code from tccgen's
     * compare_types for details. */
    if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
        to_return->type.ref = get_new_symtab_pointer(old->type.ref, rh);
    }

    /* Copy the c field, the "associated number." According to tcc-doc.texi
     * as well as the comments just above the definition of put_extern_sym2,
     * the c field will (for some Syms) point to an external symbol in an
     * associated section. But this is not true for all Syms. For structs,
     * this is the size (in bytes), and for struct members it is the byte
     * offset of the member, according to the end of struct_decl(). For
     * variable length arrays, this is "the location on the stack that holds
     * the runtime sizeof for the type." For functions, I believe this is
     * one of FUNC_NEW, FUNC_OLD, or FUNC_ELLIPSIS. At any rate, everything
     * seems to work if I simply set it to zero for functions and global
     * variables and copy it otherwise, so I'm going with that. This probably
     * needs to be more nuanced. */
    if (btype == VT_FUNC || to_return->r & (VT_SYM | VT_LVAL))
        to_return->c = 0;
    else
        to_return->c = old->c;

    /* Copy the next symbol field. Labels and gotos are tracked in a
     * separate stack, so for these Symbols we focus on next, not
     * jnext. The next field (I seem to recall) is used in storing
     * argument lists, so it needs to be copied for function
     * types. I believe it can be copied anonymously. */
    to_return->next = get_new_symtab_pointer(old->next, rh);

    return to_return;
}

Sym * get_new_deftab_pointer (Sym * old, ram_hash * rh)
{
    void ** Sym_ref;
    Sym * to_return;

    /* Handle the null case up-front */
    if (old == NULL) return NULL;

    /* Does this exist in the ram hash? */
    Sym_ref = ram_hash_get_ref(rh, old);
    to_return = *Sym_ref;
    if (to_return != NULL) return to_return;

    /* Create a new define object. See symtab pointer copy above for
     * descriptions of some of the fields. */
    to_return = *Sym_ref = tcc_mallocz(sizeof(Sym));
 
    /* Convert the symbol's token index. */
    to_return->v = old->v & ~SYM_EXTENDED;

    /* As far as I can tell, the 'r' field is not used by
     * preprocessor macros. Just copy it in the off-chance I'm wrong. */
    to_return->r = old->r;

    /* Copy the tokenstream if it exists */
    if (old->d != NULL) {
        int * str = old->d;
        int len = tokenstream_len(str);
        to_return->d = tcc_malloc(sizeof(int) * len);

        /* The extended symbol table's token ids are identical to the
         * originals, so we can just copy the token stream verbatim! */
        memcpy(to_return->d, old->d, sizeof(int) * len);
    }

    /* Set the type. define_push and parse_define indicate that this
     * will be either MACRO_OBJ or MACRO_FUNC. */
    to_return->type.t = old->type.t;

    /* Copy the macro arguments. */
    to_return->next = get_new_deftab_pointer(old->next, rh);

    return to_return;
}

int should_copy_TokenSym(TokenSym * to_check, int tok_start)
{
    /* Copy all tokens that come after tok_start */
    if (to_check->tok >= tok_start) return 1;

    /* Always ignore these, no matter what */
    if (to_check->tok == TOK___LINE__
        || to_check->tok == TOK___FILE__
        || to_check->tok == TOK___DATE__
        || to_check->tok == TOK___TIME__
    ) return 0;

    /* For the handful of specially declared tokens (like push, pop, etc),
     * decide based on contents. */
    if ((to_check->sym_define != NULL && to_check->sym_define->d != NULL)
        || to_check->sym_struct != NULL
        || to_check->sym_identifier != NULL
    ) return 1;

    return 0;
}

/* Make a complete copy of the TokenSym and Sym tables, using a ram_hash
 * for the latter. */

void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start)
{
    int i, N_tokens, tok_start_offset;
    extended_symtab * to_return;
    int curr_tok_idx;
    ram_hash * sym_rh;
    ram_hash * def_rh;

    /* Do nothing if we have an empty TCCState. */
    if (NULL == s) return;

    /* Count the number of tokens that we'll store whose token ids come
     * before tok_start. (We know we'll at least have everything after
     * and including tok_start.) */

    tok_start_offset = 0;
    for (i = 0; i < tok_start - TOK_IDENT; i++) {
        if (should_copy_TokenSym(table_ident[i], tok_start)) tok_start_offset++;
    }
    N_tokens = tok_ident - tok_start + tok_start_offset;

    /* Room for the first TokenSym is included in the struct definition, so I
     * need to allocate room for the extended symtab plus N_tokens - 1. */
    to_return = tcc_malloc(sizeof(extended_symtab) + sizeof(void*) * (N_tokens - 1));
    to_return->tok_start = tok_start;
    to_return->tok_start_offset = tok_start_offset;

    /* Allocate the trie and ram_hashs */
    to_return->tsh = token_string_hash_new();
    sym_rh = to_return->sym_rh = ram_hash_new();
    def_rh = to_return->def_rh = ram_hash_new();
    to_return->N_syms = 0; /* Setting to zero indicates that the data */
    to_return->N_defs = 0; /* are in hashes, not arrays */

    /* Copy all TokenSyms and their dependent Syms */
    curr_tok_idx = 0;
    for (i = 0; i < tok_ident - TOK_IDENT; i++)
    {
        TokenSym * tok_copy = table_ident[i];
        int tokensym_size;
        TokenSym * tok_sym;

        if (!should_copy_TokenSym(tok_copy, tok_start)) continue;
            tokensym_size = sizeof(TokenSym) + tok_copy->len;
            tok_sym = to_return->tokenSym_list[curr_tok_idx++]
                = tcc_malloc(tokensym_size);

        /* Follow the code from tok_alloc_new in tccpp.c */
        tok_sym->tok = tok_copy->tok;
        tok_sym->sym_define
            = get_new_deftab_pointer(tok_copy->sym_define, def_rh);
        tok_sym->sym_label = NULL; /* Not copying labels */
        tok_sym->sym_struct
            = get_new_symtab_pointer(tok_copy->sym_struct, sym_rh);
        tok_sym->sym_identifier
            = get_new_symtab_pointer(tok_copy->sym_identifier, sym_rh);
        tok_sym->len = tok_copy->len;
        tok_sym->hash_next = NULL;
        memcpy(tok_sym->str, tok_copy->str, tok_copy->len);
        tok_sym->str[tok_copy->len] = '\0';

        /* Add this to the token string hash */
        *token_string_hash_get_ref(to_return->tsh, tok_sym->str) = tok_sym;
    }

    /* Set the tail pointer, which points to the first address past the
     * last element. */
    to_return->tokenSym_last = to_return->tokenSym_list + N_tokens;

    /* Copy the collection of inline functions */
    if (s->nb_inline_fns > 0)
    {
        int N;
        InlineFunc* new_func;
        InlineFunc* old_func;

        /* make room for the number of inline functions */
        N = s->nb_inline_fns;
        to_return->N_inline_funcs = N;
        to_return->inline_funcs = tcc_malloc(N * sizeof(InlineFunc*));

        /* Copy each inline function verbatim. Based on the behavior of
         * get_new_deftab_pointer, I do not need to update any token ids.
         */
        for (i = 0; i < s->nb_inline_fns; i++)
        {
            int ts_len;
            old_func = s->inline_fns[i];

            new_func = tcc_malloc(sizeof *new_func + strlen(old_func->filename));
            strcpy(new_func->filename, old_func->filename);
            new_func->sym = get_new_symtab_pointer(old_func->sym, sym_rh);

            /* Copy the token stream, WITHOUT replacement (see copy_extended_tokensym
             * for contrast) */

            ts_len = tokenstream_len(old_func->FUNC_STR);
            new_func->FUNC_STR = tcc_malloc(ts_len * sizeof(int));
            memcpy(new_func->FUNC_STR, old_func->FUNC_STR, ts_len * sizeof(int));
            to_return->inline_funcs[i] = new_func;
        }
    }
    else {
        to_return->inline_funcs = 0;
        to_return->N_inline_funcs = 0;
    }

    /* Store the extended symtab */
    s->exsymtab = to_return;
}

void exsymtab_free_sym (Sym * to_delete, int is_def)
{
#ifdef BEFORE_grishka
    if (to_delete == NULL) return;
    if (is_def) {
        /* If it's a define Sym, delete the token stream */
        tcc_free(to_delete->d);
    }
    else {
        /* otherwise, clear the assembler label */
        tcc_free(to_delete->asm_label);
    }
#else
    if (to_delete == NULL) return;
    if (is_def) {
        /* If it's a define Sym, delete the token stream */
        tcc_free(to_delete->d);
    }
#endif
}

/* Frees memory associated with a copied extended symbol table. For a
 * description of the structure of the allocated memory, see the copy
 * function above. */

LIBTCCAPI void tcc_delete_extended_symbol_table (extended_symtab * symtab)
{
    TokenSym** ts_to_delete;
    TokenSym** done;

    if (symtab == NULL) return;

    if (symtab->sym_list != NULL)
    {
        /* Sym memory handling depends on storage type. If N_syms is
         * zero, then its stored via a ram_hash. */

        if (symtab->N_syms == 0)
        {
            /* Iterate through all Syms in the ram tree */
            if (symtab->sym_rh->N > 0)
            {
                void * iterator_data = NULL;
                do {
                    void ** data_ref = ram_hash_iterate(symtab->sym_rh, &iterator_data);
                    exsymtab_free_sym((Sym *)*data_ref, 0);

                    /* Clear the symbol itself */
                    tcc_free(*data_ref);
                } while (iterator_data != NULL);
            }

            /* clean up the ram_hash itself */
            ram_hash_free(symtab->sym_rh);
        }
        else
        {
            int i;
            /* Iterate through all Syms in the list */
            for (i = 0; i < symtab->N_syms; i++)
                exsymtab_free_sym(symtab->sym_list + i, 0);

            /* Clean up the sym list itself */
            tcc_free(symtab->sym_list);
        }
    }

    /* Perform identical steps for define Syms. */
    if (symtab->def_list != NULL)
    {
        if (symtab->N_defs == 0) {
            if (symtab->def_rh->N > 0)
            {
                void * iterator_data = NULL;
                do {
                    void ** data_ref = ram_hash_iterate(symtab->def_rh, &iterator_data);
                    exsymtab_free_sym((Sym *)*data_ref, 1);
                    tcc_free(*data_ref);
                } while (iterator_data != NULL);
            }
            ram_hash_free(symtab->def_rh);
        }
        else {
            int i;
            for (i = 0; i < symtab->N_defs; i++)
                exsymtab_free_sym(symtab->def_list + i, 1);
            tcc_free(symtab->def_list);
        }
    }

    /* Clear out the token string hash table */
    token_string_hash_free(symtab->tsh);

    /* Clear out the allocated TokenSym pointers */
    ts_to_delete = symtab->tokenSym_list;
    done = symtab->tokenSym_last;
    while (ts_to_delete < done) {
        tcc_free(*ts_to_delete);
        ts_to_delete++;
    }

    /* Clear out the inline functions */
    if (symtab->inline_funcs) {
        int i;
        for (i = 0; i < symtab->N_inline_funcs; i++) {
            tcc_free(symtab->inline_funcs[i]->FUNC_STR);
            tcc_free(symtab->inline_funcs[i]);
        }
        tcc_free(symtab->inline_funcs);
    }

    /* Clear out the full memory allocation. */
    tcc_free(symtab);
}

/* A function that performs a number of tests for me. I only export a single
 * function to avoid cluttering up the TCC API. */
enum {
    TS_TEST_GET_TOK,
    TS_TEST_HAS_DEFINE,
    TS_TEST_HAS_STRUCT,
    TS_TEST_HAS_IDENTIFIER
};

LIBTCCAPI int tcc_extended_symtab_test(extended_symtab_p symtab, int to_test, const char * name)
{
    TokenSym * ts;

    /* Get the tokenSym by the given name */
    ts = *token_string_hash_get_ref(symtab->tsh, name);
    if (ts == NULL) return 0;

    /* Perform the requested test */
    switch(to_test) {
        case TS_TEST_GET_TOK:
            return ts->tok;
        case TS_TEST_HAS_DEFINE:
            return ts->sym_define != NULL;
        case TS_TEST_HAS_IDENTIFIER:
            return ts->sym_identifier != NULL;
        case TS_TEST_HAS_STRUCT:
            return ts->sym_struct != NULL;
    }
    return 0;
}

/*****************************************************************************/
/*                      Pre-compilation TokenSym Prep                        */
/*****************************************************************************/

LIBTCCAPI void tcc_prep_tokensym_list(extended_symtab * symtab)
{
    int i;
    for (i = 0; i < symtab->tok_start_offset; i++)
    {
        TokenSym * ext_ts = symtab->tokenSym_list[i];
        int flagless_tok = ext_ts->tok & ~(SYM_STRUCT | SYM_FIELD | SYM_EXTENDED | SYM_FIRST_ANOM);
        TokenSym * local_ts = table_ident[flagless_tok - TOK_IDENT];

        /* Skip if we've already copied something for this TokenSym from another
         * extended symbol table, since it'll never get looked up in this
         * extended symbol table. */

        if (local_ts->sym_struct || local_ts->sym_identifier || local_ts->sym_define) continue;

        /* Copy */
        copy_extended_tokensym(symtab, ext_ts, local_ts);
    }
}

/*****************************************************************************/
/*                      copy extended symbol into local                      */
/*****************************************************************************/

/* Provide a mechanism for turning the local_stack off and back on outside of
 * the current static file scope. */

ST_DATA Sym * local_stack_backup;
void local_stack_off() {
    local_stack_backup = local_stack;
    local_stack = NULL;
}
void local_stack_on() {
    local_stack = local_stack_backup;
}

/* The define token stream is a series of bytes. They are collections of
 * integer => data pairs, where the integer indicates the type (and thus size)
 * of the data that follows. For example, if we encounter types TOK_PPNUM,
 * TOK_STR, or TOK_LSTR, then the next few bytes are a CString struct (mostly
 * filled with NULLs) followed by the associated character string. It gets
 * complicated, but has only a handful of special cases to handle. The formats
 * of the stream are codified in tok_str_add2, which is defined in tccpp.c.
 * 
 * This function returns the length of a tokenstream, including the final
 * null. If it is given a target tokenstream, it assumes that the
 * contents of the original have already been copied to the target
 * using memcpy, in which case it updates the token ids for arbitrary
 * tokens (function names, variable names, etc). Using the macro for
 * tokenstream_len defined in tccexsymtab.h, proper usage is:
 * 
 *   int len = tokenstream_len (stream_to_copy); // implicitly calls tokenstream_copy
 *   int * new = malloc(len * sizeof(int));
 *   memcpy(new, stream_to_copy, len * sizeof(int));
 *   tokenstream_copy(stream_to_copy, new, symtab);
 * 
 * Note that the final symtab pointer is the symtab associated with the
 * input stream, not the output stream.
 */

int tokenstream_copy (int * stream, int * to_stream, extended_symtab * symtab)
{
    int len;

    /* handle dumb user edge cases: either both to_stream and symtab
     * are null, or they are both specified. */

    if (to_stream == 0) symtab = 0;
    if (symtab == 0) to_stream = 0;

    len = 0;
    while(stream[len] != 0)
    {
        /* One for the type */
        len++;
        switch(stream[len-1])
        {
            case TOK_CINT: case TOK_CUINT: case TOK_CCHAR:
            case TOK_LCHAR: case TOK_CFLOAT: case TOK_LINENUM:
                len++;
                break;
            case TOK_PPNUM: case TOK_STR: case TOK_LSTR:
                {
                    CString *cstr = (CString *)(stream + len);

                    /* Note this right shift assumes 32 bit integers */
                    len += (sizeof(CString) + cstr->size + 3) >> 2;

                    /* If copying, then one might naively expect that I
                     * should set up cstr to be usable for later
                     * preprocessor expansions. See tok_str_add2 in
                     * tccpp.c for details. However, since the memcpy
                     * which was presumably performed prior to calling
                     * this function already did that! So I'm done. */
                }
                break;

            case TOK_CDOUBLE: case TOK_CLLONG: case TOK_CULLONG:
            #if LDOUBLE_SIZE == 8
            case TOK_CLDOUBLE:
            #endif
                len += 2;
                break;

            #if LDOUBLE_SIZE == 12
            case TOK_CLDOUBLE:
                len += 3;
            #elif LDOUBLE_SIZE == 16
            case TOK_CLDOUBLE:
                len += 4;
            #elif LDOUBLE_SIZE != 8
                #error add long double size support
            #endif
                break;

            default:
                if (to_stream && stream[len-1] >= symtab->tok_start)
                {
                    /* This is the case for an arbitrary token. Get a local
                     * token and replace the token stream's value with the
                     * local token's tok id. */

                    int from_tok = stream[len-1] & ~SYM_EXTENDED;
                    to_stream[len-1] = get_local_tok_for_extended_tok(from_tok, symtab);
                }

                /* Any token value less than tok_start refers to a value in
                 * the symbol table that is pre-defined, such as the C
                 * language key words (struct, case) and the ASCII letters. */

                /* Default is a single token (integer), which doesn't require
                 * any additional bytes. So do nothing. */

                break;
        }
    }

    /* add one for the zero byte */
    return len + 1;
}

/* Figures out the local token id for a given extended token id. If the given
 * token id is below tok_start, then it is known to exist in all compiler
 * contexts and so it is simply returned. If the token id is equal to or above
 * tok_start, this obtains a pointer to a local TokenSym and returns that
 * TokenSym's token id, together with the flags of the origina, extended token id. */

int get_local_tok_for_extended_tok(int orig_tok, extended_symtab* symtab)
{
    TokenSym* orig_ts;
    TokenSym* local_ts;
    int tok_start_offset;
    int orig_tok_offset;

    int tok_start = symtab->tok_start;
    int orig_tok_no_fields = orig_tok & ~(SYM_STRUCT | SYM_FIELD); /* strip flags  */

    /* special case for ordinary tokens that exist in all compiler contexts,
     * including "data", "string", and others. */

    if (orig_tok_no_fields < tok_start) return orig_tok;

    /* figure out the offset of the extended tokensym */
    tok_start_offset = symtab->tok_start_offset;
    orig_tok_offset = orig_tok_no_fields - tok_start + tok_start_offset;

    orig_ts = symtab->tokenSym_list[orig_tok_offset];         /* get ext   ts */
    local_ts = get_local_ts_for_extended_ts(orig_ts, symtab); /* get local ts */

    return local_ts->tok | (orig_tok & (SYM_STRUCT | SYM_FIELD)); /* add flags */
}

/* Figures out the local TokenSym for a given extended token id. Uses
 * get_local_ts_for_extended_ts to generate a new local TokenSym if necessary. */

TokenSym * get_local_tokensym_for_extended_tok(int tok, extended_symtab * symtab)
{
    int tok_start;

    /* Clear out extraneous flags */
    tok &= ~(SYM_STRUCT | SYM_FIELD | SYM_EXTENDED);

    tok_start = symtab->tok_start;
    if (tok >= tok_start)
    {
        /* This is easy because (1) we know exactly how to compute the TokenSym's
         * array offset and (2) we have a function that'll create a local TokenSym
         * if one isn't already available. */

        TokenSym * from_ts = symtab->tokenSym_list[tok - tok_start + symtab->tok_start_offset];
        return get_local_ts_for_extended_ts(from_ts, symtab);
    }

    /* And if it's earlier than tok_start, we can directly access the table_ident. */
    return table_ident[tok - TOK_IDENT];
}

void copy_extended_tokensym (extended_symtab * symtab, TokenSym * from, TokenSym * to)
{
    /* Mark this token as extended. This will cause a symbol-used callback to be
     * fired the first time this token is used in reference to a symbol (at
     * which point the extended flag will be cleared). */

    to->tok |= SYM_EXTENDED;

    /* We need to copy over the following fields:
     *  - sym_define
     *  - sym_struct
     *  - sym_identifier
     * These fields are OK as they are:
     *  - str has already been allocated and copied
     *  - len has already been set to the string's length
     *  - sym_label should already be null
     *  - hash_next is probably null, and ought not be modified
     */

    /***** sym_struct and sym_identifier copy *****/

    to->sym_struct = copy_extended_sym(symtab, from->sym_struct, to->tok);
    to->sym_identifier = copy_extended_sym(symtab, from->sym_identifier, to->tok);

    /***** token stream if it's an inline function *****/
    if (from->sym_identifier && (from->sym_identifier->type.t & VT_INLINE))
    {
        int i;
        InlineFunc* new_func;
        InlineFunc* old_func;

        /* Find the associated inline func, copy its contents, add to
         * current context's collection of inline functions */

        for (i = 0; i < symtab->N_inline_funcs; i++)
        {
            int ts_len;

            old_func = symtab->inline_funcs[i];
            if (old_func->sym != from->sym_identifier) continue;

            new_func = tcc_malloc(sizeof *new_func + strlen(old_func->filename));
            strcpy(new_func->filename, old_func->filename);
            new_func->sym = to->sym_identifier;

            /* Copy the token stream, WITH replacement (see copy_extended_symtab
             * for contrast) */

            ts_len = tokenstream_len(old_func->FUNC_STR);
            new_func->FUNC_STR = tcc_malloc(ts_len * sizeof(int));
            memcpy(new_func->FUNC_STR, old_func->FUNC_STR, ts_len * sizeof(int));
            tokenstream_copy(old_func->FUNC_STR, new_func->FUNC_STR, symtab);

            /* Add to the list */
            dynarray_add((void ***)&tcc_state->inline_fns, &tcc_state->nb_inline_fns, new_func);
        }
    }

    /***** sym_define copy *****/

    /* There may be no sym_define, or it may have been undef'd. Note that
     * something which is just defined (and subsequently used in #ifdef
     * statements) have a non-null d field, which points to an int string that
     * only contains a single zero-valued int. */

    if (from->sym_define == NULL || from->sym_define->d == NULL)
        to->sym_define = NULL;
    else
    {
        Sym *first_arg, *curr_from_arg, *newest_arg, **p_curr_arg;

        /* Otherwise, we need to copy it and update all of the token values. I
         * refrain from using the tok_str_* functions because I already have the
         * entire token stream and can easily get its length. After copying the
         * token stream bit-for-bit, I go back and re-assign the token values
         * referring to barewords so that they refer to tokens in the current
         * compilation context rather than the original extended symbol table. */

        /* Copy the token stream, WITH replacement */

        int * from_stream = from->sym_define->d;
        int len = tokenstream_len(from_stream);
        int * to_stream = tcc_malloc(sizeof(int) * len);

        memcpy(to_stream, from_stream, sizeof(int) * len);
        tokenstream_copy(from_stream, to_stream, symtab);

        /* We have copied the token stream, but we still need to copy the
         * argument list (for macro functions). */

        first_arg = NULL;
        p_curr_arg = &first_arg;
        for (curr_from_arg = from->sym_define->next; curr_from_arg != NULL;
            curr_from_arg = curr_from_arg->next)
        {
            /* Get local TokenSym associated with curr_from_arg */
            TokenSym * local_ts = get_local_tokensym_for_extended_tok(curr_from_arg->v, symtab);

            /* Add the argument to the local define stack and move the chains */
            newest_arg = sym_push2(&define_stack, local_ts->tok | SYM_FIELD,
                curr_from_arg->type.t, 0);
            *p_curr_arg = newest_arg;
            p_curr_arg = &newest_arg->next;
        }

        /* Now that we have all the moving parts, add the preprocessor to the
         * current compilation context. */

        #ifdef BEFORE_hash_opt
          define_push     (to->tok, from->sym_define->type.t, to_stream, first_arg); /* sym_define is now set */
        #else
          define_push_old (to->tok, from->sym_define->type.t, to_stream, first_arg); /* sym_define is now set */
        #endif
    }
}

/* Copy the CType information from an extended sym into a local CType. The hard
 * part here is finding the local tokensym associated with the type.ref field,
 * which is only an issue if the type is a pointer, struct, or function. */

#define copy_ctype(to_type, from, symtab) do { \
    int btype = from->type.t & VT_BTYPE; \
    to_type.t = from->type.t; \
    if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) { \
        /* Get the from->type.ref's token and look for it here */ \
        if (from->type.ref->v & SYM_FIRST_ANOM) { \
            /* Anonymous symbol; just copy it. */ \
            to_type.ref = copy_extended_sym(symtab, from->type.ref, \
            /* ??? */ anon_sym++ | (from->type.ref->v & (SYM_STRUCT | SYM_FIELD))); \
        } \
        else if (from->type.ref->v == SYM_FIELD) { \
            /* Anonymous symbol; just copy it. */ \
            to_type.ref = copy_extended_sym(symtab, from->type.ref, SYM_FIELD); \
        } \
        else { \
            /* Not anonymous: get the tokensym */ \
            TokenSym* local_ts = get_local_tokensym_for_extended_tok(from->type.ref->v, symtab); \
            if (btype == VT_STRUCT) to_type.ref = local_ts->sym_struct; \
            else to_type.ref = local_ts->sym_identifier; \
        } \
    } \
    else to_type.ref = NULL; \
} while(0)

Sym * copy_extended_sym (extended_symtab * symtab, Sym * from, int to_tok)
{
    CType to_type;
    Sym * s;
    Sym * from_next;
    Sym **psnext;

    if (from == NULL) return NULL;

    /* Copy the flags and CType from the "from" sym and push on the symbol stack */
    to_tok |= from->v & (SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
    copy_ctype(to_type, from, symtab);
    s = sym_push(to_tok, &to_type, from->r, from->c);

#ifdef BEFORE_grishka
    /* Copy the assembler label, if present */
    if (from->asm_label != NULL) {
        int asm_label_len = strlen(from->asm_label) + 1;
        s->asm_label = tcc_malloc(asm_label_len);
        memcpy(s->asm_label, from->asm_label, asm_label_len);
    }
#else
    if (from->asm_label != 0) {
        s->asm_label = from->asm_label;
    }
#endif

    /* All done unless we have a next field to copy as well. */
    if (from->next == NULL) return s;

    /* Copy the linked list started in the next field. Much of this code
     * resembles copy_ctype, unfortunately. */

    from_next = from->next;
    psnext = &s->next;
    while (from_next)
    {
        CType new_next_type;
        int new_tok;

        if (from_next->v & SYM_FIRST_ANOM)
        {
            /* Anonymous symbol; not attached to a TokenSym, so just copy it. */
            *psnext = copy_extended_sym(symtab, from_next,
                anon_sym++ | (from_next->v & (SYM_STRUCT | SYM_FIELD)));

            /* copy_extended_sym is a recursive function call which copied the
             * remaining elements of the next chain. Thus, we're done. */
            return s;
        }
        else if (from_next->v == SYM_FIELD)
        {
            /* This is an anonymous symbol associated with pointers, arrays, and
             * function declarations (tccgen.c in post_type). As above, just
             * copy it. */

            *psnext = copy_extended_sym(symtab, from_next, SYM_FIELD);

            /* copy_extended_sym is a recursive function call which copied the
             * remaining elements of the next chain. Thus, we're done. */
            return s;
        }

        /* Push a copy of the Sym to the local symbol stack. */
        copy_ctype(new_next_type, from_next, symtab);
        new_tok = get_local_tok_for_extended_tok(from_next->v, symtab);
        *psnext = sym_push(new_tok, &new_next_type, from_next->r, from_next->c);

        /* Cycle the pointers. */
        from_next = from_next->next;
        psnext = &((*psnext)->next);
    }
    return s;
}

/*****************************************************************************/
/*                      Extended Symbol Table Caching                        */
/*****************************************************************************/

LIBTCCAPI int tcc_set_extended_symbol(extended_symtab * symtab, const char * name, const void * pointer)
{
    TokenSym * ts = tcc_get_extended_tokensym(symtab, name);
    if (ts == NULL) return 0; /* failed */

    ts->hash_next = (void*)pointer;

    /* XXX working here - update sym's type.t as well??? */
    return 1; /* succeeded */
}

/* Write the total number of tokens that live on the end of this exsymtab, as
 * well as tok_start. */

int exsymtab_serialize_init(extended_symtab * symtab, FILE * out_fh)
{
    int to_write[3];

    to_write[0] = symtab->tokenSym_last - symtab->tokenSym_list; /* N_tokens */
    to_write[1] = symtab->tok_start;
    to_write[2] = symtab->tok_start_offset;

    if (fwrite(to_write, sizeof(int), 3, out_fh) == 3) return 1;

    /* Failed to serialize; write a message and return failure */
    printf("Serialization failed: Unable to serialize the number of tokens, tok_start, and tok_start_offset\n");
    return 0;
}

/* Get the total number of tokens that live on the end of this exsymtab
 * and allocate an exsymtab struct with enough room; read and set tok_start */

extended_symtab * exsymtab_deserialize_init(FILE * in_fh)
{
    int N_tokens;
    extended_symtab * symtab;

    if (fread(&N_tokens, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get the number of tokens\n");
        return NULL;
    }

    /* Allocate the symtab. I use mallocz so that it is full of NULLs.
     * This way, if deserialization fails durinig a later state, I can
     * simply invoke the symtab's delete and be done. */

    symtab = tcc_mallocz(sizeof(extended_symtab) + sizeof(void*) * (N_tokens - 1));
    if (symtab == NULL) {
        printf("Deserialization failed: Unable to allocate symtab to hold %d tokens\n", N_tokens);
        return NULL;
    }
    symtab->tokenSym_last = symtab->tokenSym_list + N_tokens;

    /* deserialize tok_start */
    if (fread(&(symtab->tok_start), sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get tok_start\n");
        tcc_free(symtab);
        return NULL;
    }

    /* deserialize tok_start_offset */
    if (fread(&(symtab->tok_start_offset), sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get tok_start_offset\n");
        tcc_free(symtab);
        return NULL;
    }
    return symtab;
}

/**** v, token id ****/

int exsymtab_serialize_v(FILE * out_fh, Sym * curr_sym)
{
    if (fwrite(&curr_sym->v, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write Sym's token "
            "id (v) of %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_v(FILE * in_fh, Sym * curr_sym, int i)
{
    /* Read v */
    if (fread(&(curr_sym->v), sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get token id for "
            "Sym number %d\n", i);
        return 0;
    }
    return 1;
}

/**** Associated register ****/

int exsymtab_serialize_r(FILE * out_fh, Sym * curr_sym)
{
    if (fwrite(&curr_sym->r, sizeof(curr_sym->r), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write "
            "associated register for Sym %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_r(FILE * in_fh, Sym * curr_sym, int i)
{
    if (fread(&(curr_sym->r), sizeof(curr_sym->r), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read associated "
            "register for Sym number %d\n", i);
        return 0;
    }
    return 1;
}

/**** type.t ****/

int exsymtab_serialize_type_t(FILE * out_fh, Sym * curr_sym)
{
    if (fwrite(&curr_sym->type.t, sizeof(curr_sym->type.t), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write type.t for "
            "Sym %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_type_t(FILE * in_fh, Sym * curr_sym, int i)
{
    if (fread(&(curr_sym->type.t), sizeof(curr_sym->type.t), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read type.t for "
            "Sym number %d\n", i);
        return 0;
    }
    return 1;
}

/**** next ****/

int exsymtab_serialize_next(FILE * out_fh, Sym * curr_sym, ram_hash * offset_rt)
{
    /* Default to serializing NULL */
    void * to_write = NULL;
    if (curr_sym->next != NULL) {
        /* compute the pointer offset if we have a non-null "next" field */
            void ** p_offset = ram_hash_get_ref(offset_rt, curr_sym->next);
            to_write = *p_offset;
    }

    /* Perform the write operation */
    if (fwrite(&to_write, sizeof(void *), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write 'next' pointer "
            "for Sym %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_next(FILE * in_fh, Sym * sym_list, int i)
{
    Sym * curr_sym = sym_list + i;
    uintptr_t offset;

    /* read the offset */
    if (fread(&offset, sizeof(void*), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read 'next' pointer "
            "for Sym number %d\n", i);
        return 0;
    }

    /* Set next field based on offset */
    if (offset == 0) curr_sym->next = NULL;
    else curr_sym->next = sym_list + offset - 1; /* note off-by-one */

    return 1;
}

/**** token stream, field d ****/

int exsymtab_serialize_token_stream(FILE * out_fh, Sym * curr_sym)
{
    /* write the length of the token stream first */
    int ts_len = 0;
    if (curr_sym->d != NULL) ts_len = tokenstream_len(curr_sym->d);
    if (fwrite(&ts_len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write token stream "
            "length for Sym %d\n", curr_sym->v);
        return 0;
    }

    /* If nothing to write, we're done: return success */
    if (ts_len == 0) return 1;

    /* Otherwise, write the tokenstream verbatim, so that the token
     * values line up with the TokenSym values. */
    if (fwrite(curr_sym->d, sizeof(int), ts_len, out_fh) == ts_len)
        return 1;

    /* failed */
    printf("Serialization failed: Unable to write token stream "
        "for Sym %d\n", curr_sym->v);

    return 0;
}

int exsymtab_deserialize_token_stream(FILE * in_fh, Sym * curr_sym, int i)
{
    /* get the length of the token stream */
    int ts_len;
    if (fread(&ts_len, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read token stream "
            "length for Sym number %d\n", i);
        return 0;
    }
    /* If no length, just set d to NULL and return success */
    if (ts_len == 0) {
        curr_sym->d = NULL;
        return 1;
    }

    /* Allocate memory for the token stream */
    curr_sym->d = tcc_malloc(sizeof(int) * ts_len);
    if (curr_sym->d == NULL) {
        printf("Deserialization failed: Unable to allocate %d bytes "
            "for token stream for Sym number %d\n", (int)(sizeof(int) * ts_len), i);
        return 0;
    }

    /* Read in the token stream */
    if (fread(curr_sym->d, sizeof(int), ts_len, in_fh) != ts_len) {
        printf("Deserialization failed: Unable to read %d integers "
            "for token stream for Sym number %d\n", ts_len, i);
        return 0;
    }

    return 1;
}

/**** type.ref ****/

int exsymtab_serialize_type_ref(FILE * out_fh, Sym * curr_sym, ram_hash * offset_rt)
{
    /* For details, see notes under copy_extended_symtab */
    int btype = curr_sym->type.t & VT_BTYPE;
    void * to_write;
    if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
        /* write the offset */
        void ** p_offset = ram_hash_get_ref(offset_rt, curr_sym->type.ref);
        to_write = *p_offset;
    }

    /* Perform the write operation */
    if (fwrite(&to_write, sizeof(void *), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write type.ref for "
            "Sym %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_type_ref(FILE * in_fh, Sym * sym_list, int i)
{
    /* Unlike the case for serialization, deserializing this pointer is
     * fairly easy. If something needs external linkage, then it will be
     * null. It'll need to be patched by tcc_set_extended_symbol. */

    Sym * curr_sym = sym_list + i;
    uintptr_t offset;

    if (fread(&offset, sizeof(void*), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read type.ref for "
            "Sym number %d\n", i);
        return 0;
    }

    /* Set offset */
    if (offset == 0) curr_sym->type.ref = NULL;
    else curr_sym->type.ref = sym_list + offset - 1; /* Note off-by-one */

    return 1;
}

/**** field c ****/

int exsymtab_serialize_c(FILE * out_fh, Sym * curr_sym)
{
    if (fwrite(&curr_sym->c, sizeof(curr_sym->c), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write "
            "field c for Sym %d\n", curr_sym->v);
        return 0;
    }
    return 1;
}

int exsymtab_deserialize_c(FILE * in_fh, Sym * curr_sym, int i)
{
    if (fread(&(curr_sym->c), sizeof(curr_sym->c), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to read field c "
            "for Sym number %d\n", i);
        return 0;
    }
    return 1;
}

/**** Assembler label ****/

int exsymtab_serialize_asm_label(FILE * out_fh, Sym * curr_sym)
{
#ifdef BEFORE_grishka
    int asm_label_len;

    /* no label? */
    if (curr_sym->asm_label == NULL)
    {
        int asm_label_len = 0;
        if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) == 1)
            return 1; /* success! */

        /* failure, indicate as much */
        printf("Serialization failed: Unable to write assembler "
            "label length of zero for token %d\n", curr_sym->v);
        return 0;
    }

    /* we have a label. Write out its length */
    asm_label_len = strlen(curr_sym->asm_label);
    if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write assembler "
            "label length for token %d\n", curr_sym->v);
        return 0;
    }

    if (fwrite(curr_sym->asm_label, sizeof(char), asm_label_len, out_fh) != asm_label_len) {
        printf("Serialization failed: Unable to write assembler "
            "label for token %d\n", curr_sym->v);
        return 0;
    }
    return 1;
#else
    int asm_label_len;
    const char* name;

    /* no label? */
    if (curr_sym->asm_label == 0)
    {
        int asm_label_len = 0;
        if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) == 1)
            return 1; /* success! */

        /* failure, indicate as much */
        printf("Serialization failed: Unable to write assembler "
            "label length of zero for token %d\n", curr_sym->v);
        return 0;
    }

    /* we have a label. Write out its length */
    name = get_tok_str(curr_sym->asm_label, NULL);
    asm_label_len = strlen(name);
    if (fwrite(&asm_label_len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write assembler "
            "label length for token %d\n", curr_sym->v);
        return 0;
    }

    if (fwrite(name, sizeof(char), asm_label_len, out_fh) != asm_label_len) {
        printf("Serialization failed: Unable to write assembler "
            "label for token %d\n", curr_sym->v);
        return 0;
    }
    return 1;
#endif
}

int exsymtab_deserialize_asm_label(FILE * in_fh, Sym * curr_sym, int i)
{
#ifdef BEFORE_grishka
    int asm_label_length;
    if (fread(&asm_label_length, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get assembler label length for Sym number %d\n", i);
        return 0;
    }
    if (asm_label_length == 0) {
        curr_sym->asm_label = NULL;
        return 1;
    }

    /* Allocate memory for the assembler label length, INCLUDING an
     * extra byte for the null byte. */

    curr_sym->asm_label = tcc_malloc(asm_label_length + 1);
    if (curr_sym->asm_label == NULL) {
        printf("Deserialization failed: Unable to allocate "
            "memory for assembler label for Sym number %d\n", i);
        return 0;
    }

    /* Read it in */
    if (fread(curr_sym->asm_label, 1, asm_label_length, in_fh) != asm_label_length) {
        printf("Deserialization failed: Unable to read "
            "assembler label for Sym number %d\n", i);
        tcc_free(curr_sym->asm_label);
        return 0;
    }

    /* Add null byte */
    curr_sym->asm_label[asm_label_length] = '\0';

    /* Success! */
    return 1;
#else
    int asm_label_length;
    char* name;

    if (fread(&asm_label_length, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get assembler label length for Sym number %d\n", i);
        return 0;
    }
    if (asm_label_length == 0) {
        curr_sym->asm_label = 0;
        return 1;
    }

    /* Allocate memory for the assembler label length, INCLUDING an
     * extra byte for the null byte. */

    name = tcc_malloc(asm_label_length + 1);
    if (name == NULL) {
        printf("Deserialization failed: Unable to allocate "
            "memory for assembler label for Sym number %d\n", i);
        return 0;
    }

    /* Read it in */
    if (fread(name, 1, asm_label_length, in_fh) != asm_label_length) {
        printf("Deserialization failed: Unable to read "
            "assembler label for Sym number %d\n", i);
        tcc_free(name);
        return 0;
    }

    name [asm_label_length] = '\0';
    curr_sym->asm_label = tok_alloc(name, asm_label_length)->tok;
    tcc_free (name);

    /* Success! */
    return 1;
#endif
}

/**** Serialize/deserialize a full Sym ****/

int exsymtab_serialize_sym(FILE * out_fh, Sym * curr_sym, ram_hash * offset_rt)
{
    if (!exsymtab_serialize_v(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_r(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_type_t(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_next(out_fh, curr_sym, offset_rt)) return 0;
    if (!exsymtab_serialize_type_ref(out_fh, curr_sym, offset_rt)) return 0;
    if (!exsymtab_serialize_c(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_asm_label(out_fh, curr_sym)) return 0;

    /* Success! */
    return 1;
}

int exsymtab_deserialize_sym(FILE * in_fh, Sym * sym_list, int i)
{
    Sym * curr_sym = sym_list + i;
    if (!exsymtab_deserialize_v(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_r(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_type_t(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_next(in_fh, sym_list, i)) return 0;
    if (!exsymtab_deserialize_type_ref(in_fh, sym_list, i)) return 0;
    if (!exsymtab_deserialize_c(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_asm_label(in_fh, curr_sym, i)) return 0;

    /* Success! */
    return 1;
}

/**** Serialize/deserialize a full Def ****/

int exsymtab_serialize_def(FILE * out_fh, Sym * curr_sym, ram_hash * offset_rt)
{
    if (!exsymtab_serialize_v(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_r(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_type_t(out_fh, curr_sym)) return 0;
    if (!exsymtab_serialize_next(out_fh, curr_sym, offset_rt)) return 0;
    if (!exsymtab_serialize_token_stream(out_fh, curr_sym)) return 0;

    /* Success! */
    return 1;
}

int exsymtab_deserialize_def(FILE * in_fh, Sym * sym_list, int i)
{
    Sym * curr_sym = sym_list + i;
    if (!exsymtab_deserialize_v(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_r(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_type_t(in_fh, curr_sym, i)) return 0;
    if (!exsymtab_deserialize_next(in_fh, sym_list, i)) return 0;
    if (!exsymtab_deserialize_token_stream(in_fh, curr_sym, i))
        return 0;

    /* Success! */
    return 1;
}

/**** Serialize/deserialize the full set of syms or defs ****/

ram_hash * exsymtab_serialize_syms(extended_symtab * symtab, FILE * out_fh, int is_def)
{
    /* Count the number of elements in the define ram_hash and build a
     * new one to map current addresses to new offsets. Offsets begin
     * counting at 1, not zero. */

    int N_syms_i;
    uintptr_t N_syms = 0;
    ram_hash * offset_rt = ram_hash_new();
    ram_hash * original_rt = symtab->sym_rh;

    if (is_def) original_rt = symtab->def_rh;
    if (original_rt->N > 0)
    {
        void * iterator_data = NULL;
        N_syms = 0;

        /* Iterate through all Syms in the ram tree */
        do {
            void ** old_ref;
            Sym * to_count;
            void ** new_ref;

            N_syms++;
            /* Get the Sym pointer */
            old_ref = ram_hash_iterate(original_rt, &iterator_data);
            to_count = (Sym *)*old_ref;

            /* Get and set the data slot for the mapping. Note that I do
             * not allocate any memory for this, I merely treat the void*
             * as an integer via uintptr_t. */

            new_ref = ram_hash_get_ref(offset_rt, to_count);
            *new_ref = (void*)N_syms; /* note 1-offset, not 0-offset */
        } while (iterator_data != NULL);
    }

    /* We now know the number of Syms that will be written out, and we
     * have a mapping from current address to serialized offset. */

    N_syms_i = N_syms;
    if (fwrite(&N_syms_i, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write number of%s Syms\n",
            is_def ? " define" : "");
        goto FAIL;
    }

    if (N_syms > 0)
    {
        /* Write out the contents of each Sym in the order set by the original
         * ram_hash iterator. */

        void * iterator_data = NULL;
        do {
            /* Get the Sym pointer */
            void ** old_ref = ram_hash_iterate(original_rt, &iterator_data);
            Sym * to_serialize = (Sym *)*old_ref;

            /* Call the appropriate serialization function */
            int result = is_def ? exsymtab_serialize_def(out_fh, to_serialize, offset_rt)
                                : exsymtab_serialize_sym(out_fh, to_serialize, offset_rt);

            /* bow out early if bad things happened */
            if (result == 0) goto FAIL;
        } while (iterator_data != NULL);
    }

    /* All done, return the offset ram_hash */
    return offset_rt;

    /* In case of failure, clean up the offset ram tree */
FAIL:
    ram_hash_free(offset_rt);
    return NULL;
}

int exsymtab_deserialize_syms(extended_symtab * symtab, FILE * in_fh, int is_def)
{
    int N_syms;
    Sym * new_list;
    int i;

    /* Get the number of syms in sym_list */
    if (fread(&N_syms, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get number of Syms\n");
        return 0;
    }

    /* Allocate the sym_list array. */
    new_list = tcc_mallocz(sizeof(Sym) * (N_syms ? N_syms : 1));
    if (new_list == NULL) {
        printf("Deserialization failed: Unable to allocate array to "
            "hold %d Syms\n", N_syms);
        return 0;
    }

    /* Corner case: if zero, set internal value of N_syms to 1 so that
     * deallocation will work correctly. */
    if (is_def) {
        symtab->def_list = new_list;
        symtab->N_defs = N_syms ? N_syms : 1;

        /* Deserialize each def. */
        for (i = 0; i < N_syms; i++) {
            if (!exsymtab_deserialize_def(in_fh, new_list, i)) return 0;
        }
    }
    else {
        symtab->sym_list = new_list;
        symtab->N_syms = N_syms ? N_syms : 1;

        /* Deserialize each sym. */
        for (i = 0; i < N_syms; i++) {
            if (!exsymtab_deserialize_sym(in_fh, new_list, i)) return 0;
        }
    }

    return 1;
}

/**** Serialize/deserialize a single TokenSym ****/

int exsymtab_serialize_tokensym(TokenSym ** ts_list, int i, FILE * out_fh,
    ram_hash * sym_offset_rt, ram_hash * def_offset_rt)
{
    void * offset_list[3];
    void ** offset_ref;
    TokenSym * ts = ts_list[i];

    /* start with the token name length so that deserialization can
     * allocate the needed memory. */

    if (fwrite(&ts->len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write name length "
            "for TokenSym number %d\n", i);
        return 0;
    }

    /* Serialize the token id */
    if (fwrite(&ts->tok, sizeof(ts->tok), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write token id "
            "for TokenSym number %d\n", i);
        return 0;
    }

    /* Serialize the Sym pointer offsets */
    offset_ref = ram_hash_get_ref(def_offset_rt, ts->sym_define);
    offset_list[0] = *offset_ref;
    offset_ref = ram_hash_get_ref(sym_offset_rt, ts->sym_struct);
    offset_list[1] = *offset_ref;
    offset_ref = ram_hash_get_ref(sym_offset_rt, ts->sym_identifier);
    offset_list[2] = *offset_ref;

    if (fwrite(offset_list, sizeof(void*), 3, out_fh) != 3) {
        printf("Serialization failed: Unable to write Sym pointer "
            "offsets for TokenSym number %d\n", i);
        return 0;
    }

    /* Serialize the name */
    if (fwrite(ts->str, sizeof(char), ts->len, out_fh) != ts->len) {
        printf("Serialization failed: Unable to write label name "
            "for TokenSym number %d\n", i);
        return 0;
    }

    /* Success! */
    return 1;
}

int exsymtab_deserialize_tokensym(extended_symtab * symtab, int curr_tok, FILE * in_fh)
{
    int ts_len;
    TokenSym * curr_ts;
    uintptr_t offset_list[3];

    /* Get the full tokensym length */
    if (fread(&ts_len, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get name length "
            "for TokenSym number %d\n", curr_tok);
        return 0;
    }

    /* Allocate it */
    curr_ts = symtab->tokenSym_list[curr_tok] = tcc_mallocz(ts_len + sizeof(TokenSym));

    if (curr_ts == NULL) {
        printf("Deserialization failed: Unable to allocate %d bytes "
            "for TokenSym number %d\n", (int)(ts_len + sizeof(TokenSym)), curr_tok);
        return 0;
    }

    /* Read the token id */
    if (fread(&curr_ts->tok, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get token id for "
            "for TokenSym number %d\n", curr_tok);
        return 0;
    }

    /* Get the three pointer offsets */
    if (fread(offset_list, sizeof(void*), 3, in_fh) != 3) {
        printf("Deserialization failed: Unable to get Sym pointer "
            "offsets for TokenSym number %d\n", curr_tok);
        return 0;
    }
    if (offset_list[0] != 0)
        curr_ts->sym_define = symtab->def_list + offset_list[0] - 1;
    if (offset_list[1] != 0)
        curr_ts->sym_struct = symtab->sym_list + offset_list[1] - 1;
    if (offset_list[2] != 0)
        curr_ts->sym_identifier = symtab->sym_list + offset_list[2] - 1;

    /* read in the name */
    if (fread(curr_ts->str, sizeof(char), ts_len, in_fh) != ts_len) {
        printf("Deserialization failed: Unable to read name for "
            "TokenSym number %d\n", curr_tok);
        return 0;
    }
    curr_ts->str[ts_len] = '\0';
    curr_ts->len = ts_len;

    /* Add this to the trie */
    *token_string_hash_get_ref(symtab->tsh, curr_ts->str) = curr_ts;

    /* Success! */
    return 1;
}

/**** Serialize/deserialize the full set of TokenSyms ****/

int exsymtab_serialize_tokensyms(extended_symtab * symtab, FILE * out_fh,
    ram_hash * sym_offset_rt, ram_hash * def_offset_rt)
{
    int i;
    int N_ts = symtab->tokenSym_last - symtab->tokenSym_list;

    for (i = 0; i < N_ts; i++) {
        if (!exsymtab_serialize_tokensym(symtab->tokenSym_list, i, out_fh, sym_offset_rt, def_offset_rt))
            return 0;
    }
    return 1;
}

int exsymtab_deserialize_tokensyms(extended_symtab * symtab, FILE * in_fh)
{
    int i;
    int N_ts = symtab->tokenSym_last - symtab->tokenSym_list;
    for (i = 0; i < N_ts; i++) {
        if (!exsymtab_deserialize_tokensym(symtab, i, in_fh)) return 0;
    }
    return 1;
}

/**** Serialize/deserialize an inline function filename ****/

/* Serialize the filename length (not including null) and its contents */
int exsymtab_serialize_inline_func_filename(FILE * out_fh,
    InlineFunc * curr_func)
{
    int len = strlen(curr_func->filename);
    if (fwrite(&len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write filename "
            "length for inline function associated with "
            "Sym %d\n", curr_func->sym->v);
        return 0;
    }

    if (fwrite(curr_func->filename, sizeof(char), len, out_fh) == len)
        return 1;

    /* Failed */
    printf("Serialization failed: Unable to write filename for inline "
        "function associated with Sym %d\n", curr_func->sym->v);

    return 0;
}

/* Deserialize the filename length, allocate the InlineFunc struct with
 * enough room for the filename, and deserialize the filename. */

InlineFunc * exsymtab_deserialize_inline_func_filename(FILE * in_fh, int inline_offset)
{
    int len;
    InlineFunc * to_return;

    if (fread(&len, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get length "
            "of filename for Inline function number %d\n", inline_offset);
        return NULL;
    }

    to_return = tcc_mallocz(sizeof(InlineFunc) + len);
    if (to_return == NULL) {
        printf("Deserialization failed: Unable to allocate memory for "
            "Inline function number %d\n", inline_offset);
        return NULL;
    }

    if (fread(to_return->filename, sizeof(char), len, in_fh) == len)
        return to_return;

    /* Failed */
    printf("Deserialization failed: Unable to get filename for "
        "Inline function number %d\n", inline_offset);

    return NULL;
}

/**** Serialize/deserialize an inline function Sym ****/
/* see exsymtab_serialize_tokensym and exsymtab_deserialize_tokensym for
 * similar logic */

int exsymtab_serialize_inline_func_sym(FILE * out_fh,
    InlineFunc * curr_func, ram_hash * sym_offset_rt)
{
    /* Of course, serialize the sym offset, not the sym itself */
    void * sym_offset = *ram_hash_get_ref(sym_offset_rt, curr_func->sym);
    if (fwrite(&sym_offset, sizeof(void*), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write sym offset "
            "for inline function associated with Sym %d\n", curr_func->sym->v);
        return 0;
    }

    /* Success! */
    return 1;
}

int exsymtab_deserialize_inline_func_sym(FILE * in_fh, int inline_offset,
    InlineFunc * curr_func, extended_symtab * symtab)
{
    uintptr_t offset;
    if (fread(&offset, sizeof(void*), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get sym offset for "
            "Inline function number %d\n", inline_offset);
        return 0;
    }

    /* offsets are always off by one so that they are nonzero */
    curr_func->sym = symtab->sym_list + offset - 1;

    return 1;
}

/**** Serialize/deserialize an inline function token stream ****/

int exsymtab_serialize_inline_func_token_stream(FILE * out_fh,
    InlineFunc * curr_func)
{
    int len = tokenstream_len(curr_func->FUNC_STR);
    if (fwrite(&len, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to write token stream "
            "length for inline function associated with Sym %d\n",curr_func->sym->v);
        return 0;
    }

    if (fwrite(curr_func->FUNC_STR, sizeof(int), len, out_fh) == len)
        return 1;

    printf("Serialization failed: Unable to write token stream for "
        "for inline function associated with Sym %d\n", curr_func->sym->v);

    return 0;
}

int exsymtab_deserialize_inline_func_token_stream(FILE * in_fh,
    int inline_offset, InlineFunc * curr_func)
{
    int len;
    int * stream;

    if (fread(&len, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get token stream "
            "length for Inline function number %d\n", inline_offset);
        return 0;
    }

    stream = tcc_malloc(sizeof(int) * len);
    if (stream == NULL) {
        printf("Deserialization failed: Unable to allocate memory for "
            "token stream for Inline function number %d\n", inline_offset);
        return 0;
    }

    if (fread(stream, sizeof(int), len, in_fh) != len) {
        tcc_free(stream);
        printf("Deserialization failed: Unable to get token stream for "
            "Inline function number %d\n", inline_offset);
        return 0;
    }

    curr_func->FUNC_STR = stream;
    return 1;
}

/**** Serialize/deserialize an inline function ****/

int exsymtab_serialize_inline_func(FILE * out_fh, InlineFunc * curr_func,
    ram_hash * sym_offset_rt)
{
    if (exsymtab_serialize_inline_func_filename(out_fh, curr_func)
        && exsymtab_serialize_inline_func_sym(out_fh, curr_func, sym_offset_rt)
        && exsymtab_serialize_inline_func_token_stream(out_fh, curr_func))
    {
        return 1;
    }
    return 0;
}

InlineFunc *  exsymtab_deserialize_inline_func(FILE * in_fh,
    int inline_offset, extended_symtab * symtab)
{
    InlineFunc * to_return = exsymtab_deserialize_inline_func_filename(in_fh, inline_offset);
    if (to_return == NULL) return NULL;

    if (exsymtab_deserialize_inline_func_sym(in_fh, inline_offset,to_return, symtab) == 0)
        goto FAIL;

    if (exsymtab_deserialize_inline_func_token_stream(in_fh, inline_offset, to_return) == 1)
        return to_return;
FAIL:
    tcc_free(to_return);
    return NULL;
}

/**** Serialize/deserialize all inline functions ****/

int exsymtab_serialize_inline_funcs(extended_symtab * symtab, FILE * out_fh,
    ram_hash * sym_offset_rt)
{
    int i;

    /* serialize the number of inline functions */
    if (fwrite(&symtab->N_inline_funcs, sizeof(int), 1, out_fh) != 1) {
        printf("Serialization failed: Unable to serialize the number "
            "of inline functions\n");
        return 0;
    }
    for (i = 0; i < symtab->N_inline_funcs; i++) {
        if (!exsymtab_serialize_inline_func(out_fh, symtab->inline_funcs[i], sym_offset_rt))
            return 0;
    }
    return 1;
}

int exsymtab_deserialize_inline_funcs(extended_symtab * symtab, FILE * in_fh) {
    int len, i;
    InlineFunc ** inline_funcs;

    if (fread(&len, sizeof(int), 1, in_fh) != 1) {
        printf("Deserialization failed: Unable to get number of inline "
            "functions\n");
        return 0;
    }

    inline_funcs = tcc_mallocz(sizeof(InlineFunc *) * len);
    if (inline_funcs == NULL) {
        printf("Deserialization failed: Unable to allocate memory for "
            "Inline function list\n");
        return 0;
    }

    symtab->inline_funcs = inline_funcs;
    symtab->N_inline_funcs = len;

    for (i = 0; i < len; i++) {
        inline_funcs[i] = exsymtab_deserialize_inline_func(in_fh, i,symtab);
        if (inline_funcs[i] == NULL) return 0;
    }

    return 1;
}

/**** Serialize/deserialize full symtab ****/

LIBTCCAPI int tcc_serialize_extended_symtab(extended_symtab * symtab, const char * output_filename)
{
    FILE * out_fh;
    ram_hash * sym_offset_rt;
    ram_hash * def_offset_rt;

    /* Do nothing if we have an empty symtab. */
    if (NULL == symtab) return 0;

    /* Open the file for writing */
    out_fh = fopen(output_filename, "wb");
    if (!out_fh) {
        printf("Serialization failed: Unable ot open extended symtab serialization file %s\n",
            output_filename);
        return 0;
    }

    /* Start the file with two useful integers: the number of tokens and
     * the value of tok_start. */
    if (!exsymtab_serialize_init(symtab, out_fh))
        goto FAIL;

    /* Serialize the syms and defs */
    sym_offset_rt = exsymtab_serialize_syms(symtab, out_fh, 0);
    if (sym_offset_rt == NULL) goto FAIL;

    def_offset_rt = exsymtab_serialize_syms(symtab, out_fh, 1);
    if (def_offset_rt == NULL) goto FAIL_RT;

    /* Serialize the TokenSyms */
    if (!exsymtab_serialize_tokensyms(symtab, out_fh, sym_offset_rt, def_offset_rt))
        goto FAIL_RT;

    /* serialize the inline functions */
    if (!exsymtab_serialize_inline_funcs(symtab, out_fh, sym_offset_rt))
        goto FAIL_RT;

    /* All set! */
    ram_hash_free(sym_offset_rt);
    ram_hash_free(def_offset_rt);
    fclose(out_fh);
    return 1;

FAIL_RT:
    ram_hash_free(sym_offset_rt);
    ram_hash_free(def_offset_rt);
FAIL:
    fclose(out_fh);
    return 0;
}

LIBTCCAPI extended_symtab * tcc_deserialize_extended_symtab(const char * input_filename)
{
    FILE * in_fh;
    extended_symtab * symtab;

    /* Open the file for writing */
    in_fh = fopen(input_filename, "rb");
    if (!in_fh) {
        printf("Deserialization failed: Unable to open extended symtab serialization file %s\n",
            input_filename);
        return NULL;
    }

    /* Allocate the symtab */
    symtab = exsymtab_deserialize_init(in_fh);
    if (symtab == NULL) goto FAIL;

    /* Allocate the c_trie. This may some day be serialized and
     * deserialized with the rest of the data, but for now keep things
     * simple. */

    symtab->tsh = token_string_hash_new();
    if (symtab->tsh == NULL) {
        printf("Deserialization failed: Unable to allocate new token string hash table\n");
        goto FAIL;
    }

    /* load the Syms, TokenSyms, and inline functions */
    if (!exsymtab_deserialize_syms(symtab, in_fh, 0)) goto FAIL;
    if (!exsymtab_deserialize_syms(symtab, in_fh, 1)) goto FAIL;
    if (!exsymtab_deserialize_tokensyms(symtab, in_fh)) goto FAIL;
    if (!exsymtab_deserialize_inline_funcs(symtab, in_fh)) goto FAIL;

    /* All set! */
    return symtab;

    /* If it fails, clean up the symtab. Note that the symtab deletion
     * works even when its structure is only partially built. */

FAIL:
    tcc_delete_extended_symbol_table(symtab);
    fclose(in_fh);
    return NULL;
}
