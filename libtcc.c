/*
 *  TCC - Tiny C Compiler
 * 
 *  Copyright (c) 2001-2004 Fabrice Bellard
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

/********************************************************/
/* global variables */

/* use GNU C extensions */
ST_DATA int gnu_ext = 1;

/* use TinyCC extensions */
ST_DATA int tcc_ext = 1;

/* XXX: get rid of this ASAP */
ST_DATA struct TCCState *tcc_state;

/********************************************************/

#ifdef ONE_SOURCE
#include "tccpp.c"
#include "tccgen.c"
#include "tccelf.c"
#include "tccrun.c"
#ifdef TCC_TARGET_I386
#include "i386-gen.c"
#endif
#ifdef TCC_TARGET_ARM
#include "arm-gen.c"
#endif
#ifdef TCC_TARGET_C67
#include "c67-gen.c"
#endif
#ifdef TCC_TARGET_X86_64
#include "x86_64-gen.c"
#endif
#ifdef CONFIG_TCC_ASM
#include "tccasm.c"
#if defined TCC_TARGET_I386 || defined TCC_TARGET_X86_64
#include "i386-asm.c"
#endif
#endif
#ifdef TCC_TARGET_COFF
#include "tcccoff.c"
#endif
#ifdef TCC_TARGET_PE
#include "tccpe.c"
#endif
#endif /* ONE_SOURCE */

/********************************************************/
#ifndef CONFIG_TCC_ASM
ST_FUNC void asm_instr(void)
{
    tcc_error("inline asm() not supported");
}
ST_FUNC void asm_global_instr(void)
{
    tcc_error("inline asm() not supported");
}
#endif

/********************************************************/
#ifdef _WIN32
static char *normalize_slashes(char *path)
{
    char *p;
    for (p = path; *p; ++p)
        if (*p == '\\')
            *p = '/';
    return path;
}

static HMODULE tcc_module;

/* on win32, we suppose the lib and includes are at the location of 'tcc.exe' */
static void tcc_set_lib_path_w32(TCCState *s)
{
    char path[1024], *p;
    GetModuleFileNameA(tcc_module, path, sizeof path);
    p = tcc_basename(normalize_slashes(strlwr(path)));
    if (p - 5 > path && 0 == strncmp(p - 5, "/bin/", 5))
        p -= 5;
    else if (p > path)
        p--;
    *p = 0;
    tcc_set_lib_path(s, path);
}

#ifdef TCC_TARGET_PE
static void tcc_add_systemdir(TCCState *s)
{
    char buf[1000];
    GetSystemDirectory(buf, sizeof buf);
    tcc_add_library_path(s, normalize_slashes(buf));
}
#endif

#ifndef CONFIG_TCC_STATIC
void dlclose(void *p)
{
    FreeLibrary((HMODULE)p);
}
#endif

#ifdef LIBTCC_AS_DLL
BOOL WINAPI DllMain (HANDLE hDll, DWORD dwReason, LPVOID lpReserved)
{
    if (DLL_PROCESS_ATTACH == dwReason)
        tcc_module = hDll;
    return TRUE;
}
#endif
#endif

/********************************************************/
/* copy a string and truncate it. */
PUB_FUNC char *pstrcpy(char *buf, int buf_size, const char *s)
{
    char *q, *q_end;
    int c;

    if (buf_size > 0) {
        q = buf;
        q_end = buf + buf_size - 1;
        while (q < q_end) {
            c = *s++;
            if (c == '\0')
                break;
            *q++ = c;
        }
        *q = '\0';
    }
    return buf;
}

/* strcat and truncate. */
PUB_FUNC char *pstrcat(char *buf, int buf_size, const char *s)
{
    int len;
    len = strlen(buf);
    if (len < buf_size) 
        pstrcpy(buf + len, buf_size - len, s);
    return buf;
}

PUB_FUNC char *pstrncpy(char *out, const char *in, size_t num)
{
    memcpy(out, in, num);
    out[num] = '\0';
    return out;
}

/* extract the basename of a file */
PUB_FUNC char *tcc_basename(const char *name)
{
    char *p = strchr(name, 0);
    while (p > name && !IS_DIRSEP(p[-1]))
        --p;
    return p;
}

/* extract extension part of a file
 *
 * (if no extension, return pointer to end-of-string)
 */
PUB_FUNC char *tcc_fileextension (const char *name)
{
    char *b = tcc_basename(name);
    char *e = strrchr(b, '.');
    return e ? e : strchr(b, 0);
}

/********************************************************/
/* memory management */

#undef free
#undef malloc
#undef realloc

#ifdef MEM_DEBUG
ST_DATA int mem_cur_size;
ST_DATA int mem_max_size;
unsigned malloc_usable_size(void*);
#endif

PUB_FUNC void tcc_free(void *ptr)
{
#ifdef MEM_DEBUG
    mem_cur_size -= malloc_usable_size(ptr);
#endif
    free(ptr);
}

PUB_FUNC void *tcc_malloc(unsigned long size)
{
    void *ptr;
    ptr = malloc(size);
    if (!ptr && size)
        tcc_error("memory full");
#ifdef MEM_DEBUG
    mem_cur_size += malloc_usable_size(ptr);
    if (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;
#endif
    return ptr;
}

PUB_FUNC void *tcc_mallocz(unsigned long size)
{
    void *ptr;
    ptr = tcc_malloc(size);
    memset(ptr, 0, size);
    return ptr;
}

PUB_FUNC void *tcc_realloc(void *ptr, unsigned long size)
{
    void *ptr1;
#ifdef MEM_DEBUG
    mem_cur_size -= malloc_usable_size(ptr);
#endif
    ptr1 = realloc(ptr, size);
    if (!ptr1 && size)
        tcc_error("memory full");
#ifdef MEM_DEBUG
    /* NOTE: count not correct if alloc error, but not critical */
    mem_cur_size += malloc_usable_size(ptr1);
    if (mem_cur_size > mem_max_size)
        mem_max_size = mem_cur_size;
#endif
    return ptr1;
}

PUB_FUNC char *tcc_strdup(const char *str)
{
    char *ptr;
    ptr = tcc_malloc(strlen(str) + 1);
    strcpy(ptr, str);
    return ptr;
}

PUB_FUNC void tcc_memstats(void)
{
#ifdef MEM_DEBUG
    printf("memory: %d bytes, max = %d bytes\n", mem_cur_size, mem_max_size);
#endif
}

#define free(p) use_tcc_free(p)
#define malloc(s) use_tcc_malloc(s)
#define realloc(p, s) use_tcc_realloc(p, s)

/********************************************************/
/* dynarrays */

ST_FUNC void dynarray_add(void ***ptab, int *nb_ptr, void *data)
{
    int nb, nb_alloc;
    void **pp;
    
    nb = *nb_ptr;
    pp = *ptab;
    /* every power of two we double array size */
    if ((nb & (nb - 1)) == 0) {
        if (!nb)
            nb_alloc = 1;
        else
            nb_alloc = nb * 2;
        pp = tcc_realloc(pp, nb_alloc * sizeof(void *));
        *ptab = pp;
    }
    pp[nb++] = data;
    *nb_ptr = nb;
}

ST_FUNC void dynarray_reset(void *pp, int *n)
{
    void **p;
    for (p = *(void***)pp; *n; ++p, --*n)
        if (*p)
            tcc_free(*p);
    tcc_free(*(void**)pp);
    *(void**)pp = NULL;
}

static void tcc_split_path(TCCState *s, void ***p_ary, int *p_nb_ary, const char *in)
{
    const char *p;
    do {
        int c;
        CString str;

        cstr_new(&str);
        for (p = in; c = *p, c != '\0' && c != PATHSEP; ++p) {
            if (c == '{' && p[1] && p[2] == '}') {
                c = p[1], p += 2;
                if (c == 'B')
                    cstr_cat(&str, s->tcc_lib_path);
            } else {
                cstr_ccat(&str, c);
            }
        }
        cstr_ccat(&str, '\0');
        dynarray_add(p_ary, p_nb_ary, str.data);
        in = p+1;
    } while (*p);
}

/********************************************************/

ST_FUNC Section *new_section(TCCState *s1, const char *name, int sh_type, int sh_flags)
{
    Section *sec;

    sec = tcc_mallocz(sizeof(Section) + strlen(name));
    strcpy(sec->name, name);
    sec->sh_type = sh_type;
    sec->sh_flags = sh_flags;
    switch(sh_type) {
    case SHT_HASH:
    case SHT_REL:
    case SHT_RELA:
    case SHT_DYNSYM:
    case SHT_SYMTAB:
    case SHT_DYNAMIC:
        sec->sh_addralign = 4;
        break;
    case SHT_STRTAB:
        sec->sh_addralign = 1;
        break;
    default:
        sec->sh_addralign = 32; /* default conservative alignment */
        break;
    }

    if (sh_flags & SHF_PRIVATE) {
        dynarray_add((void ***)&s1->priv_sections, &s1->nb_priv_sections, sec);
    } else {
        sec->sh_num = s1->nb_sections;
        dynarray_add((void ***)&s1->sections, &s1->nb_sections, sec);
    }

    return sec;
}

static void free_section(Section *s)
{
    tcc_free(s->data);
}

/* realloc section and set its content to zero */
ST_FUNC void section_realloc(Section *sec, unsigned long new_size)
{
    unsigned long size;
    unsigned char *data;
    
    size = sec->data_allocated;
    if (size == 0)
        size = 1;
    while (size < new_size)
        size = size * 2;
    data = tcc_realloc(sec->data, size);
    memset(data + sec->data_allocated, 0, size - sec->data_allocated);
    sec->data = data;
    sec->data_allocated = size;
}

/* reserve at least 'size' bytes in section 'sec' from
   sec->data_offset. */
ST_FUNC void *section_ptr_add(Section *sec, unsigned long size)
{
    unsigned long offset, offset1;

    offset = sec->data_offset;
    offset1 = offset + size;
    if (offset1 > sec->data_allocated)
        section_realloc(sec, offset1);
    sec->data_offset = offset1;
    return sec->data + offset;
}

/* reserve at least 'size' bytes from section start */
ST_FUNC void section_reserve(Section *sec, unsigned long size)
{
    if (size > sec->data_allocated)
        section_realloc(sec, size);
    if (size > sec->data_offset)
        sec->data_offset = size;
}

/* return a reference to a section, and create it if it does not
   exists */
ST_FUNC Section *find_section(TCCState *s1, const char *name)
{
    Section *sec;
    int i;
    for(i = 1; i < s1->nb_sections; i++) {
        sec = s1->sections[i];
        if (!strcmp(name, sec->name)) 
            return sec;
    }
    /* sections are created as PROGBITS */
    return new_section(s1, name, SHT_PROGBITS, SHF_ALLOC);
}

/* update sym->c so that it points to an external symbol in section
   'section' with value 'value' */
ST_FUNC void put_extern_sym2(Sym *sym, Section *section, 
                            addr_t value, unsigned long size,
                            int can_add_underscore)
{
    int sym_type, sym_bind, sh_num, info, other;
    ElfW(Sym) *esym;
    const char *name;
    char buf1[256];

    if (section == NULL)
        sh_num = SHN_UNDEF;
    else if (section == SECTION_ABS) 
        sh_num = SHN_ABS;
    else
        sh_num = section->sh_num;

    if ((sym->type.t & VT_BTYPE) == VT_FUNC) {
        sym_type = STT_FUNC;
    } else if ((sym->type.t & VT_BTYPE) == VT_VOID) {
        sym_type = STT_NOTYPE;
    } else {
        sym_type = STT_OBJECT;
    }

    if (sym->type.t & VT_STATIC)
        sym_bind = STB_LOCAL;
    else {
        if (sym->type.t & VT_WEAK)
            sym_bind = STB_WEAK;
        else
            sym_bind = STB_GLOBAL;
    }

    if (!sym->c) {
        name = get_tok_str(sym->v, NULL);
#ifdef CONFIG_TCC_BCHECK
        if (tcc_state->do_bounds_check) {
            char buf[32];

            /* XXX: avoid doing that for statics ? */
            /* if bound checking is activated, we change some function
               names by adding the "__bound" prefix */
            switch(sym->v) {
#ifdef TCC_TARGET_PE
            /* XXX: we rely only on malloc hooks */
            case TOK_malloc: 
            case TOK_free: 
            case TOK_realloc: 
            case TOK_memalign: 
            case TOK_calloc: 
#endif
            case TOK_memcpy: 
            case TOK_memmove:
            case TOK_memset:
            case TOK_strlen:
            case TOK_strcpy:
            case TOK_alloca:
                strcpy(buf, "__bound_");
                strcat(buf, name);
                name = buf;
                break;
            }
        }
#endif
        other = 0;

#ifdef TCC_TARGET_PE
        if (sym->type.t & VT_EXPORT)
            other |= 1;
        if (sym_type == STT_FUNC && sym->type.ref) {
            int attr = sym->type.ref->r;
            if (FUNC_EXPORT(attr))
                other |= 1;
            if (FUNC_CALL(attr) == FUNC_STDCALL && can_add_underscore) {
                sprintf(buf1, "_%s@%d", name, FUNC_ARGS(attr) * PTR_SIZE);
                name = buf1;
                other |= 2;
                can_add_underscore = 0;
            }
        } else {
            if (find_elf_sym(tcc_state->dynsymtab_section, name))
                other |= 4;
            if (sym->type.t & VT_IMPORT)
                other |= 4;
        }
#endif
        if (tcc_state->leading_underscore && can_add_underscore) {
            buf1[0] = '_';
            pstrcpy(buf1 + 1, sizeof(buf1) - 1, name);
            name = buf1;
        }
        if (sym->asm_label) {
            name = sym->asm_label;
        }
        info = ELFW(ST_INFO)(sym_bind, sym_type);
        sym->c = add_elf_sym(symtab_section, value, size, info, other, sh_num, name);
    } else {
        esym = &((ElfW(Sym) *)symtab_section->data)[sym->c];
        esym->st_value = value;
        esym->st_size = size;
        esym->st_shndx = sh_num;
    }
}

ST_FUNC void put_extern_sym(Sym *sym, Section *section, 
                           addr_t value, unsigned long size)
{
    put_extern_sym2(sym, section, value, size, 1);
}

/* add a new relocation entry to symbol 'sym' in section 's' */
ST_FUNC void greloc(Section *s, Sym *sym, unsigned long offset, int type)
{
    int c = 0;
    if (sym) {
        if (0 == sym->c)
            put_extern_sym(sym, NULL, 0, 0);
        c = sym->c;
    }
    /* now we can add ELF relocation info */
    put_elf_reloc(symtab_section, s, offset, type, c);
}

/********************************************************/

static void strcat_vprintf(char *buf, int buf_size, const char *fmt, va_list ap)
{
    int len;
    len = strlen(buf);
    vsnprintf(buf + len, buf_size - len, fmt, ap);
}

static void strcat_printf(char *buf, int buf_size, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    strcat_vprintf(buf, buf_size, fmt, ap);
    va_end(ap);
}

static void error1(TCCState *s1, int is_warning, const char *fmt, va_list ap)
{
    char buf[2048];
    BufferedFile **pf, *f;
    
    buf[0] = '\0';
    /* use upper file if inline ":asm:" or token ":paste:" */
    for (f = file; f && f->filename[0] == ':'; f = f->prev)
     ;
    if (f) {
        for(pf = s1->include_stack; pf < s1->include_stack_ptr; pf++)
            strcat_printf(buf, sizeof(buf), "In file included from %s:%d:\n",
                (*pf)->filename, (*pf)->line_num);
        if (f->line_num > 0) {
            strcat_printf(buf, sizeof(buf), "%s:%d: ",
                f->filename, f->line_num);
        } else {
            strcat_printf(buf, sizeof(buf), "%s: ",
                f->filename);
        }
    } else {
        strcat_printf(buf, sizeof(buf), "tcc: ");
    }
    if (is_warning)
        strcat_printf(buf, sizeof(buf), "warning: ");
    else
        strcat_printf(buf, sizeof(buf), "error: ");
    strcat_vprintf(buf, sizeof(buf), fmt, ap);

    if (!s1->error_func) {
        /* default case: stderr */
        fprintf(stderr, "%s\n", buf);
    } else {
        s1->error_func(s1->error_opaque, buf);
    }
    if (!is_warning || s1->warn_error)
        s1->nb_errors++;
}

LIBTCCAPI void tcc_set_error_func(TCCState *s, void *error_opaque,
                        void (*error_func)(void *opaque, const char *msg))
{
    s->error_opaque = error_opaque;
    s->error_func = error_func;
}

/* error without aborting current compilation */
PUB_FUNC void tcc_error_noabort(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
}

PUB_FUNC void tcc_error(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    va_start(ap, fmt);
    error1(s1, 0, fmt, ap);
    va_end(ap);
    /* better than nothing: in some cases, we accept to handle errors */
    if (s1->error_set_jmp_enabled) {
        longjmp(s1->error_jmp_buf, 1);
    } else {
        /* XXX: eliminate this someday */
        exit(1);
    }
}

PUB_FUNC void tcc_warning(const char *fmt, ...)
{
    TCCState *s1 = tcc_state;
    va_list ap;

    if (s1->warn_none)
        return;

    va_start(ap, fmt);
    error1(s1, 1, fmt, ap);
    va_end(ap);
}

/********************************************************/
/* I/O layer */

ST_FUNC void tcc_open_bf(TCCState *s1, const char *filename, int initlen)
{
    BufferedFile *bf;
    int buflen = initlen ? initlen : IO_BUF_SIZE;

    bf = tcc_malloc(sizeof(BufferedFile) + buflen);
    bf->buf_ptr = bf->buffer;
    bf->buf_end = bf->buffer + initlen;
    bf->buf_end[0] = CH_EOB; /* put eob symbol */
    pstrcpy(bf->filename, sizeof(bf->filename), filename);
#ifdef _WIN32
    normalize_slashes(bf->filename);
#endif
    bf->line_num = 1;
    bf->ifndef_macro = 0;
    bf->ifdef_stack_ptr = s1->ifdef_stack_ptr;
    bf->fd = -1;
    bf->prev = file;
    file = bf;
}

ST_FUNC void tcc_close(void)
{
    BufferedFile *bf = file;
    if (bf->fd > 0) {
        close(bf->fd);
        total_lines += bf->line_num;
    }
    file = bf->prev;
    tcc_free(bf);
}

ST_FUNC int tcc_open(TCCState *s1, const char *filename)
{
    int fd;
    if (strcmp(filename, "-") == 0)
        fd = 0, filename = "stdin";
    else
        fd = open(filename, O_RDONLY | O_BINARY);
    if ((s1->verbose == 2 && fd >= 0) || s1->verbose == 3)
        printf("%s %*s%s\n", fd < 0 ? "nf":"->",
               (int)(s1->include_stack_ptr - s1->include_stack), "", filename);
    if (fd < 0)
        return -1;

    tcc_open_bf(s1, filename, 0);
    file->fd = fd;
    return fd;
}

void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start);

/* compile the C file opened in 'file'. Return non zero if errors. */
static int tcc_compile(TCCState *s1)
{
    Sym *define_start;
    int tok_start;
    SValue *pvtop;
    char buf[512];
    volatile int section_sym;

#ifdef INC_DEBUG
    printf("%s: **** new file\n", file->filename);
#endif
    preprocess_init(s1);
    
    /* Note where we start adding new tokens */
    tok_start = tok_ident;

    cur_text_section = NULL;
    funcname = "";
    anon_sym = SYM_FIRST_ANOM; 

    /* file info: full path + filename */
    section_sym = 0; /* avoid warning */
    if (s1->do_debug) {
        section_sym = put_elf_sym(symtab_section, 0, 0, 
                                  ELFW(ST_INFO)(STB_LOCAL, STT_SECTION), 0, 
                                  text_section->sh_num, NULL);
        getcwd(buf, sizeof(buf));
#ifdef _WIN32
        normalize_slashes(buf);
#endif
        pstrcat(buf, sizeof(buf), "/");
        put_stabs_r(buf, N_SO, 0, 0, 
                    text_section->data_offset, text_section, section_sym);
        put_stabs_r(file->filename, N_SO, 0, 0, 
                    text_section->data_offset, text_section, section_sym);
    }
    /* an elf symbol of type STT_FILE must be put so that STB_LOCAL
       symbols can be safely used */
    put_elf_sym(symtab_section, 0, 0, 
                ELFW(ST_INFO)(STB_LOCAL, STT_FILE), 0, 
                SHN_ABS, file->filename);

    /* define some often used types */
    int_type.t = VT_INT;

    char_pointer_type.t = VT_BYTE;
    mk_pointer(&char_pointer_type);

#if PTR_SIZE == 4
    size_type.t = VT_INT;
#else
    size_type.t = VT_LLONG;
#endif

    func_old_type.t = VT_FUNC;
    func_old_type.ref = sym_push(SYM_FIELD, &int_type, FUNC_CDECL, FUNC_OLD);
#ifdef TCC_TARGET_ARM
    arm_init_types();
#endif

#if 0
    /* define 'void *alloca(unsigned int)' builtin function */
    {
        Sym *s1;

        p = anon_sym++;
        sym = sym_push(p, mk_pointer(VT_VOID), FUNC_CDECL, FUNC_NEW);
        s1 = sym_push(SYM_FIELD, VT_UNSIGNED | VT_INT, 0, 0);
        s1->next = NULL;
        sym->next = s1;
        sym_push(TOK_alloca, VT_FUNC | (p << VT_STRUCT_SHIFT), VT_CONST, 0);
    }
#endif

    define_start = define_stack;
    nocode_wanted = 1;

    if (setjmp(s1->error_jmp_buf) == 0) {
        s1->nb_errors = 0;
        s1->error_set_jmp_enabled = 1;

        ch = file->buf_ptr[0];
        tok_flags = TOK_FLAG_BOL | TOK_FLAG_BOF;
        parse_flags = PARSE_FLAG_PREPROCESS | PARSE_FLAG_TOK_NUM;
        pvtop = vtop;
        next();
        decl(VT_CONST);
        if (tok != TOK_EOF)
            expect("declaration");
        if (pvtop != vtop)
            tcc_warning("internal compiler error: vstack leak? (%d)", vtop - pvtop);

        /* end of translation unit info */
        if (s1->do_debug) {
            put_stabs_r(NULL, N_SO, 0, 0, 
                        text_section->data_offset, text_section, section_sym);
        }
    }

    s1->error_set_jmp_enabled = 0;
    
    /* Perform the symbol table callback, if requested */
    if (s1->nb_errors == 0 && s1->symtab_copy_callback != NULL) {
		copy_extended_symtab(s1, define_start, tok_start);
	}

    /* reset define stack, but leave -Dsymbols (may be incorrect if
       they are undefined) */
    free_defines(define_start); 

    gen_inline_functions();

    sym_pop(&global_stack, NULL);
    sym_pop(&local_stack, NULL);

    return s1->nb_errors != 0 ? -1 : 0;
}

LIBTCCAPI int tcc_compile_string_ex(TCCState *s, const char *str, int len, const char * filename, int line_num)
{
    int ret;

    /* Open the buffer and copy the contents */
    tcc_open_bf(s, filename, len);
    memcpy(file->buffer, str, len);
    /* Set the line number */
    file->line_num = line_num;
    /* Compile and cleanup */
    ret = tcc_compile(s);
    tcc_close();
    return ret;
}

LIBTCCAPI int tcc_compile_string(TCCState *s, const char *str)
{
    int len, ret;
    len = strlen(str);

    tcc_open_bf(s, "<string>", len);
    memcpy(file->buffer, str, len);
    ret = tcc_compile(s);
    tcc_close();
    return ret;
}

/* define a preprocessor symbol. A value can also be provided with the '=' operator */
LIBTCCAPI void tcc_define_symbol(TCCState *s1, const char *sym, const char *value)
{
    int len1, len2;
    /* default value */
    if (!value)
        value = "1";
    len1 = strlen(sym);
    len2 = strlen(value);

    /* init file structure */
    tcc_open_bf(s1, "<define>", len1 + len2 + 1);
    memcpy(file->buffer, sym, len1);
    file->buffer[len1] = ' ';
    memcpy(file->buffer + len1 + 1, value, len2);

    /* parse with define parser */
    ch = file->buf_ptr[0];
    next_nomacro();
    parse_define();

    tcc_close();
}

/* undefine a preprocessor symbol */
LIBTCCAPI void tcc_undefine_symbol(TCCState *s1, const char *sym)
{
    TokenSym *ts;
    Sym *s;
    ts = tok_alloc(sym, strlen(sym));
    s = define_find(ts->tok);
    /* undefine symbol by putting an invalid name */
    if (s)
        define_undef(s);
}

/* cleanup all static data used during compilation */
static void tcc_cleanup(void)
{
    int i, n;
    if (NULL == tcc_state)
        return;
    tcc_state = NULL;

    /* free -D defines */
    free_defines(NULL);

    /* free tokens */
    n = tok_ident - TOK_IDENT;
    for(i = 0; i < n; i++)
        tcc_free(table_ident[i]);
    tcc_free(table_ident);

    /* free sym_pools */
    dynarray_reset(&sym_pools, &nb_sym_pools);
    /* string buffer */
    cstr_free(&tokcstr);
    /* reset symbol stack */
    sym_free_first = NULL;
    /* cleanup from error/setjmp */
    macro_ptr = NULL;
}

LIBTCCAPI TCCState *tcc_new(void)
{
    TCCState *s;
    char buffer[100];
    int a,b,c;

    tcc_cleanup();

    s = tcc_mallocz(sizeof(TCCState));
    if (!s)
        return NULL;
    tcc_state = s;
#ifdef _WIN32
    tcc_set_lib_path_w32(s);
#else
    tcc_set_lib_path(s, CONFIG_TCCDIR);
#endif
    s->output_type = TCC_OUTPUT_MEMORY;
    preprocess_new();
    s->include_stack_ptr = s->include_stack;

    /* we add dummy defines for some special macros to speed up tests
       and to have working defined() */
    define_push(TOK___LINE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___FILE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___DATE__, MACRO_OBJ, NULL, NULL);
    define_push(TOK___TIME__, MACRO_OBJ, NULL, NULL);

    /* define __TINYC__ 92X  */
    sscanf(TCC_VERSION, "%d.%d.%d", &a, &b, &c);
    sprintf(buffer, "%d", a*10000 + b*100 + c);
    tcc_define_symbol(s, "__TINYC__", buffer);

    /* standard defines */
    tcc_define_symbol(s, "__STDC__", NULL);
    tcc_define_symbol(s, "__STDC_VERSION__", "199901L");
    tcc_define_symbol(s, "__STDC_HOSTED__", NULL);

    /* target defines */
#if defined(TCC_TARGET_I386)
    tcc_define_symbol(s, "__i386__", NULL);
    tcc_define_symbol(s, "__i386", NULL);
    tcc_define_symbol(s, "i386", NULL);
#elif defined(TCC_TARGET_X86_64)
    tcc_define_symbol(s, "__x86_64__", NULL);
#elif defined(TCC_TARGET_ARM)
    tcc_define_symbol(s, "__ARM_ARCH_4__", NULL);
    tcc_define_symbol(s, "__arm_elf__", NULL);
    tcc_define_symbol(s, "__arm_elf", NULL);
    tcc_define_symbol(s, "arm_elf", NULL);
    tcc_define_symbol(s, "__arm__", NULL);
    tcc_define_symbol(s, "__arm", NULL);
    tcc_define_symbol(s, "arm", NULL);
    tcc_define_symbol(s, "__APCS_32__", NULL);
#if defined(TCC_ARM_HARDFLOAT)
    tcc_define_symbol(s, "__ARM_PCS_VFP", NULL);
#endif
#endif

#ifdef TCC_TARGET_PE
    tcc_define_symbol(s, "_WIN32", NULL);
# ifdef TCC_TARGET_X86_64
    tcc_define_symbol(s, "_WIN64", NULL);
# endif
#else
    tcc_define_symbol(s, "__unix__", NULL);
    tcc_define_symbol(s, "__unix", NULL);
    tcc_define_symbol(s, "unix", NULL);
# if defined(__linux)
    tcc_define_symbol(s, "__linux__", NULL);
    tcc_define_symbol(s, "__linux", NULL);
# endif
# if defined(__FreeBSD__)
#  define str(s) #s
    tcc_define_symbol(s, "__FreeBSD__", str( __FreeBSD__));
#  undef str
# endif
# if defined(__FreeBSD_kernel__)
    tcc_define_symbol(s, "__FreeBSD_kernel__", NULL);
# endif
#endif

    /* TinyCC & gcc defines */
#if defined TCC_TARGET_PE && defined TCC_TARGET_X86_64
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long long");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long long");
#else
    tcc_define_symbol(s, "__SIZE_TYPE__", "unsigned long");
    tcc_define_symbol(s, "__PTRDIFF_TYPE__", "long");
#endif

#ifdef TCC_TARGET_PE
    tcc_define_symbol(s, "__WCHAR_TYPE__", "unsigned short");
#else
    tcc_define_symbol(s, "__WCHAR_TYPE__", "int");
#endif

#ifndef TCC_TARGET_PE
    /* glibc defines */
    tcc_define_symbol(s, "__REDIRECT(name, proto, alias)", "name proto __asm__ (#alias)");
    tcc_define_symbol(s, "__REDIRECT_NTH(name, proto, alias)", "name proto __asm__ (#alias) __THROW");
    /* paths for crt objects */
    tcc_split_path(s, (void ***)&s->crt_paths, &s->nb_crt_paths, CONFIG_TCC_CRTPREFIX);
#endif

    /* no section zero */
    dynarray_add((void ***)&s->sections, &s->nb_sections, NULL);

    /* create standard sections */
    text_section = new_section(s, ".text", SHT_PROGBITS, SHF_ALLOC | SHF_EXECINSTR);
    data_section = new_section(s, ".data", SHT_PROGBITS, SHF_ALLOC | SHF_WRITE);
    bss_section = new_section(s, ".bss", SHT_NOBITS, SHF_ALLOC | SHF_WRITE);

    /* symbols are always generated for linking stage */
    symtab_section = new_symtab(s, ".symtab", SHT_SYMTAB, 0,
                                ".strtab",
                                ".hashtab", SHF_PRIVATE); 
    strtab_section = symtab_section->link;
    s->symtab = symtab_section;
    
    /* private symbol table for dynamic symbols */
    s->dynsymtab_section = new_symtab(s, ".dynsymtab", SHT_SYMTAB, SHF_PRIVATE,
                                      ".dynstrtab", 
                                      ".dynhashtab", SHF_PRIVATE);
    s->alacarte_link = 1;
    s->nocommon = 1;
    s->section_align = ELF_PAGE_SIZE;

#ifdef CHAR_IS_UNSIGNED
    s->char_is_unsigned = 1;
#endif
    /* enable this if you want symbols with leading underscore on windows: */
#if 0 /* def TCC_TARGET_PE */
    s->leading_underscore = 1;
#endif
#ifdef TCC_TARGET_I386
    s->seg_size = 32;
#endif
    s->runtime_main = "main";
    
    /* Extended symbol table API */
    s->symtab_name_callback = NULL;
    s->symtab_number_callback = NULL;
    s->symtab_callback_data = NULL;
    return s;
}

LIBTCCAPI void tcc_delete(TCCState *s1)
{
    int i;

    tcc_cleanup();

    /* free all sections */
    for(i = 1; i < s1->nb_sections; i++)
        free_section(s1->sections[i]);
    dynarray_reset(&s1->sections, &s1->nb_sections);

    for(i = 0; i < s1->nb_priv_sections; i++)
        free_section(s1->priv_sections[i]);
    dynarray_reset(&s1->priv_sections, &s1->nb_priv_sections);
        
    /* free any loaded DLLs */
#ifdef TCC_IS_NATIVE
    for ( i = 0; i < s1->nb_loaded_dlls; i++) {
        DLLReference *ref = s1->loaded_dlls[i];
        if ( ref->handle )
            dlclose(ref->handle);
    }
#endif
    
    /* free loaded dlls array */
    dynarray_reset(&s1->loaded_dlls, &s1->nb_loaded_dlls);

    /* free library paths */
    dynarray_reset(&s1->library_paths, &s1->nb_library_paths);
    dynarray_reset(&s1->crt_paths, &s1->nb_crt_paths);

    /* free include paths */
    dynarray_reset(&s1->cached_includes, &s1->nb_cached_includes);
    dynarray_reset(&s1->include_paths, &s1->nb_include_paths);
    dynarray_reset(&s1->sysinclude_paths, &s1->nb_sysinclude_paths);

    tcc_free(s1->tcc_lib_path);
    tcc_free(s1->soname);
    tcc_free(s1->rpath);
    tcc_free(s1->init_symbol);
    tcc_free(s1->fini_symbol);
    tcc_free(s1->outfile);
    tcc_free(s1->deps_outfile);
    dynarray_reset(&s1->files, &s1->nb_files);
    dynarray_reset(&s1->target_deps, &s1->nb_target_deps);

#ifdef TCC_IS_NATIVE
# ifdef HAVE_SELINUX
    munmap (s1->write_mem, s1->mem_size);
    munmap (s1->runtime_mem, s1->mem_size);    
# else
    tcc_free(s1->runtime_mem);
# endif
#endif

    tcc_free(s1);
}

LIBTCCAPI int tcc_add_include_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, (void ***)&s->include_paths, &s->nb_include_paths, pathname);
    return 0;
}

LIBTCCAPI int tcc_add_sysinclude_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, (void ***)&s->sysinclude_paths, &s->nb_sysinclude_paths, pathname);
    return 0;
}

ST_FUNC int tcc_add_file_internal(TCCState *s1, const char *filename, int flags)
{
    const char *ext;
    ElfW(Ehdr) ehdr;
    int fd, ret, size;

    /* find source file type with extension */
    ext = tcc_fileextension(filename);
    if (ext[0])
        ext++;

#ifdef CONFIG_TCC_ASM
    /* if .S file, define __ASSEMBLER__ like gcc does */
    if (!strcmp(ext, "S"))
        tcc_define_symbol(s1, "__ASSEMBLER__", NULL);
#endif

    /* open the file */
    ret = tcc_open(s1, filename);
    if (ret < 0) {
        if (flags & AFF_PRINT_ERROR)
            tcc_error_noabort("file '%s' not found", filename);
        return ret;
    }

    /* update target deps */
    dynarray_add((void ***)&s1->target_deps, &s1->nb_target_deps,
            tcc_strdup(filename));

    if (flags & AFF_PREPROCESS) {
        ret = tcc_preprocess(s1);
        goto the_end;
    }

    if (!ext[0] || !PATHCMP(ext, "c")) {
        /* C file assumed */
        ret = tcc_compile(s1);
        goto the_end;
    }

#ifdef CONFIG_TCC_ASM
    if (!strcmp(ext, "S")) {
        /* preprocessed assembler */
        ret = tcc_assemble(s1, 1);
        goto the_end;
    }

    if (!strcmp(ext, "s")) {
        /* non preprocessed assembler */
        ret = tcc_assemble(s1, 0);
        goto the_end;
    }
#endif

    fd = file->fd;
    /* assume executable format: auto guess file type */
    size = read(fd, &ehdr, sizeof(ehdr));
    lseek(fd, 0, SEEK_SET);
    if (size <= 0) {
        tcc_error_noabort("could not read header");
        goto the_end;
    }

    if (size == sizeof(ehdr) &&
        ehdr.e_ident[0] == ELFMAG0 &&
        ehdr.e_ident[1] == ELFMAG1 &&
        ehdr.e_ident[2] == ELFMAG2 &&
        ehdr.e_ident[3] == ELFMAG3) {

        /* do not display line number if error */
        file->line_num = 0;
        if (ehdr.e_type == ET_REL) {
            ret = tcc_load_object_file(s1, fd, 0);
            goto the_end;

        }
#ifndef TCC_TARGET_PE
        if (ehdr.e_type == ET_DYN) {
            if (s1->output_type == TCC_OUTPUT_MEMORY) {
#ifdef TCC_IS_NATIVE
                void *h;
                h = dlopen(filename, RTLD_GLOBAL | RTLD_LAZY);
                if (h)
#endif
                    ret = 0;
            } else {
                ret = tcc_load_dll(s1, fd, filename, 
                                   (flags & AFF_REFERENCED_DLL) != 0);
            }
            goto the_end;
        }
#endif
        tcc_error_noabort("unrecognized ELF file");
        goto the_end;
    }

    if (memcmp((char *)&ehdr, ARMAG, 8) == 0) {
        file->line_num = 0; /* do not display line number if error */
        ret = tcc_load_archive(s1, fd);
        goto the_end;
    }

#ifdef TCC_TARGET_COFF
    if (*(uint16_t *)(&ehdr) == COFF_C67_MAGIC) {
        ret = tcc_load_coff(s1, fd);
        goto the_end;
    }
#endif

#ifdef TCC_TARGET_PE
    ret = pe_load_file(s1, filename, fd);
#else
    /* as GNU ld, consider it is an ld script if not recognized */
    ret = tcc_load_ldscript(s1);
#endif
    if (ret < 0)
        tcc_error_noabort("unrecognized file type");

the_end:
    tcc_close();
    return ret;
}

LIBTCCAPI int tcc_add_file(TCCState *s, const char *filename)
{
    if (s->output_type == TCC_OUTPUT_PREPROCESS)
        return tcc_add_file_internal(s, filename, AFF_PRINT_ERROR | AFF_PREPROCESS);
    else
        return tcc_add_file_internal(s, filename, AFF_PRINT_ERROR);
}

LIBTCCAPI int tcc_add_library_path(TCCState *s, const char *pathname)
{
    tcc_split_path(s, (void ***)&s->library_paths, &s->nb_library_paths, pathname);
    return 0;
}

static int tcc_add_library_internal(TCCState *s, const char *fmt,
    const char *filename, int flags, char **paths, int nb_paths)
{
    char buf[1024];
    int i;

    for(i = 0; i < nb_paths; i++) {
        snprintf(buf, sizeof(buf), fmt, paths[i], filename);
        if (tcc_add_file_internal(s, buf, flags) == 0)
            return 0;
    }
    return -1;
}

/* find and load a dll. Return non zero if not found */
/* XXX: add '-rpath' option support ? */
ST_FUNC int tcc_add_dll(TCCState *s, const char *filename, int flags)
{
    return tcc_add_library_internal(s, "%s/%s", filename, flags,
        s->library_paths, s->nb_library_paths);
}

ST_FUNC int tcc_add_crt(TCCState *s, const char *filename)
{
    if (-1 == tcc_add_library_internal(s, "%s/%s",
        filename, 0, s->crt_paths, s->nb_crt_paths))
        tcc_error_noabort("file '%s' not found", filename);
    return 0;
}

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI int tcc_add_library(TCCState *s, const char *libraryname)
{
#ifdef TCC_TARGET_PE
    const char *libs[] = { "%s/%s.def", "%s/lib%s.def", "%s/%s.dll", "%s/lib%s.dll", "%s/lib%s.a", NULL };
    const char **pp = s->static_link ? libs + 4 : libs;
#else
    const char *libs[] = { "%s/lib%s.so", "%s/lib%s.a", NULL };
    const char **pp = s->static_link ? libs + 1 : libs;
#endif
    while (*pp) {
        if (0 == tcc_add_library_internal(s, *pp,
            libraryname, 0, s->library_paths, s->nb_library_paths))
            return 0;
        ++pp;
    }
    return -1;
}

LIBTCCAPI int tcc_add_symbol(TCCState *s, const char *name, const void *val)
{
#ifdef TCC_TARGET_PE
    /* On x86_64 'val' might not be reachable with a 32bit offset.
       So it is handled here as if it were in a DLL. */
    pe_putimport(s, 0, name, (uintptr_t)val);
#else
    /* XXX: Same problem on linux but currently "solved" elsewhere
       via the rather dirty 'runtime_plt_and_got' hack. */
    add_elf_sym(symtab_section, (uintptr_t)val, 0,
        ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE), 0,
        SHN_ABS, name);
#endif
    return 0;
}

LIBTCCAPI int tcc_set_output_type(TCCState *s, int output_type)
{
    s->output_type = output_type;

    if (!s->nostdinc) {
        /* default include paths */
        /* -isystem paths have already been handled */
        tcc_add_sysinclude_path(s, CONFIG_TCC_SYSINCLUDEPATHS);
    }

    /* if bound checking, then add corresponding sections */
#ifdef CONFIG_TCC_BCHECK
    if (s->do_bounds_check) {
        /* define symbol */
        tcc_define_symbol(s, "__BOUNDS_CHECKING_ON", NULL);
        /* create bounds sections */
        bounds_section = new_section(s, ".bounds", 
                                     SHT_PROGBITS, SHF_ALLOC);
        lbounds_section = new_section(s, ".lbounds", 
                                      SHT_PROGBITS, SHF_ALLOC);
    }
#endif

    if (s->char_is_unsigned) {
        tcc_define_symbol(s, "__CHAR_UNSIGNED__", NULL);
    }

    /* add debug sections */
    if (s->do_debug) {
        /* stab symbols */
        stab_section = new_section(s, ".stab", SHT_PROGBITS, 0);
        stab_section->sh_entsize = sizeof(Stab_Sym);
        stabstr_section = new_section(s, ".stabstr", SHT_STRTAB, 0);
        put_elf_str(stabstr_section, "");
        stab_section->link = stabstr_section;
        /* put first entry */
        put_stabs("", 0, 0, 0, 0);
    }

    tcc_add_library_path(s, CONFIG_TCC_LIBPATHS);
#ifdef TCC_TARGET_PE
# ifdef _WIN32
    tcc_add_systemdir(s);
# endif
#else
    /* add libc crt1/crti objects */
    if ((output_type == TCC_OUTPUT_EXE || output_type == TCC_OUTPUT_DLL) &&
        !s->nostdlib) {
        if (output_type != TCC_OUTPUT_DLL)
            tcc_add_crt(s, "crt1.o");
        tcc_add_crt(s, "crti.o");
    }
#endif
    return 0;
}

LIBTCCAPI void tcc_set_lib_path(TCCState *s, const char *path)
{
    tcc_free(s->tcc_lib_path);
    s->tcc_lib_path = tcc_strdup(path);
}

#define WD_ALL    0x0001 /* warning is activated when using -Wall */
#define FD_INVERT 0x0002 /* invert value before storing */

typedef struct FlagDef {
    uint16_t offset;
    uint16_t flags;
    const char *name;
} FlagDef;

static const FlagDef warning_defs[] = {
    { offsetof(TCCState, warn_unsupported), 0, "unsupported" },
    { offsetof(TCCState, warn_write_strings), 0, "write-strings" },
    { offsetof(TCCState, warn_error), 0, "error" },
    { offsetof(TCCState, warn_implicit_function_declaration), WD_ALL,
      "implicit-function-declaration" },
};

ST_FUNC int set_flag(TCCState *s, const FlagDef *flags, int nb_flags,
                    const char *name, int value)
{
    int i;
    const FlagDef *p;
    const char *r;

    r = name;
    if (r[0] == 'n' && r[1] == 'o' && r[2] == '-') {
        r += 3;
        value = !value;
    }
    for(i = 0, p = flags; i < nb_flags; i++, p++) {
        if (!strcmp(r, p->name))
            goto found;
    }
    return -1;
 found:
    if (p->flags & FD_INVERT)
        value = !value;
    *(int *)((uint8_t *)s + p->offset) = value;
    return 0;
}

/* set/reset a warning */
static int tcc_set_warning(TCCState *s, const char *warning_name, int value)
{
    int i;
    const FlagDef *p;

    if (!strcmp(warning_name, "all")) {
        for(i = 0, p = warning_defs; i < countof(warning_defs); i++, p++) {
            if (p->flags & WD_ALL)
                *(int *)((uint8_t *)s + p->offset) = 1;
        }
        return 0;
    } else {
        return set_flag(s, warning_defs, countof(warning_defs),
                        warning_name, value);
    }
}

static const FlagDef flag_defs[] = {
    { offsetof(TCCState, char_is_unsigned), 0, "unsigned-char" },
    { offsetof(TCCState, char_is_unsigned), FD_INVERT, "signed-char" },
    { offsetof(TCCState, nocommon), FD_INVERT, "common" },
    { offsetof(TCCState, leading_underscore), 0, "leading-underscore" },
};

/* set/reset a flag */
static int tcc_set_flag(TCCState *s, const char *flag_name, int value)
{
    return set_flag(s, flag_defs, countof(flag_defs),
                    flag_name, value);
}


static int strstart(const char *val, const char **str)
{
    const char *p, *q;
    p = *str;
    q = val;
    while (*q) {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }
    *str = p;
    return 1;
}

/* Like strstart, but automatically takes into account that ld options can
 *
 * - start with double or single dash (e.g. '--soname' or '-soname')
 * - arguments can be given as separate or after '=' (e.g. '-Wl,-soname,x.so'
 *   or '-Wl,-soname=x.so')
 *
 * you provide `val` always in 'option[=]' form (no leading -)
 */
static int link_option(const char *str, const char *val, const char **ptr)
{
    const char *p, *q;

    /* there should be 1 or 2 dashes */
    if (*str++ != '-')
        return 0;
    if (*str == '-')
        str++;

    /* then str & val should match (potentialy up to '=') */
    p = str;
    q = val;

    while (*q != '\0' && *q != '=') {
        if (*p != *q)
            return 0;
        p++;
        q++;
    }

    /* '=' near eos means ',' or '=' is ok */
    if (*q == '=') {
        if (*p != ',' && *p != '=')
            return 0;
        p++;
        q++;
    }

    if (ptr)
        *ptr = p;
    return 1;
}

static const char *skip_linker_arg(const char **str)
{
    const char *s1 = *str;
    const char *s2 = strchr(s1, ',');
    *str = s2 ? s2++ : (s2 = s1 + strlen(s1));
    return s2;
}

static char *copy_linker_arg(const char *p)
{
    const char *q = p;
    skip_linker_arg(&q);
    return pstrncpy(tcc_malloc(q - p + 1), p, q - p);
}

/* set linker options */
static int tcc_set_linker(TCCState *s, const char *option)
{
    while (option && *option) {

        const char *p = option;
        char *end = NULL;
        int ignoring = 0;

        if (link_option(option, "Bsymbolic", &p)) {
            s->symbolic = 1;
        } else if (link_option(option, "nostdlib", &p)) {
            s->nostdlib = 1;
        } else if (link_option(option, "fini=", &p)) {
            s->fini_symbol = copy_linker_arg(p);
            ignoring = 1;
        } else if (link_option(option, "image-base=", &p)
                || link_option(option, "Ttext=", &p)) {
            s->text_addr = strtoull(p, &end, 16);
            s->has_text_addr = 1;
        } else if (link_option(option, "init=", &p)) {
            s->init_symbol = copy_linker_arg(p);
            ignoring = 1;
        } else if (link_option(option, "oformat=", &p)) {
#if defined(TCC_TARGET_PE)
            if (strstart("pe-", &p)) {
#elif defined(TCC_TARGET_X86_64)
            if (strstart("elf64-", &p)) {
#else
            if (strstart("elf32-", &p)) {
#endif
                s->output_format = TCC_OUTPUT_FORMAT_ELF;
            } else if (!strcmp(p, "binary")) {
                s->output_format = TCC_OUTPUT_FORMAT_BINARY;
#ifdef TCC_TARGET_COFF
            } else if (!strcmp(p, "coff")) {
                s->output_format = TCC_OUTPUT_FORMAT_COFF;
#endif
            } else
                goto err;

        } else if (link_option(option, "rpath=", &p)) {
            s->rpath = copy_linker_arg(p);
        } else if (link_option(option, "section-alignment=", &p)) {
            s->section_align = strtoul(p, &end, 16);
        } else if (link_option(option, "soname=", &p)) {
            s->soname = copy_linker_arg(p);
#ifdef TCC_TARGET_PE
        } else if (link_option(option, "file-alignment=", &p)) {
            s->pe_file_align = strtoul(p, &end, 16);
        } else if (link_option(option, "stack=", &p)) {
            s->pe_stack_size = strtoul(p, &end, 10);
        } else if (link_option(option, "subsystem=", &p)) {
#if defined(TCC_TARGET_I386) || defined(TCC_TARGET_X86_64)
            if (!strcmp(p, "native")) {
                s->pe_subsystem = 1;
            } else if (!strcmp(p, "console")) {
                s->pe_subsystem = 3;
            } else if (!strcmp(p, "gui")) {
                s->pe_subsystem = 2;
            } else if (!strcmp(p, "posix")) {
                s->pe_subsystem = 7;
            } else if (!strcmp(p, "efiapp")) {
                s->pe_subsystem = 10;
            } else if (!strcmp(p, "efiboot")) {
                s->pe_subsystem = 11;
            } else if (!strcmp(p, "efiruntime")) {
                s->pe_subsystem = 12;
            } else if (!strcmp(p, "efirom")) {
                s->pe_subsystem = 13;
#elif defined(TCC_TARGET_ARM)
            if (!strcmp(p, "wince")) {
                s->pe_subsystem = 9;
#endif
            } else
                goto err;
#endif
        } else
            goto err;

        if (ignoring && s->warn_unsupported) err: {
            char buf[100], *e;
            pstrcpy(buf, sizeof buf, e = copy_linker_arg(option)), tcc_free(e);
            if (ignoring)
                tcc_warning("unsupported linker option '%s'", buf);
            else
                tcc_error("unsupported linker option '%s'", buf);
        }
        option = skip_linker_arg(&p);
    }
    return 0;
}

typedef struct TCCOption {
    const char *name;
    uint16_t index;
    uint16_t flags;
} TCCOption;

enum {
    TCC_OPTION_HELP,
    TCC_OPTION_I,
    TCC_OPTION_D,
    TCC_OPTION_U,
    TCC_OPTION_L,
    TCC_OPTION_B,
    TCC_OPTION_l,
    TCC_OPTION_bench,
    TCC_OPTION_bt,
    TCC_OPTION_b,
    TCC_OPTION_g,
    TCC_OPTION_c,
    TCC_OPTION_static,
    TCC_OPTION_shared,
    TCC_OPTION_soname,
    TCC_OPTION_o,
    TCC_OPTION_r,
    TCC_OPTION_s,
    TCC_OPTION_Wl,
    TCC_OPTION_W,
    TCC_OPTION_O,
    TCC_OPTION_m,
    TCC_OPTION_f,
    TCC_OPTION_isystem,
    TCC_OPTION_nostdinc,
    TCC_OPTION_nostdlib,
    TCC_OPTION_print_search_dirs,
    TCC_OPTION_rdynamic,
    TCC_OPTION_pedantic,
    TCC_OPTION_pthread,
    TCC_OPTION_run,
    TCC_OPTION_norunsrc,
    TCC_OPTION_v,
    TCC_OPTION_w,
    TCC_OPTION_pipe,
    TCC_OPTION_E,
    TCC_OPTION_MD,
    TCC_OPTION_MF,
    TCC_OPTION_x,
    TCC_OPTION_dumpversion,
};

#define TCC_OPTION_HAS_ARG 0x0001
#define TCC_OPTION_NOSEP   0x0002 /* cannot have space before option and arg */

static const TCCOption tcc_options[] = {
    { "h", TCC_OPTION_HELP, 0 },
    { "-help", TCC_OPTION_HELP, 0 },
    { "?", TCC_OPTION_HELP, 0 },
    { "I", TCC_OPTION_I, TCC_OPTION_HAS_ARG },
    { "D", TCC_OPTION_D, TCC_OPTION_HAS_ARG },
    { "U", TCC_OPTION_U, TCC_OPTION_HAS_ARG },
    { "L", TCC_OPTION_L, TCC_OPTION_HAS_ARG },
    { "B", TCC_OPTION_B, TCC_OPTION_HAS_ARG },
    { "l", TCC_OPTION_l, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "bench", TCC_OPTION_bench, 0 },
#ifdef CONFIG_TCC_BACKTRACE
    { "bt", TCC_OPTION_bt, TCC_OPTION_HAS_ARG },
#endif
#ifdef CONFIG_TCC_BCHECK
    { "b", TCC_OPTION_b, 0 },
#endif
    { "g", TCC_OPTION_g, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "c", TCC_OPTION_c, 0 },
    { "static", TCC_OPTION_static, 0 },
    { "shared", TCC_OPTION_shared, 0 },
    { "soname", TCC_OPTION_soname, TCC_OPTION_HAS_ARG },
    { "o", TCC_OPTION_o, TCC_OPTION_HAS_ARG },
    { "pedantic", TCC_OPTION_pedantic, 0},
    { "pthread", TCC_OPTION_pthread, 0},
    { "run", TCC_OPTION_run, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "norunsrc", TCC_OPTION_norunsrc, 0 },
    { "rdynamic", TCC_OPTION_rdynamic, 0 },
    { "r", TCC_OPTION_r, 0 },
    { "s", TCC_OPTION_s, 0 },
    { "Wl,", TCC_OPTION_Wl, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "W", TCC_OPTION_W, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "O", TCC_OPTION_O, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "m", TCC_OPTION_m, TCC_OPTION_HAS_ARG },
    { "f", TCC_OPTION_f, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "isystem", TCC_OPTION_isystem, TCC_OPTION_HAS_ARG },
    { "nostdinc", TCC_OPTION_nostdinc, 0 },
    { "nostdlib", TCC_OPTION_nostdlib, 0 },
    { "print-search-dirs", TCC_OPTION_print_search_dirs, 0 }, 
    { "v", TCC_OPTION_v, TCC_OPTION_HAS_ARG | TCC_OPTION_NOSEP },
    { "w", TCC_OPTION_w, 0 },
    { "pipe", TCC_OPTION_pipe, 0},
    { "E", TCC_OPTION_E, 0},
    { "MD", TCC_OPTION_MD, 0},
    { "MF", TCC_OPTION_MF, TCC_OPTION_HAS_ARG },
    { "x", TCC_OPTION_x, TCC_OPTION_HAS_ARG },
    { "dumpversion", TCC_OPTION_dumpversion, 0},
    { NULL, 0, 0 },
};

static void parse_option_D(TCCState *s1, const char *optarg)
{
    char *sym = tcc_strdup(optarg);
    char *value = strchr(sym, '=');
    if (value)
        *value++ = '\0';
    tcc_define_symbol(s1, sym, value);
    tcc_free(sym);
}

PUB_FUNC int tcc_parse_args(TCCState *s, int argc, char **argv)
{
    const TCCOption *popt;
    const char *optarg, *r;
    int run = 0;
    int norunsrc = 0;
    int pthread = 0;
    int optind = 0;

    /* collect -Wl options for input such as "-Wl,-rpath -Wl,<path>" */
    CString linker_arg;
    cstr_new(&linker_arg);

    while (optind < argc) {

        r = argv[optind++];
        if (r[0] != '-' || r[1] == '\0') {
            /* add a new file */
            if (!run || !norunsrc)
              dynarray_add((void ***)&s->files, &s->nb_files, tcc_strdup(r));
            if (run) {
                optind--;
                /* argv[0] will be this file */
                break;
            }
            continue;
        }

        /* find option in table */
        for(popt = tcc_options; ; ++popt) {
            const char *p1 = popt->name;
            const char *r1 = r + 1;
            if (p1 == NULL)
                tcc_error("invalid option -- '%s'", r);
            if (!strstart(p1, &r1))
                continue;
            optarg = r1;
            if (popt->flags & TCC_OPTION_HAS_ARG) {
                if (*r1 == '\0' && !(popt->flags & TCC_OPTION_NOSEP)) {
                    if (optind >= argc)
                        tcc_error("argument to '%s' is missing", r);
                    optarg = argv[optind++];
                }
            } else if (*r1 != '\0')
                continue;
            break;
        }

        switch(popt->index) {
        case TCC_OPTION_HELP:
            return 0;
        case TCC_OPTION_I:
            if (tcc_add_include_path(s, optarg) < 0)
                tcc_error("too many include paths");
            break;
        case TCC_OPTION_D:
            parse_option_D(s, optarg);
            break;
        case TCC_OPTION_U:
            tcc_undefine_symbol(s, optarg);
            break;
        case TCC_OPTION_L:
            tcc_add_library_path(s, optarg);
            break;
        case TCC_OPTION_B:
            /* set tcc utilities path (mainly for tcc development) */
            tcc_set_lib_path(s, optarg);
            break;
        case TCC_OPTION_l:
            dynarray_add((void ***)&s->files, &s->nb_files, tcc_strdup(r));
            s->nb_libraries++;
            break;
        case TCC_OPTION_pthread:
            parse_option_D(s, "_REENTRANT");
            pthread = 1;
            break;
        case TCC_OPTION_bench:
            s->do_bench = 1;
            break;
#ifdef CONFIG_TCC_BACKTRACE
        case TCC_OPTION_bt:
            tcc_set_num_callers(atoi(optarg));
            break;
#endif
#ifdef CONFIG_TCC_BCHECK
        case TCC_OPTION_b:
            s->do_bounds_check = 1;
            s->do_debug = 1;
            break;
#endif
        case TCC_OPTION_g:
            s->do_debug = 1;
            break;
        case TCC_OPTION_c:
            s->output_type = TCC_OUTPUT_OBJ;
            break;
        case TCC_OPTION_static:
            s->static_link = 1;
            break;
        case TCC_OPTION_shared:
            s->output_type = TCC_OUTPUT_DLL;
            break;
        case TCC_OPTION_soname:
            s->soname = tcc_strdup(optarg);
            break;
        case TCC_OPTION_m:
            s->option_m = tcc_strdup(optarg);
            break;
        case TCC_OPTION_o:
            s->outfile = tcc_strdup(optarg);
            break;
        case TCC_OPTION_r:
            /* generate a .o merging several output files */
            s->option_r = 1;
            s->output_type = TCC_OUTPUT_OBJ;
            break;
        case TCC_OPTION_isystem:
            tcc_add_sysinclude_path(s, optarg);
            break;
        case TCC_OPTION_nostdinc:
            s->nostdinc = 1;
            break;
        case TCC_OPTION_nostdlib:
            s->nostdlib = 1;
            break;
        case TCC_OPTION_print_search_dirs:
            s->print_search_dirs = 1;
            break;
        case TCC_OPTION_run:
            s->output_type = TCC_OUTPUT_MEMORY;
            tcc_set_options(s, optarg);
            run = 1;
            break;
        case TCC_OPTION_norunsrc:
            norunsrc = 1;
            break;
        case TCC_OPTION_v:
            do ++s->verbose; while (*optarg++ == 'v');
            break;
        case TCC_OPTION_f:
            if (tcc_set_flag(s, optarg, 1) < 0 && s->warn_unsupported)
                goto unsupported_option;
            break;
        case TCC_OPTION_W:
            if (tcc_set_warning(s, optarg, 1) < 0 && 
                s->warn_unsupported)
                goto unsupported_option;
            break;
        case TCC_OPTION_w:
            s->warn_none = 1;
            break;
        case TCC_OPTION_rdynamic:
            s->rdynamic = 1;
            break;
        case TCC_OPTION_Wl:
            if (linker_arg.size)
                --linker_arg.size, cstr_ccat(&linker_arg, ',');
            cstr_cat(&linker_arg, optarg);
            cstr_ccat(&linker_arg, '\0');
            break;
        case TCC_OPTION_E:
            s->output_type = TCC_OUTPUT_PREPROCESS;
            break;
        case TCC_OPTION_MD:
            s->gen_deps = 1;
            break;
        case TCC_OPTION_MF:
            s->deps_outfile = tcc_strdup(optarg);
            break;
        case TCC_OPTION_dumpversion:
            printf ("%s\n", TCC_VERSION);
            exit(0);
        case TCC_OPTION_O:
        case TCC_OPTION_pedantic:
        case TCC_OPTION_pipe:
        case TCC_OPTION_s:
        case TCC_OPTION_x:
            /* ignored */
            break;
        default:
            if (s->warn_unsupported) {
            unsupported_option:
                tcc_warning("unsupported option '%s'", r);
            }
            break;
        }
    }

    if (pthread && s->output_type != TCC_OUTPUT_OBJ)
        tcc_set_options(s, "-lpthread");

    tcc_set_linker(s, (const char *)linker_arg.data);
    cstr_free(&linker_arg);

    return optind;
}

LIBTCCAPI int tcc_set_options(TCCState *s, const char *str)
{
    const char *s1;
    char **argv, *arg;
    int argc, len;
    int ret;

    argc = 0, argv = NULL;
    for(;;) {
        while (is_space(*str))
            str++;
        if (*str == '\0')
            break;
        s1 = str;
        while (*str != '\0' && !is_space(*str))
            str++;
        len = str - s1;
        arg = tcc_malloc(len + 1);
        pstrncpy(arg, s1, len);
        dynarray_add((void ***)&argv, &argc, arg);
    }
    ret = tcc_parse_args(s, argc, argv);
    dynarray_reset(&argv, &argc);
    return ret;
}

PUB_FUNC void tcc_print_stats(TCCState *s, int64_t total_time)
{
    double tt;
    tt = (double)total_time / 1000000.0;
    if (tt < 0.001)
        tt = 0.001;
    if (total_bytes < 1)
        total_bytes = 1;
    printf("%d idents, %d lines, %d bytes, %0.3f s, %d lines/s, %0.1f MB/s\n", 
           tok_ident - TOK_IDENT, total_lines, total_bytes,
           tt, (int)(total_lines / tt),
           total_bytes / tt / 1000000.0);
}

PUB_FUNC void tcc_set_environment(TCCState *s)
{
    char * path;
    
    path = getenv("C_INCLUDE_PATH");
    if(path != NULL) {
        tcc_add_include_path(s, path);
    }
    path = getenv("CPATH");
    if(path != NULL) {
        tcc_add_include_path(s, path);
    }
    path = getenv("LIBRARY_PATH");
    if(path != NULL) {
        tcc_add_library_path(s, path);
    }
}

/* ---- Extended symbol table management ---- */

/* Allow the user to provide custom symbol table lookup functions. They
 * need to provide functions to look up symbols both by name and by
 * number */
LIBTCCAPI void tcc_set_extended_symtab_callbacks (
	TCCState * s,
	extended_symtab_copy_callback new_copy_callback,
	extended_symtab_lookup_by_name_callback new_name_callback,
	extended_symtab_lookup_by_number_callback new_number_callback,
	void * data
) {
	s->symtab_copy_callback = new_copy_callback;
	s->symtab_name_callback = new_name_callback;
	s->symtab_number_callback = new_number_callback;
	s->symtab_callback_data = data;
}

int _sym_is_all_zeros(Sym * to_check) {
	if (to_check == NULL) return 0;
	if (
		to_check->v == 0
		&& to_check->asm_label == NULL
		&& to_check->r == 0
		&& to_check->c == 0
		&& to_check->type.t == 0
		/*&& to_check->type.ref == NULL*/
		&& to_check->next == NULL
		/*&& to_check->prev == NULL*/
		&& to_check->prev_tok == NULL
	) return 1;
	return 0;
}

Sym * _get_new_sym_or_def_pointer (Sym * old, Sym * new_list, int offset_of_last, Sym * stack) {
	/* We assume that old IS NOT null; this must be checked by the higher-level
	 * functions that call this one. */
	
	/* Determine the offset of the to-be-generated list of symbols and return the
	 * pointer to that offset. Note that the list will only include non-extended
	 * symbols, so we do not increment our offset when we encounter one of those. */
	int offset = 0;
	while(stack != NULL) {
		/* Return pointer to the new symbol's location if found */
		if (stack == old) return new_list + offset;
		/* Advance to the next symbol */
		if (stack->v < SYM_EXTENDED) offset++;
		stack = stack->prev;
	}
	/* There is this weird case where sometimes a symbol refers to this
	 * strange empty symbol. Why the pointer points to it, instead of
	 * null, is not clear to me. However, we have created just such a
	 * pointer! So we use our own special empty Sym for this purpose.
	 */
	if (_sym_is_all_zeros(old)) return new_list + offset_of_last;
	return NULL;
}

Sym * get_new_symtab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Handle the extended case next. We know that copies of the extended symbols live
	 * outside of the "global" symbol stack, but "old" points to a copy that lives on
	 * the global symbol stack and will be destroyed after copy_extended_symtab returns.
	 * Since we know that the original extended symbol will stick around, we simply
	 * use that. */
	if (old->v >= SYM_EXTENDED) {
		TokenSym* tsym;
		/* Call the extended symbol lookup. */
		if (s->symtab_number_callback == NULL) {
			tcc_error_noabort("exsymtab copy found extended symbol but no symtab_number_callback");
			return NULL;
		}
		tsym = s->symtab_number_callback(old->v, s->symtab_callback_data, 0);
		if (tsym == NULL) {
			tcc_error_noabort("exsymtab copy unable to locate extended symbol for token %x", old->v);
			return NULL;
		}
		if (old->v & SYM_STRUCT) {
			if (tsym->sym_struct == NULL)
				tcc_error_noabort("exsymtab copy found extended token but no struct symbol for \"%s\" (%x)",
					tsym->str, old->v);
			return tsym->sym_struct;
		}
		if (tsym->sym_identifier == NULL)
			tcc_error_noabort("exsymtab copy found extended token but no identifier symbol for \"%s\" (%x)",
				tsym->str, old->v);
		return tsym->sym_identifier;
	}
	
	/* Check the global symbol stack. */
	Sym * to_return = _get_new_sym_or_def_pointer(old, new_list, offset_of_last, global_stack);
	if (NULL != to_return) return to_return;
	
	/* If we're here, we couldn't find the symbol on the global stack. This
	 * means it's an anonymous symbol, i.e. in a function declaration. Allocate
	 * a new Sym and attach it to *our* anonymous stack. */
	Sym * ll = new_list + offset_of_last - 1;
	while(ll->prev != NULL) ll = ll->prev;
	to_return = tcc_mallocz(sizeof(Sym));
	ll->prev = to_return;
	
	/* Fill in the values, as appropriate. See notes under the symbol stack
	 * copying for explanations. I suspect (highly) that this is only used in
	 * function argument lists, in which case the values of most fields don't
	 * matter. I know that the .t and .next fields are important, however, so
	 * I'll be sure to copy those. */
	/* XXX working here - check that assumption at some point with valgrind */
	to_return->r = old->r;
	to_return->type.t = old->type.t;
	int btype = old->type.t & VT_BTYPE;
	if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
		to_return->type.ref
			= get_new_symtab_pointer(s, old->type.ref, new_list, offset_of_last);
if (old->type.ref == NULL) printf("old->type.ref is null!\n");
if (to_return->type.ref == NULL) printf("to_return->type.ref is null!\n");
	}
	if (btype == VT_FUNC) to_return->c = 0;
	else to_return->c = old->c;
	to_return->next
		= get_new_symtab_pointer(s, old->next, new_list, offset_of_last);
	return to_return;
}

Sym * get_new_deftab_pointer (TCCState * s, Sym * old, Sym * new_list, int offset_of_last) {
	/* Handle the null case up-front */
	if (old == NULL) return NULL;
	
	/* Handle the extended case next. We know that copies of the extended symbols live
	 * outside of the "global" symbol stack, but "old" points to a copy that lives on
	 * the global symbol stack and will be destroyed after copy_extended_symtab returns.
	 * Since we know that the original extended symbol will stick around, we simply
	 * use that. */
	if (old->v >= SYM_EXTENDED) {
		TokenSym* tsym;
		/* Call the extended symbol lookup. */
		if (s->symtab_number_callback == NULL) {
			tcc_error_noabort("exsymtab copy found extended symbol but no symtab_number_callback");
			return NULL;
		}
		tsym = s->symtab_number_callback(old->v, s->symtab_callback_data, 0);
		if (tsym == NULL) {
			tcc_error_noabort("exsymtab copy unable to locate extended symbol for preprocessor token %x", old->v);
			return NULL;
		}
		if (tsym->sym_define == NULL) {
			tcc_error_noabort("exsymtab copy found extended token but no preprocessor symbol for \"%s\" (%x)"
				, tsym->str, old->v);
		}
		return tsym->sym_define;
	}
	
	/* Otherwise use the non-extended symbol lookup */
	Sym * to_return = _get_new_sym_or_def_pointer(old, new_list, offset_of_last, define_stack);
	if (to_return != NULL) return to_return;
	
	/* If we're here, we ran into trouble: print out a diagnostic */
	printf("In %s line %d, unable to locate #define symbol offset for old address %p:\n", __FILE__, __LINE__, old);
	printf("  Symbol token: %X\n", old->v);
	printf("  Assembler label at address %p\n", old->asm_label);
	printf("  Associated register %lX\n", old->r);
	printf("  Associated number %lX\n", old->c);
	printf("  Type.t %X\n", old->type.t);
	printf("  Type.ref %p\n", old->type.ref);
	printf("  next symbol pointer %p\n", old->next);
	printf("  previous symbol in stack at address %p\n", old->prev);
	printf("  previous symbol for this token at address %p\n", old->prev_tok);
	return NULL;
}

/* Make a complete copy of the token-symbol table, the symbol stack, and
 * the define stack XXX ??? XXX, though the user only thinks they have
 * the token symbol table. */
int _tcc_extended_symbol_counter = SYM_EXTENDED;
void copy_extended_symtab (TCCState * s, Sym * define_start, int tok_start) {

    /* Do nothing if we have an empty TCCState. */
    if (NULL == s) return;
	
	/* Note: to prevent two extended symbols from clashing by number, we
	 * use a revised symbol numbering system. This simply increments
	 * _tcc_extended_symbol_counter with each new extended symbol. Among
	 * other things, this guarantees that extended symbol numbers for a
	 * specific extended symbol table lie within a contiguous range. */
	
    int i;
    
	/* Allocate the memory for the extended symbol table. The structure
	 * for this table will be:
	 * Sym* SymList
	 * Sym* PastEndOfSymList
	 * Sym* defineList
	 * Sym* PastEndOfDefineList
	 * TokenSym** TAIL (contains the address of the last TokenSym*)
	 * TokenSym* = table_ident[0], <- returned pointer points here
	 * TokenSym* = table_ident[1],
	 * ...
	 * TokenSym* = table_ident[n-1] <- TAIL points to this offset
	 * 
	 * So we need N_tokens + 5
	 */
    
    int N_tokens = tok_ident - tok_start;
    void ** to_return = tcc_malloc(sizeof(void*) * (N_tokens + 5));
    
    /********* symbol stack *********/
    
    /* Calculate the number of symbols */
    Sym * curr_Sym = global_stack;
    int N_Syms = 0;
    while(curr_Sym != NULL) {
    	/* We only track non-extended symbols */
		if (curr_Sym->v < SYM_EXTENDED) N_Syms++;
		/* Step to the next symbol in the list */
		curr_Sym = curr_Sym->prev;
	}
    /* In addition to the sym list just counted, we need two more. We need a
     * sym that is all zeros (which we put as the last item), and we need a
     * sym that serves as the head of our anonymous symbol stack. The anonymous
     * symbol stack will be located at offset N_Syms - 1, so we have to
     * increment N_Syms by one.
     * working here - is this reasoning correct? */
    N_Syms++;
    Sym * sym_list = to_return[0] = tcc_malloc(sizeof(Sym) * (N_Syms + 1));
    to_return[1] = sym_list + N_Syms;
    
	/* Zero out last sym */
    sym_list[N_Syms-1].v = sym_list[N_Syms].v = 0;
    sym_list[N_Syms-1].asm_label = sym_list[N_Syms].asm_label = NULL;
    sym_list[N_Syms-1].r = sym_list[N_Syms].r = 0;
    sym_list[N_Syms-1].c = sym_list[N_Syms].c = 0;
    sym_list[N_Syms-1].type.t = sym_list[N_Syms].type.t = 0;
    sym_list[N_Syms-1].type.ref = sym_list[N_Syms].type.ref = NULL;
    sym_list[N_Syms-1].next = sym_list[N_Syms].next = NULL;
    sym_list[N_Syms-1].prev = sym_list[N_Syms].prev = NULL;
    sym_list[N_Syms-1].prev_tok = sym_list[N_Syms].prev_tok = NULL;
    
    /* Copy the Sym list */
    for (curr_Sym = global_stack, i = 0; i < N_Syms-1; curr_Sym = curr_Sym->prev) {
    	/* only copy non-extended symbols */
    	if (curr_Sym->v >= SYM_EXTENDED) continue;
    	
		/* See tcc.h around line 425 for descriptions of some of the fields.
		 * See also tccgen.c line 5987 to see what needs to happen for function
		 * declarations to work properly (and, in turn, line 446 for how to
		 * push a forward reference). */
		 
		/* Convert the symbol's token index based on what we will allocate when
		 * we build the TokenSym list. These extra flags must be removed for
		 * proper conversion, but must be added back on. This is because the
		 * v fields of these symbols compared directly to a token index that
		 * has had these flags ORed onto them. This is the case, for example,
		 * with struct member identification. See the handling of TOK_ARROW as
		 * a post-op in unary(), which is in tccgen.c. */
		int extra_flags = (curr_Sym->v & (SYM_FIELD | SYM_STRUCT | SYM_FIRST_ANOM));
		int bare_v = curr_Sym->v - extra_flags;
		if (curr_Sym->v == SYM_FIELD) {
			/* Functions of type int (int) have a token type that is
			 * always present in every compiler context and can be
			 * copied without modification. */
			sym_list[i].v = SYM_FIELD;
		}
		else {
			sym_list[i].v = bare_v - tok_start
				+ _tcc_extended_symbol_counter
				+ extra_flags; /* XXX double check */
		}
		
		/* Copy the assembler label */
		if (curr_Sym->asm_label != NULL) {
			int asm_label_len = strlen(curr_Sym->asm_label) + 1;
			sym_list[i].asm_label = tcc_malloc(asm_label_len);
			memcpy(sym_list[i].asm_label, curr_Sym->asm_label,
				asm_label_len);
		}
		else {
			sym_list[i].asm_label = NULL;
		}
		
		/* associated register. Except for functions, I believe this specifies
		 * the register size that can hold the value. For function types,
		 * however, this gets cast as an AttributeDef and queried for function
		 * attributes. So far, I have only seen the .r field queried for
		 * the FUNC_CALL field, so I do not think that the full long data slot
		 * is used. It matters little; copying the whole long is easy. At any
		 * rate, I am fairly confident that this can just be copied straight. */
		sym_list[i].r = curr_Sym->r;
		
		int btype = curr_Sym->type.t & VT_BTYPE;
		
		/* Set the type. Judging by the constants in tcc.h and code that
		 * uses this field, I'm pretty sure that the .t field tells tcc
		 * how to load the data into a register. Since that is not
		 * something that can be extended at runtime, I should be able
		 * to copy the value as-is. */
		sym_list[i].type.t = curr_Sym->type.t;
		/* All functions need to be declared as external definitions */
		if (btype == VT_FUNC) sym_list[i].type.t |= VT_EXTERN;
		
		/* The type.ref field contains something useful only if the basic type
		 * is a pointer, struct, or function. See code from tccgen's
		 * compare_types for details. */
		if (btype == VT_PTR || btype == VT_STRUCT || btype == VT_FUNC) {
			sym_list[i].type.ref
				= get_new_symtab_pointer(s, curr_Sym->type.ref, sym_list, N_Syms);
		}
		
		/* Copy the c field, the "associated number." For functions, this is
		 * one of FUNC_NEW, FUNC_OLD, or FUNC_ELLIPSIS. For structs, this is
		 * the size (in bytes), and for struct members it is the byte offset
		 * of the member, according to the end of struct_decl().
		 * Line 5982 of tccgen.c seems to suggest that this needs to be
		 * **negative** and we need VT_CONST in order to get external linkage. 
		 */
		/* Working here, this almost certainly needs to be more nuanced... */
		if ((sym_list[i].type.t & VT_BTYPE) == VT_FUNC) sym_list[i].c = 0;
		else sym_list[i].c = curr_Sym->c;
		
		/* Copy the next symbol field. Labels and gotos are tracked in a
		 * separate stack, so for these Symbols we focus on next, not
		 * jnext. */
		sym_list[i].next
			= get_new_symtab_pointer(s, curr_Sym->next, sym_list, N_Syms);
		
		/* These are only needed for symbol table pushing/popping, so I
		 * should be able to safely set them to null. */
		sym_list[i].prev = NULL;
		sym_list[i].prev_tok = NULL;
		
		/* Move on to the next symbol. */
		i++;
	}
    
    /********* define stack *********/
    
    /* Calculate the number of defines */
    Sym * curr_Def = define_stack;
    int N_Defs = 0;
    while(curr_Def != define_start) {
		curr_Def = curr_Def->prev;
		N_Defs++;
	}
    /* Allocate the Def list */
    Sym * def_list = to_return[2] = tcc_malloc(sizeof(Sym) * (N_Defs + 1));
    to_return[3] = def_list + N_Defs;
    
	/* Zero out last sym */
    def_list[N_Defs].v = 0;
    def_list[N_Defs].asm_label = NULL;
    def_list[N_Defs].r = 0;
    def_list[N_Defs].d = NULL;
    def_list[N_Defs].type.t = 0;
    def_list[N_Defs].type.ref = NULL;
    def_list[N_Defs].next = NULL;
    def_list[N_Defs].prev = NULL;
    def_list[N_Defs].prev_tok = NULL;
    
    /* Copy the define list */
    for (curr_Def = define_stack, i = 0; i < N_Defs; i++, curr_Def = curr_Def->prev) {
		/* See above for descriptions of some of the fields. */
		 
		/* Convert the symbol's token index. Undefined items are not
		 * actually cleared from the stack; instead their v field is
		 * simply set to zero. */
		if (curr_Def->v == 0) {
			def_list[i].v = 0;
		}
		else {
			def_list[i].v = curr_Def->v - tok_start
				+ _tcc_extended_symbol_counter; /* XXX double check? */
		}
		
		/* assembler label should be null for preprocessor stuff */
		if (curr_Def->asm_label != NULL) {
			tcc_warning("Unexpected assembler label for macro symbol");
		}
		def_list[i].asm_label = NULL;
		
		/* As far as I can tell, the 'r' field is not used by
		 * preprocessor macros. Just copy it. */
		def_list[i].r = curr_Def->r;
		
		/* Copy the define token stream, which is a series of bytes.
		 * The series of bytes are collections of integer => data pairs,
		 * where the integer indicates the type (and thus size) of the
		 * data that follows. For example, if we types TOK_PPNUM,
		 * TOK_STR, or TOK_LSTR, then the next few bytes are a CString
		 * struct (mostly filled with NULLs) followed by the associated
		 * character string. It gets complicated, but once we have
		 * computed the proper length of the stream, we can (with one
		 * exception) copy it verbatim. The formats of the stream are
		 * codified in tok_str_add2, which is defined in tccpp.c
		 * 
		 * The important exception noted above is that if the token is
		 * not one of the known types, then it is a plain token, which
		 * should exist on the define stack. In that case, we need to
		 * update the token number to point to one of our (to be copied
		 * further below) extended TokenSyms.
		 */
		if (curr_Def->d != NULL) {
			
			/* First compute the length and copy it... */
			int len = 0;
			int * str = curr_Def->d;
			while(str[len] != 0) {
				/* One for the type */
				len++;
				switch(str[len-1]) {
					case TOK_CINT: case TOK_CUINT: case TOK_CCHAR:
					case TOK_LCHAR: case TOK_CFLOAT: case TOK_LINENUM:
						len++;
						break;
					case TOK_PPNUM: case TOK_STR: case TOK_LSTR:
						{
							CString *cstr = (CString *)(str + len);
							/* Note this right shift assumes 32 bit integers */
							len += (sizeof(CString) + cstr->size + 3) >> 2;
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
						break;
				}
			}
			/* note the comment above about 32-bit integers */
			def_list[i].d = tcc_malloc(sizeof(int) * (len + 1));
			memcpy(def_list[i].d, curr_Def->d, sizeof(int) * (len + 1));
			
			/* ... then update TokenSym references to point to our
			 * extended symbol table.*/
			for (len = 0; str[len] != 0;len++) {
				switch(str[len]) {
					case TOK_CINT: case TOK_CUINT: case TOK_CCHAR:
					case TOK_LCHAR: case TOK_CFLOAT: case TOK_LINENUM:
						/* Skip the next stream value */
						len++;
						break;
					case TOK_PPNUM: case TOK_STR: case TOK_LSTR:
					{
						CString *cstr = (CString *)(str + len + 1);
						/* Note this right shift assumes 32 bit integers */
						len += (sizeof(CString) + cstr->size + 3) >> 2;
						/* Naively, I should set up cstr to be usable for later
						 * preprocessor expansions. See tok_str_add2 in tccpp.c
						 * for details. However, the memcpy performed above
						 * already did that! So I'm done. */
						break;
					}
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
						/* This is the case for a token stream! */
						if (str[len] < tok_start) {
						}
						else {
							def_list[i].d[len] = str[len] - tok_start
								+ _tcc_extended_symbol_counter;
						}
						
						break;
				}
			}
		}
		else {
			def_list[i].d = NULL;
		}
		
		/* Set the type. define_push and parse_define indicate that this
		 * will be either MACRO_OBJ or MACRO_FUNC. */
		def_list[i].type.t = curr_Def->type.t;
		/* Macro types should be null; issue a warning if this is not
		 * the case, just so we're aware. */
		if (curr_Def->type.ref != NULL) {
			tcc_warning("Unexpected type ref for macro symbol");
		}
		def_list[i].type.ref = NULL;
		
		/* Copy the macro token stream. All macro arguments are copied
		 * to the define stack, according to the sym_push2 in
		 * parse_define from tccpp.c. We only need to update this Sym's
		 * next; the following Sym's next will be updated when it comes
		 * across the loop. */
		/* XXX is this interpretation correct? */
		def_list[i].next
			= get_new_deftab_pointer(s, curr_Def->next, def_list, N_Defs);
		
		/* These are only needed for symbol table pushing/popping and
		 * label identification. Since these Sym objects will do no
		 * such things, I should be able to safely set them to null. */
		def_list[i].prev = NULL;
		def_list[i].prev_tok = NULL;
	}
    
    /* Set the tail pointer, which points to the first address past the
     * last element. */
    to_return[4] = to_return + N_tokens + 5;
    to_return += 5;
    
    /********* TokenSym list *********/
    
    /* Copy the tokens */
	for (i = 0; i < N_tokens; i++) {
		TokenSym * tok_copy = table_ident[tok_start + i - TOK_IDENT];
		int tokensym_size = sizeof(TokenSym) + tok_copy->len;
		TokenSym * tok_sym = to_return[i] = tcc_malloc(tokensym_size);
		
		/* Follow the code from tok_alloc_new in tccpp.c */
		tok_sym->tok = _tcc_extended_symbol_counter++;
		/* Add all the appropriate flags, of course */
		tok_sym->tok |= tok_copy->tok & (SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM);
		tok_sym->sym_define
			= get_new_deftab_pointer(s, tok_copy->sym_define, def_list, N_Defs);
		tok_sym->sym_label = NULL; /* Not copying labels */
		tok_sym->sym_struct
			= get_new_symtab_pointer(s, tok_copy->sym_struct, sym_list, N_Syms);
		tok_sym->sym_identifier
			= get_new_symtab_pointer(s, tok_copy->sym_identifier, sym_list, N_Syms);
		tok_sym->len = tok_copy->len;
		tok_sym->hash_next = NULL;
		memcpy(tok_sym->str, tok_copy->str, tok_copy->len);
		tok_sym->str[tok_copy->len] = '\0';
	}

	/* NOTE II: The guarantee of contiguity means that this function is
	 * not threadsafe as is. To make it threadsafe some day, the easiest
	 * approach is probably to make the function somehow aware of
	 * whether a function is currently copying a symbol table, and then
	 * lend a hand to help finish that copy before starting on its own
	 * copy. This way, the thread that was in the middle of its copy
	 * gets a contiguous range without forcing the second thread to idle
	 * while waiting. */
	
	/* Send the pointer to the first TokenSym* to the callback */
	extended_symtab_copy_callback to_call = s->symtab_copy_callback;
	to_call((TokenSym**)to_return, s->symtab_callback_data);
}

/* Frees memory associated with a copied extended symbol table. For a
 * description of the structure of the allocated memory, see the copy
 * function above. */
LIBTCCAPI void tcc_delete_extended_symbol_table (
	TokenSym** my_extended_symtab
) {
	Sym * sym_to_delete = (Sym*)my_extended_symtab[-5];
	Sym * last = (Sym*)my_extended_symtab[-4];
	
	/* clear out the anonymous symbol stack */
	Sym * curr_sym = (last - 1)->prev;
	Sym * prev_sym;
	while(curr_sym != NULL) {
		prev_sym = curr_sym->prev;
		tcc_free(curr_sym);
		curr_sym = prev_sym;
	}
	
	/* clear out the symbol list */
	while(sym_to_delete < last) {
		tcc_free(sym_to_delete->asm_label);
		sym_to_delete++;
	}
	tcc_free(my_extended_symtab[-5]);
	
	/* clear out the define list */
	sym_to_delete = (Sym*)my_extended_symtab[-3];
	last = (Sym*)my_extended_symtab[-2];
	while(sym_to_delete < last) {
		/* free the token stream */
		tcc_free(sym_to_delete->d);
		sym_to_delete++;
	}
	tcc_free(my_extended_symtab[-3]);
	
	/* Clear out the allocated TokenSym pointers */
	TokenSym** ts_to_delete = my_extended_symtab;
	TokenSym** done = (TokenSym**)my_extended_symtab[-1];
	while (ts_to_delete < done) {
		tcc_free(*ts_to_delete);
		ts_to_delete++;
	}
	
	/* Clear out the full memory allocation. */
	tcc_free(my_extended_symtab - 5);
}

/* We don't expose the entire TokenSym structure to the user. Thus, we
 * need to give them some way to at least know what symbol names they
 * have on hand. */
LIBTCCAPI char * tcc_tokensym_name (TokenSym * tokensym) {
	return tokensym->str;
}

LIBTCCAPI int tcc_tokensym_tok (TokenSym * tokensym) {
	return tokensym->tok;
}

LIBTCCAPI long tcc_tokensym_get_id_c(TokenSym * tokensym) {
	Sym * sym_id = tokensym->sym_identifier;
	if (sym_id == NULL) return 0;
	return sym_id->c;
}

LIBTCCAPI void tcc_tokensym_set_id_c(TokenSym * tokensym, long new_c) {
	Sym * sym_id = tokensym->sym_identifier;
	if (sym_id != NULL) sym_id->c = new_c;
}

LIBTCCAPI int tcc_tokensym_has_define (TokenSym * tokensym) {
	return tokensym->sym_define != NULL;
}
LIBTCCAPI int tcc_tokensym_has_struct (TokenSym * tokensym) {
	return tokensym->sym_struct != NULL;
}
LIBTCCAPI int tcc_tokensym_has_identifier (TokenSym * tokensym) {
	return tokensym->sym_identifier != NULL;
}
LIBTCCAPI int tcc_tokensym_is_shareable (TokenSym * tokensym) {
	return	tokensym->sym_define != NULL
			|| tokensym->sym_struct != NULL
			|| tokensym->sym_identifier != NULL
			;
}

LIBTCCAPI int tcc_tokensym_no_extra_bits(int tok) {
	return (~(SYM_STRUCT | SYM_FIELD | SYM_FIRST_ANOM) & tok);
}

LIBTCCAPI TokenSym* tcc_tokensym_by_tok(int tok, TokenSym ** list) {
	int tok_no_flags = tcc_tokensym_no_extra_bits(tok);
	int first_tok = tcc_tokensym_no_extra_bits(list[0]->tok);
	if (tok_no_flags < first_tok) return 0;
	TokenSym ** tail = *(TokenSym***)(list - 1) - 1;
	if (tok_no_flags > tcc_tokensym_no_extra_bits((*tail)->tok)) return 0;
	return list[tok_no_flags - first_tok];
}


/* We also don't provide a means for the user to know if they've reached
 * the end of the list. Instead, we provide a function to get the number
 * of TokenSyms. */
LIBTCCAPI int tcc_tokensym_list_length (TokenSym ** list) {
	TokenSym ** tail = *(TokenSym***)(list - 1);
	return (int)(tail - list);
}
