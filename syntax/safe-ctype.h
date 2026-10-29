/* syntax/safe-ctype.h --- The character classes of <ctype.h>, for any code point.
   Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

   Included before each grammar's own code by tools/grammars.sh. Scanners
   give <ctype.h>'s functions the character ahead, which may be any code
   point, while those take only an unsigned char or EOF: glibc answers from a
   table, and a code point past its end reads memory that is not the table,
   or no memory at all, and the grammar crashes. Here each answers for any
   value as the C locale does, a code point past 255 being of no class. */

#ifndef TUATARA_SAFE_CTYPE_H
#define TUATARA_SAFE_CTYPE_H

#include <ctype.h>

#define TUATARA_CLASS(name) \
  static inline int tuatara_##name(int c) { return (c >= -1 && c <= 255) ? (name)(c) : 0; }

TUATARA_CLASS(isalnum)
TUATARA_CLASS(isalpha)
TUATARA_CLASS(isblank)
TUATARA_CLASS(iscntrl)
TUATARA_CLASS(isdigit)
TUATARA_CLASS(isgraph)
TUATARA_CLASS(islower)
TUATARA_CLASS(isprint)
TUATARA_CLASS(ispunct)
TUATARA_CLASS(isspace)
TUATARA_CLASS(isupper)
TUATARA_CLASS(isxdigit)

static inline int tuatara_tolower(int c) { return (c >= -1 && c <= 255) ? (tolower)(c) : c; }
static inline int tuatara_toupper(int c) { return (c >= -1 && c <= 255) ? (toupper)(c) : c; }

#undef isalnum
#undef isalpha
#undef isblank
#undef iscntrl
#undef isdigit
#undef isgraph
#undef islower
#undef isprint
#undef ispunct
#undef isspace
#undef isupper
#undef isxdigit
#undef tolower
#undef toupper
#define isalnum tuatara_isalnum
#define isalpha tuatara_isalpha
#define isblank tuatara_isblank
#define iscntrl tuatara_iscntrl
#define isdigit tuatara_isdigit
#define isgraph tuatara_isgraph
#define islower tuatara_islower
#define isprint tuatara_isprint
#define ispunct tuatara_ispunct
#define isspace tuatara_isspace
#define isupper tuatara_isupper
#define isxdigit tuatara_isxdigit
#define tolower tuatara_tolower
#define toupper tuatara_toupper

#endif
