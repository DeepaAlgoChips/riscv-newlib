/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
FUNCTION
<<vfprintf>>, <<vprintf>>, <<vsprintf>>, <<vsnprintf>>, <<vasprintf>>, <<vasnprintf>>---format argument list

INDEX
	vfprintf
INDEX
	_vfprintf_r
INDEX
	vprintf
INDEX
	_vprintf_r
INDEX
	vsprintf
INDEX
	_vsprintf_r
INDEX
	vsnprintf
INDEX
	_vsnprintf_r
INDEX
	vasprintf
INDEX
	_vasprintf_r
INDEX
	vasnprintf
INDEX
	_vasnprintf_r

SYNOPSIS
	#include <stdio.h>
	#include <stdarg.h>
	int vprintf(const char *<[fmt]>, va_list <[list]>);
	int vfprintf(FILE *<[fp]>, const char *<[fmt]>, va_list <[list]>);
	int vsprintf(char *<[str]>, const char *<[fmt]>, va_list <[list]>);
	int vsnprintf(char *<[str]>, size_t <[size]>, const char *<[fmt]>,
                      va_list <[list]>);
	int vasprintf(char **<[strp]>, const char *<[fmt]>, va_list <[list]>);
	char *vasnprintf(char *<[str]>, size_t *<[size]>, const char *<[fmt]>,
                         va_list <[list]>);

	int _vprintf_r(struct _reent *<[reent]>, const char *<[fmt]>,
                        va_list <[list]>);
	int _vfprintf_r(struct _reent *<[reent]>, FILE *<[fp]>,
                        const char *<[fmt]>, va_list <[list]>);
	int _vsprintf_r(struct _reent *<[reent]>, char *<[str]>,
                        const char *<[fmt]>, va_list <[list]>);
	int _vasprintf_r(struct _reent *<[reent]>, char **<[str]>,
                         const char *<[fmt]>, va_list <[list]>);
	int _vsnprintf_r(struct _reent *<[reent]>, char *<[str]>,
                         size_t <[size]>, const char *<[fmt]>, va_list <[list]>);
	char *_vasnprintf_r(struct _reent *<[reent]>, char *<[str]>,
                            size_t *<[size]>, const char *<[fmt]>, va_list <[list]>);

DESCRIPTION
<<vprintf>>, <<vfprintf>>, <<vasprintf>>, <<vsprintf>>, <<vsnprintf>>,
and <<vasnprintf>> are (respectively) variants of <<printf>>,
<<fprintf>>, <<asprintf>>, <<sprintf>>, <<snprintf>>, and
<<asnprintf>>.  They differ only in allowing their caller to pass the
variable argument list as a <<va_list>> object (initialized by
<<va_start>>) rather than directly accepting a variable number of
arguments.  The caller is responsible for calling <<va_end>>.

<<_vprintf_r>>, <<_vfprintf_r>>, <<_vasprintf_r>>, <<_vsprintf_r>>,
<<_vsnprintf_r>>, and <<_vasnprintf_r>> are reentrant versions of the
above.

RETURNS
The return values are consistent with the corresponding functions.

PORTABILITY
ANSI C requires <<vprintf>>, <<vfprintf>>, <<vsprintf>>, and
<<vsnprintf>>.  The remaining functions are newlib extensions.

Supporting OS subroutines required: <<close>>, <<fstat>>, <<isatty>>,
<<lseek>>, <<read>>, <<sbrk>>, <<write>>.
*/

#if defined(LIBC_SCCS) && !defined(lint)
/*static char *sccsid = "from: @(#)vfprintf.c	5.50 (Berkeley) 12/16/92";*/
static char *rcsid = "$Id$";
#endif /* LIBC_SCCS and not lint */

/*
 * Actual printf innards.
 *
 * This code is large and complicated...
 */
#include <newlib.h>

#ifdef INTEGER_ONLY
# define VFPRINTF vfiprintf
# ifdef STRING_ONLY
#   define _VFPRINTF_R _svfiprintf_r
# else
#   define _VFPRINTF_R _vfiprintf_r
# endif
#else
# define VFPRINTF vfprintf
# ifdef STRING_ONLY
#   define _VFPRINTF_R _svfprintf_r
# else
#   define _VFPRINTF_R _vfprintf_r
# endif
# ifndef NO_FLOATING_POINT
#  define FLOATING_POINT
# endif
#endif

#define _NO_POS_ARGS
#ifdef _WANT_IO_POS_ARGS
# undef _NO_POS_ARGS
#endif

#include <_ansi.h>
#include <reent.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <stdint.h>
#include <wchar.h>
#include <sys/lock.h>
#include <stdarg.h>
#include "local.h"
#include "../stdlib/local.h"
#include "fvwrite.h"
#include "vfieeefp.h"

/* Currently a test is made to see if long double processing is warranted.
   This could be changed in the future should the _ldtoa_r code be
   preferred over _dtoa_r.  */
#define _NO_LONGDBL
#if defined _WANT_IO_LONG_DOUBLE && (LDBL_MANT_DIG > DBL_MANT_DIG)
#undef _NO_LONGDBL
#endif



#ifdef STRING_ONLY
# ifdef _FVWRITE_IN_STREAMIO
#  define __SPRINT __ssprint_r
# else
#  define __SPRINT __ssputs_r
# endif
#else
# ifdef _FVWRITE_IN_STREAMIO
#  define __SPRINT __sprint_r
# else
#  define __SPRINT __sfputs_r
# endif
#endif

/* The __sprint_r/__ssprint_r functions are shared between all versions of
   vfprintf and vfwprintf.  They must only be defined once, which we do in
   the INTEGER_ONLY versions here. */
#ifdef STRING_ONLY
#ifdef INTEGER_ONLY
#ifndef _FVWRITE_IN_STREAMIO

#endif

int
__ssprint_r (struct _reent *ptr,
       FILE *fp,
       register struct __suio *uio)
{ 
   
	register size_t len;
	register int w;
	register struct __siov *iov;
	register const char *p = NULL;

	iov = uio->uio_iov;
	len = 0;

	if (uio->uio_resid == 0) {
		uio->uio_iovcnt = 0;
		return (0);
	}

        do {
		while (len == 0) {
			p = iov->iov_base;
			len = iov->iov_len;
			iov++;
		}
		w = fp->_w;
		if (len >= w && fp->_flags & (__SMBF | __SOPT)) {
			/* must be asprintf family */
			unsigned char *str;
			int curpos = (fp->_p - fp->_bf._base);
			/* Choose a geometric growth factor to avoid
		 	 * quadratic realloc behavior, but use a rate less
			 * than (1+sqrt(5))/2 to accomodate malloc
		 	 * overhead. asprintf EXPECTS us to overallocate, so
		 	 * that it can add a trailing \0 without
		 	 * reallocating.  The new allocation should thus be
		 	 * max(prev_size*1.5, curpos+len+1). */
			int newsize = fp->_bf._size * 3 / 2;
			if (newsize < curpos + len + 1)
				newsize = curpos + len + 1;
			if (fp->_flags & __SOPT)
			{
				/* asnprintf leaves original buffer alone.  */
				str = (unsigned char *)_malloc_r (ptr, newsize);
				if (!str)
				{
					ptr->_errno = ENOMEM;
					goto err;
				}
				memcpy (str, fp->_bf._base, curpos);
				fp->_flags = (fp->_flags & ~__SOPT) | __SMBF;
			}
			else
			{
				str = (unsigned char *)_realloc_r (ptr, fp->_bf._base,
						newsize);
				if (!str) {
					/* Free unneeded buffer.  */
					_free_r (ptr, fp->_bf._base);
					/* Ensure correct errno, even if free
					 * changed it.  */
					ptr->_errno = ENOMEM;
					goto err;
				}
			}
			fp->_bf._base = str;
			fp->_p = str + curpos;
			fp->_bf._size = newsize;
			w = len;
			fp->_w = newsize - curpos;
		}
		if (len < w)
			w = len;
		(void)memmove ((void *) fp->_p, (void *) p, (size_t) (w));
		fp->_w -= w;
		fp->_p += w;
		w = len;          /* pretend we copied all */
		p += w;
		len -= w;
        } while ((uio->uio_resid -= w) != 0);

	uio->uio_resid = 0;
	uio->uio_iovcnt = 0;
	return 0;

err:
  fp->_flags |= __SERR;
  uio->uio_resid = 0;
  uio->uio_iovcnt = 0;
  return EOF;
}
#else /* !INTEGER_ONLY */
#ifndef _FVWRITE_IN_STREAMIO
int __ssputs_r (struct _reent *, FILE *, const char *, size_t);
#endif
int __ssprint_r (struct _reent *, FILE *, register struct __suio *);
#endif /* !INTEGER_ONLY */

#else /* !STRING_ONLY */
#ifdef INTEGER_ONLY

#ifndef _FVWRITE_IN_STREAMIO

#endif
/*
 * Flush out all the vectors defined by the given uio,
 * then reset it so that it can be reused.
 */
int
__sprint_r (struct _reent *ptr,
       FILE *fp,
       register struct __suio *uio)
{
	register int err = 0;

	if (uio->uio_resid == 0) {
		uio->uio_iovcnt = 0;
		return (0);
	}
#ifdef _WIDE_ORIENT
	if (fp->_flags2 & __SWID) {
		struct __siov *iov;
		wchar_t *p;
		int i, len;

		iov = uio->uio_iov;
		for (; uio->uio_resid != 0;
		     uio->uio_resid -= len * sizeof (wchar_t), iov++) {
			p = (wchar_t *) iov->iov_base;
			len = iov->iov_len / sizeof (wchar_t);
			for (i = 0; i < len; i++) {
				if (_fputwc_r (ptr, p[i], fp) == WEOF) {
					err = -1;
					goto out;
				}
			}
		}
	} else
#endif
		err = __sfvwrite_r(ptr, fp, uio);
out:
	uio->uio_resid = 0;
	uio->uio_iovcnt = 0;
	return (err);
}
#else /* !INTEGER_ONLY */
#ifndef _FVWRITE_IN_STREAMIO
int __sfputs_r (struct _reent *, FILE *, const char *buf, size_t);
#endif
int __sprint_r (struct _reent *, FILE *, register struct __suio *);
#endif /* !INTEGER_ONLY */


#endif /* !STRING_ONLY */


#if defined (FLOATING_POINT) || defined (_WANT_IO_C99_FORMATS)
# include <locale.h>
#endif
#ifdef FLOATING_POINT
# include <math.h>

/* For %La, an exponent of 15 bits occupies the exponent character, a
   sign, and up to 5 digits.  */
# define MAXEXPLEN		7
# define DEFPREC		6



extern char *_ldtoa_r (struct _reent *, _LONG_DOUBLE, int,
			      int, int *, int *, char **);

extern int _ldcheck (_LONG_DOUBLE *);

#  define _PRINTF_FLOAT_TYPE _LONG_DOUBLE
#  define _DTOA_R _ldtoa_r
/* FIXME - frexpl is not yet supported; and cvt infloops if (double)f
   converts a finite value into infinity.  */
/* #  define FREXP frexpl */
#  define FREXP(f,e) ((_LONG_DOUBLE) frexp ((double)f, e))


static char *cvt(struct _reent *, _PRINTF_FLOAT_TYPE, int, int, char *, int *,
                 int, int *, char *);

static int exponent(char *, int, int);

#endif /* FLOATING_POINT */

/* BUF must be big enough for the maximum %#llo (assuming long long is
   at most 64 bits, this would be 23 characters), the maximum
   multibyte character %C, and the maximum default precision of %La
   (assuming long double is at most 128 bits with 113 bits of
   mantissa, this would be 29 characters).  %e, %f, and %g use
   reentrant storage shared with mprec.  All other formats that use
   buf get by with fewer characters.  Making BUF slightly bigger
   reduces the need for malloc in %.*a and %S, when large precision or
   long strings are processed.
   The bigger size of 100 bytes is used on systems which allow number
   strings using the locale's grouping character.  Since that's a multibyte
   value, we should use a conservative value.
   */
#ifdef _WANT_IO_C99_FORMATS
#define	BUF		100
#else
#define	BUF		40
#endif
#if defined _MB_CAPABLE && MB_LEN_MAX > BUF
# undef BUF
# define BUF MB_LEN_MAX
#endif







#define	to_digit(c)	((c) - '0')
#define is_digit(c)	((unsigned)to_digit (c) <= 9)
#define	to_char(n)	((n) + '0')

/*
 * Flags used during conversion.
 */
#define	ALT		0x001		/* alternate form */
#define	HEXPREFIX	0x002		/* add 0x or 0X prefix */
#define	LADJUST		0x004		/* left adjustment */
#define	LONGDBL		0x008		/* long double */


#define	ZEROPAD		0x080		/* zero (as opposed to blank) pad */
#define FPT		0x100		/* Floating point number */
#ifdef _WANT_IO_C99_FORMATS

#else /* define as 0, to make SARG and UARG occupy fewer instructions  */
# define CHARINT	0
#endif
#ifdef _WANT_IO_C99_FORMATS
# define GROUPING	0x400		/* use grouping ("'" flag) */
#endif

int _VFPRINTF_R (struct _reent *, FILE *, const char *, va_list);

#ifndef STRING_ONLY
int
VFPRINTF (FILE * fp,
       const char *fmt0,
       va_list ap)
{
  int result;
  result = _VFPRINTF_R (_REENT, fp, fmt0, ap);
  return result;
}
#endif /* STRING_ONLY */

int
_VFPRINTF_R (struct _reent *data,
       FILE * fp,
       const char *fmt0,
       va_list ap)
{
	register char *fmt;	/* format string */
	register int ch;	/* character from fmt */
	register int n, m;	/* handy integers (short term usage) */
	register char *cp;	/* handy char pointer (short term usage) */
	register int flags;	/* flags as above */
	char *fmt_anchor;       /* current format spec being processed */
	extern int puts(char const *);


	int ret;		/* return value accumulator */
	int width;		/* width from format (%8d), or 0 */
	int prec;		/* precision from format (%.3d), or -1 */
	char sign;		/* sign prefix (' ', '+', '-', or \0) */
#ifdef _WANT_IO_C99_FORMATS
				/* locale specific numeric grouping */
	char *thousands_sep = NULL;
	size_t thsnd_len = 0;
	const char *grouping = NULL;
#endif

#ifdef FLOATING_POINT
	char *decimal_point = _localeconv_r (data)->decimal_point;
	size_t decp_len = strlen (decimal_point);
	char softsign;		/* temporary negative sign for floats */
	union { int i; _PRINTF_FLOAT_TYPE fp; } _double_ = {0};
# define _fpvalue (_double_.fp)
	int expt;		/* integer value of exponent */
	int expsize = 0;	/* character count for expstr */
	char expstr[MAXEXPLEN];	/* buffer for exponent string */
	int lead;		/* sig figs before decimal or group sep */
#endif /* FLOATING_POINT */
#if defined (FLOATING_POINT) || defined (_WANT_IO_C99_FORMATS)
	int ndig = 0;		/* actual number of digits returned by cvt */
#endif
#if defined (FLOATING_POINT) && defined (_WANT_IO_C99_FORMATS)
	int nseps;		/* number of group separators with ' */
	int nrepeats;		/* number of repeats of the last group */
#endif
	
	enum { OCT, DEC, HEX } base;/* base for [diouxX] conversion */
	int dprec;		/* a copy of prec if [diouxX], 0 otherwise */
	int realsz;		/* field size expanded by dprec */
	int size;		/* size of converted field or string */
	char *xdigs = NULL;	/* digits for [xX] conversion */
#ifdef _FVWRITE_IN_STREAMIO
#define NIOV 8
	struct __suio uio;	/* output information: summary */
	struct __siov iov[NIOV];/* ... and individual io vectors */
	register struct __siov *iovp;/* for PRINT macro */
#endif
	char buf[BUF];		/* space for %c, %S, %[diouxX], %[aA] */
	char ox[2];		/* space for 0x hex-prefix */

	char *malloc_buf = NULL;/* handy pointer for malloced buffers */

	/*
	 * Choose PADSIZE to trade efficiency vs. size.  If larger printf
	 * fields occur frequently, increase PADSIZE and make the initialisers
	 * below longer.
	 */
#define	PADSIZE	16		/* pad chunk size */
	static const char blanks[PADSIZE] =
	 {' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '};
	static const char zeroes[PADSIZE] =
	 {'0','0','0','0','0','0','0','0','0','0','0','0','0','0','0','0'};


	/*
	 * BEWARE, these `goto error' on error, and PAD uses `n'.
	 */
#ifdef _FVWRITE_IN_STREAMIO
#define	PRINT(ptr, len) { \
	iovp->iov_base = (ptr); \
	iovp->iov_len = (len); \
	uio.uio_resid += (len); \
	iovp++; \
	if (++uio.uio_iovcnt >= NIOV) { \
		if (__SPRINT(data, fp, &uio)) \
			goto error; \
		iovp = iov; \
	} \
}
#define	PAD(howmany, with) { \
	if ((n = (howmany)) > 0) { \
		while (n > PADSIZE) { \
			PRINT (with, PADSIZE); \
			n -= PADSIZE; \
		} \
		PRINT (with, n); \
	} \
}
#define PRINTANDPAD(p, ep, len, with) { \
	int n = (ep) - (p); \
	if (n > (len)) \
		n = (len); \
	if (n > 0) \
		PRINT((p), n); \
	PAD((len) - (n > 0 ? n : 0), (with)); \
}
#define	FLUSH() { \
	if (uio.uio_resid && __SPRINT(data, fp, &uio)) \
		goto error; \
	uio.uio_iovcnt = 0; \
	iovp = iov; \
}
#else
#define PRINT(ptr, len) {		\
	if (__SPRINT (data, fp, (ptr), (len)) == EOF) \
		goto error;		\
}
#define	PAD(howmany, with) {		\
	if ((n = (howmany)) > 0) {	\
		while (n > PADSIZE) {	\
			PRINT (with, PADSIZE);	\
			n -= PADSIZE;	\
		}			\
		PRINT (with, n);	\
	}				\
}
#define PRINTANDPAD(p, ep, len, with) {	\
	int n = (ep) - (p);		\
	if (n > (len))			\
		n = (len);		\
	if (n > 0)			\
		PRINT((p), n);		\
	PAD((len) - (n > 0 ? n : 0), (with)); \
}
#define FLUSH()
#endif

	


# define GET_ARG(n, ap, type) (va_arg (ap, type))


	/*
	 * To extend shorts properly, we need both signed and unsigned
	 * argument extraction methods.
	 */

#define	SARG() \
	(flags&QUADINT ? GET_ARG (N, ap, quad_t) : \
	    flags&LONGINT ? GET_ARG (N, ap, long) : \
	    flags&SHORTINT ? (long)(short)GET_ARG (N, ap, int) : \
	    flags&CHARINT ? (long)(signed char)GET_ARG (N, ap, int) : \
	    (long)GET_ARG (N, ap, int))
	    
#define	UARG() \
	(flags&QUADINT ? GET_ARG (N, ap, u_quad_t) : \
	    flags&LONGINT ? GET_ARG (N, ap, u_long) : \
	    flags&SHORTINT ? (u_long)(u_short)GET_ARG (N, ap, int) : \
	    flags&CHARINT ? (u_long)(unsigned char)GET_ARG (N, ap, int) : \
	    (u_long)GET_ARG (N, ap, u_int))



#ifndef STRING_ONLY
	/* Initialize std streams if not dealing with sprintf family.  */
	CHECK_INIT (data, fp);
	_newlib_flockfile_start (fp);

	ORIENT(fp, -1);

	/* sorry, fprintf(read_only_file, "") returns EOF, not 0 */
	if (cantwrite (data, fp)) {
		_newlib_flockfile_exit (fp);
		return (EOF);
	}


#else /* STRING_ONLY */
        /* Create initial buffer if we are called by asprintf family.  */
        if (fp->_flags & __SMBF && !fp->_bf._base)
        {
		fp->_bf._base = fp->_p = _malloc_r (data, 64);
		if (!fp->_p)
		{
			data->_errno = ENOMEM;
			return EOF;
		}
		fp->_bf._size = 64;
        }
#endif /* STRING_ONLY */

	fmt = (char *)fmt0;
#ifdef _FVWRITE_IN_STREAMIO
	uio.uio_iov = iovp = iov;
	uio.uio_resid = 0;
	uio.uio_iovcnt = 0;
#endif
	ret = 0;


	/*
	 * Scan the format for conversions (`%' character).
	 */
	for (;;) {
	        cp = fmt;

                while (*fmt != '\0' && *fmt != '%')
                    fmt += 1;

		if ((m = fmt - cp) != 0) {
			PRINT (cp, m);
			ret += m;
		}
          if (*fmt == '\0')
                    goto done;

		fmt_anchor = fmt;
		fmt++;		/* skip over '%' */

		flags = 0;
		dprec = 0;
		width = 0;
		prec = -1;
		sign = '\0';
#ifdef FLOATING_POINT
		lead = 0;
#ifdef _WANT_IO_C99_FORMATS
		nseps = nrepeats = 0;
#endif
#endif


rflag:		ch = *fmt++;
reswitch:	switch (ch) {
#ifdef _WANT_IO_C99_FORMATS
		case '\'':
			thousands_sep = _localeconv_r (data)->thousands_sep;
			thsnd_len = strlen (thousands_sep);
			grouping = _localeconv_r (data)->grouping;
			if (thsnd_len > 0 && grouping && *grouping)
			  flags |= GROUPING;
			goto rflag;
#endif
		case ' ':
		    puts("inside space");
			/*
			 * ``If the space and + flags both appear, the space
			 * flag will be ignored.''
			 *	-- ANSI X3J11
			 */
			if (!sign)
				sign = ' ';
			goto rflag;
		case '#':
			flags |= ALT;
			goto rflag;
		case '*':


			/*
			 * ``A negative field width argument is taken as a
			 * - flag followed by a positive field width.''
			 *	-- ANSI X3J11
			 * They don't exclude field widths read from args.
			 */
			width = GET_ARG (n, ap, int);

			if (width >= 0)
				goto rflag;
			width = -width;
			/* FALLTHROUGH */
		case '-':
		    puts("inside -");
			flags |= LADJUST;
			goto rflag;
		case '+':
		    puts("inside +");
			sign = '+';
			goto rflag;
		case '.': 
		  
		    puts("inside .");
			if ((ch = *fmt++) == '*') {

				prec = GET_ARG (n, ap, int);

				if (prec < 0)
					prec = -1;
				goto rflag;
			}
			n = 0;
			while (is_digit (ch)) {
				n = 10 * n + to_digit (ch);
				ch = *fmt++;
			}
			prec = n < 0 ? -1 : n;
			goto reswitch;
		case '0':
			/*
			 * ``Note that 0 is taken as a flag, not as the
			 * beginning of a field width.''
			 *	-- ANSI X3J11
			 */
			flags |= ZEROPAD;
			goto rflag;
		case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			n = 0;
			do {
				n = 10 * n + to_digit (ch);
				ch = *fmt++;
			} while (is_digit (ch));

           
			width = n;
			goto reswitch;
#ifdef FLOATING_POINT
		case 'L':
			flags |= LONGDBL;
			goto rflag;
#endif
		

		
#ifdef FLOATING_POINT
# ifdef _WANT_IO_C99_FORMATS
		case 'a':
		case 'A':
		case 'F':
# endif
		case 'e':
		case 'E':
		case 'f':
		case 'g':
		case 'G':


			if (flags & LONGDBL) {
				_fpvalue = GET_ARG (N, ap, _LONG_DOUBLE);
			} else {
				_fpvalue = (_LONG_DOUBLE)GET_ARG (N, ap, double);
			}

			/* do this before tricky precision changes */
			expt = _ldcheck (&_fpvalue);
			if (expt == 2) {
				if (_fpvalue < 0)
					sign = '-';
				if (ch <= 'G') /* 'A', 'E', 'F', or 'G' */
					cp = "INF";
				else
					cp = "inf";
				size = 3;
				flags &= ~ZEROPAD;
				break;
			}
			if (expt == 1) {
				if (signbit (_fpvalue))
					sign = '-';
				if (ch <= 'G') /* 'A', 'E', 'F', or 'G' */
					cp = "NAN";
				else
					cp = "nan";
				size = 3;
				flags &= ~ZEROPAD;
				break;
			}


# ifdef _WANT_IO_C99_FORMATS
			if (ch == 'a' || ch == 'A') {
				ox[0] = '0';
				ox[1] = ch == 'a' ? 'x' : 'X';
				flags |= HEXPREFIX;
				if (prec >= BUF)
				  {
				    if ((malloc_buf =
					 (char *)_malloc_r (data, prec + 1))
					== NULL)
				      {
					fp->_flags |= __SERR;
					goto error;
				      }
				    cp = malloc_buf;
				  }
				else
				  cp = buf;
			} else
# endif /* _WANT_IO_C99_FORMATS */
			if (prec == -1) {
				prec = DEFPREC;
			} else if ((ch == 'g' || ch == 'G') && prec == 0) {
				prec = 1;
			}

			flags |= FPT;

			cp = cvt (data, _fpvalue, prec, flags, &softsign,
				  &expt, ch, &ndig, cp);

			if (ch == 'g' || ch == 'G') {
				if (expt <= -4 || expt > prec)
					ch -= 2; /* 'e' or 'E' */
				else
					ch = 'g';
			}
# ifdef _WANT_IO_C99_FORMATS
			else if (ch == 'F')
				ch = 'f';
# endif
			if (ch <= 'e') {	/* 'a', 'A', 'e', or 'E' fmt */
				--expt;
				expsize = exponent (expstr, expt, ch);
				size = expsize + ndig;
				if (ndig > 1 || flags & ALT)
					size += decp_len;
# ifdef _WANT_IO_C99_FORMATS
				flags &= ~GROUPING;
# endif
			} else {
				if (ch == 'f') {		/* f fmt */
					if (expt > 0) {
						size = expt;
						if (prec || flags & ALT)
							size += prec + decp_len;
					} else	/* "0.X" */
						size = (prec || flags & ALT)
							  ? prec + 1 + decp_len
							  : 1;
				} else if (expt >= ndig) { /* fixed g fmt */
					size = expt;
					if (flags & ALT)
						size += decp_len;
				} else {
					size = ndig + decp_len;
					if (expt <= 0)
						size += 1 - expt;
				}
# ifdef _WANT_IO_C99_FORMATS
				if ((flags & GROUPING) && expt > 0) {
					/* space for thousands' grouping */
					nseps = nrepeats = 0;
					lead = expt;
					while (*grouping != CHAR_MAX) {
						if (lead <= *grouping)
							break;
						lead -= *grouping;
						if (grouping[1]) {
							nseps++;
							grouping++;
						} else
							nrepeats++;
					}
					size += (nseps + nrepeats) * thsnd_len;
				} else
# endif
					lead = expt;
			}

			if (softsign)
				sign = '-';
			break;
#endif /* FLOATING_POINT */

		
			
		


			if (prec >= 0) {
				/*
				 * can't use strlen; can only look for the
				 * NUL in the first `prec' characters, and
				 * strlen () will go further.
				 */
				char *p = memchr (cp, 0, prec);

				if (p != NULL)
					size = p - cp;
				else
					size = prec;
			} else
				size = strlen (cp);

			break;
		
		
		
		default:	/* "%?" prints ?, unless ? is NUL */
			if (ch == '\0')
				goto done;
			/* pretend it was %c with argument ch */
			cp = buf;
			*cp = ch;
			size = 1;
			sign = '\0';
			break;
		}

		/*
		 * All reasonable formats wind up here.  At this point, `cp'
		 * points to a string which (if not flags&LADJUST) should be
		 * padded out to `width' places.  If flags&ZEROPAD, it should
		 * first be prefixed by any sign or other prefix; otherwise,
		 * it should be blank padded before the prefix is emitted.
		 * After any left-hand padding and prefixing, emit zeroes
		 * required by a decimal [diouxX] precision, then print the
		 * string proper, then emit zeroes required by any leftover
		 * floating precision; finally, if LADJUST, pad with blanks.
		 * If flags&FPT, ch must be in [aAeEfg].
		 *
		 * Compute actual size, so we know how much to pad.
		 * size excludes decimal prec; realsz includes it.
		 */
		realsz = dprec > size ? dprec : size;
		if (sign)
			realsz++;
		if (flags & HEXPREFIX)
			realsz+= 2;

		/* right-adjusting blank padding */
		if ((flags & (LADJUST|ZEROPAD)) == 0)
			PAD (width - realsz, blanks);

		/* prefix */
		if (sign)
			PRINT (&sign, 1);
		if (flags & HEXPREFIX)
			PRINT (ox, 2);

		/* right-adjusting zero padding */
		if ((flags & (LADJUST|ZEROPAD)) == ZEROPAD)
			PAD (width - realsz, zeroes);

		/* leading zeroes from decimal precision */
		PAD (dprec - size, zeroes);

		/* the string or number proper */
#ifdef FLOATING_POINT
		if ((flags & FPT) == 0) {
			PRINT (cp, size);
		} else {	/* glue together f_p fragments */
			if (ch >= 'f') {	/* 'f' or 'g' */
				if (_fpvalue == 0) {
					/* kludge for __dtoa irregularity */
					PRINT ("0", 1);
					if (expt < ndig || flags & ALT) {
						PRINT (decimal_point, decp_len);
						PAD (ndig - 1, zeroes);
					}
				} else if (expt <= 0) {
					PRINT ("0", 1);
					if (expt || ndig || flags & ALT) {
						PRINT (decimal_point, decp_len);
						PAD (-expt, zeroes);
						PRINT (cp, ndig);
					}
				} else {
					char *convbuf = cp;
					PRINTANDPAD(cp, convbuf + ndig,
						    lead, zeroes);
					cp += lead;
#ifdef _WANT_IO_C99_FORMATS
					if (flags & GROUPING) {
					    while (nseps > 0 || nrepeats > 0) {
						if (nrepeats > 0)
						    nrepeats--;
						else {
						    grouping--;
						    nseps--;
						}
						PRINT(thousands_sep, thsnd_len);
						PRINTANDPAD (cp, convbuf + ndig,
							     *grouping, zeroes);
						cp += *grouping;
					    }
					    if (cp > convbuf + ndig)
						cp = convbuf + ndig;
					}
#endif
					if (expt < ndig || flags & ALT)
					    PRINT (decimal_point, decp_len);
					PRINTANDPAD (cp, convbuf + ndig,
						     ndig - expt, zeroes);
				}
			} else {	/* 'a', 'A', 'e', or 'E' */
				if (ndig > 1 || flags & ALT) {
					PRINT (cp, 1);
					cp++;
					PRINT (decimal_point, decp_len);
					if (_fpvalue) {
						PRINT (cp, ndig - 1);
					} else	/* 0.[0..] */
						/* __dtoa irregularity */
						PAD (ndig - 1, zeroes);
				} else	/* XeYYY */
					PRINT (cp, 1);
				PRINT (expstr, expsize);
			}
		}
#else /* !FLOATING_POINT */
		PRINT (cp, size);
#endif
		/* left-adjusting padding (always blank) */
		if (flags & LADJUST)
			PAD (width - realsz, blanks);

		/* finally, adjust ret */
		ret += width > realsz ? width : realsz;

		FLUSH ();	/* copy out the I/O vectors */

                if (malloc_buf != NULL) {
			_free_r (data, malloc_buf);
			malloc_buf = NULL;
		}
	}
done:
	FLUSH ();
error:
	if (malloc_buf != NULL)
		_free_r (data, malloc_buf);
#ifndef STRING_ONLY
	_newlib_flockfile_end (fp);
#endif
	return (__sferror (fp) ? EOF : ret);
	/* NOTREACHED */
}

#ifdef FLOATING_POINT

/* Using reentrant DATA, convert finite VALUE into a string of digits
   with no decimal point, using NDIGITS precision and FLAGS as guides
   to whether trailing zeros must be included.  Set *SIGN to nonzero
   if VALUE was negative.  Set *DECPT to the exponent plus one.  Set
   *LENGTH to the length of the returned string.  CH must be one of
   [aAeEfFgG]; if it is [aA], then the return string lives in BUF,
   otherwise the return value shares the mprec reentrant storage.  */
static char *
cvt(struct _reent *data, _PRINTF_FLOAT_TYPE value, int ndigits, int flags,
    char *sign, int *decpt, int ch, int *length, char *buf)
{
	int mode, dsgn;
	char *digits, *bp, *rve;

	union
	{
	  struct ldieee ieee;
	  _LONG_DOUBLE val;
	} ld;

	ld.val = value;
	if (ld.ieee.sign) { /* this will check for < 0 and -0.0 */
		value = -value;
		*sign = '-';
	} else
		*sign = '\000';


# ifdef _WANT_IO_C99_FORMATS
	if (ch == 'a' || ch == 'A') {
		/* This code assumes FLT_RADIX is a power of 2.  The initial
		   division ensures the digit before the decimal will be less
		   than FLT_RADIX (unless it is rounded later).	 There is no
		   loss of precision in these calculations.  */
		value = FREXP (value, decpt) / 8;
		if (!value)
			*decpt = 1;
		digits = ch == 'a' ? "0123456789abcdef" : "0123456789ABCDEF";
		bp = buf;
		do {
			value *= 16;
			mode = (int) value;
			value -= mode;
			*bp++ = digits[mode];
		} while (ndigits-- && value);
		if (value > 0.5 || (value == 0.5 && mode & 1)) {
			/* round to even */
			rve = bp;
			while (*--rve == digits[0xf]) {
				*rve = '0';
			}
			*rve = *rve == '9' ? digits[0xa] : *rve + 1;
		} else {
			while (ndigits-- >= 0) {
				*bp++ = '0';
			}
		}
		*length = bp - buf;
		return buf;
	}
# endif /* _WANT_IO_C99_FORMATS */
	if (ch == 'f' || ch == 'F') {
		mode = 3;		/* ndigits after the decimal point */
	} else {
		/* To obtain ndigits after the decimal point for the 'e'
		 * and 'E' formats, round to ndigits + 1 significant
		 * figures.
		 */
		if (ch == 'e' || ch == 'E') {
			ndigits++;
		}
		mode = 2;		/* ndigits significant digits */
	}

	digits = _DTOA_R (data, value, mode, ndigits, decpt, &dsgn, &rve);

	if ((ch != 'g' && ch != 'G') || flags & ALT) {	/* Print trailing zeros */
		bp = digits + ndigits;
		if (ch == 'f' || ch == 'F') {
			if (*digits == '0' && value)
				*decpt = -ndigits + 1;
			bp += *decpt;
		}
		if (value == 0)	/* kludge for __dtoa irregularity */
			rve = bp;
		while (rve < bp)
			*rve++ = '0';
	}
	*length = rve - digits;
	return (digits);
}

static int
exponent(char *p0, int exp, int fmtch)
{
	register char *p, *t;
	char expbuf[MAXEXPLEN];
# ifdef _WANT_IO_C99_FORMATS
	int isa = fmtch == 'a' || fmtch == 'A';
# else
#  define isa 0
# endif

	p = p0;
	*p++ = isa ? 'p' - 'a' + fmtch : fmtch;
	if (exp < 0) {
		exp = -exp;
		*p++ = '-';
	}
	else
		*p++ = '+';
	t = expbuf + MAXEXPLEN;
	if (exp > 9) {
		do {
			*--t = to_char (exp % 10);
		} while ((exp /= 10) > 9);
		*--t = to_char (exp);
		for (; t < expbuf + MAXEXPLEN; *p++ = *t++);
	}
	else {
		if (!isa)
			*p++ = '0';
		*p++ = to_char (exp);
	}
	return (p - p0);
}
#endif /* FLOATING_POINT */







