/* Soundex algorithm for R package "RecordLinkage"
 * Taken from PostgreSQL 8.3.6, file contrib/fuzzystrmatch.c with
 * slight modifications.  
 * Original copyright notice below.
 */


/*
 * fuzzystrmatch.h
 *
 * Functions for "fuzzy" comparison of strings
 *
 * Joe Conway <mail@joeconway.com>
 *
 * $PostgreSQL: pgsql/contrib/fuzzystrmatch/fuzzystrmatch.h,v 1.16 2008/01/01 19:45:45 momjian Exp $
 * Copyright (c) 2001-2008, PostgreSQL Global Development Group
 * ALL RIGHTS RESERVED;
 *
 * levenshtein()
 * -------------
 * Written based on a description of the algorithm by Michael Gilleland
 * found at http://www.merriampark.com/ld.htm
 * Also looked at levenshtein.c in the PHP 4.0.6 distribution for
 * inspiration.
 *
 * metaphone()
 * -----------
 * Modified for PostgreSQL by Joe Conway.
 * Based on CPAN's "Text-Metaphone-1.96" by Michael G Schwern <schwern@pobox.com>
 * Code slightly modified for use as PostgreSQL function (palloc, elog, etc).
 * Metaphone was originally created by Lawrence Philips and presented in article
 * in "Computer Language" December 1990 issue.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without a written agreement
 * is hereby granted, provided that the above copyright notice and this
 * paragraph and the following two paragraphs appear in all copies.
 *
 * IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING
 * LOST PROFITS, ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
 * DOCUMENTATION, EVEN IF THE AUTHOR OR DISTRIBUTORS HAVE BEEN ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE AUTHOR AND DISTRIBUTORS HAS NO OBLIGATIONS TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 */


#include <ctype.h>
#include <R.h>



/*
 * Soundex
 */
void soundex(char **src, char **dst, int * length);
void soundex_single(char *instr, char *outstr);

#define SOUNDEX_LEN 4


/*									ABCDEFGHIJKLMNOPQRSTUVWXYZ */
char soundex_table[] = "01230120022455012623010202";

#define soundex_code(letter) soundex_table[toupper((unsigned char) (letter)) - 'A']

