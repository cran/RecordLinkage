#include<string.h>
#include<stdio.h>
#include <R.h>

int levenshtein_internal(const char *s, const char *t,
                     int ins_c, int del_c, int sub_c);

void levenshtein(char ** strvec_1, char ** strvec_2,
             int * length_1, int * length_2, int * ans);

/* Interface for GNU implementation below
 * 
 * Arguments:
 * 	strvec_1, strvec_2		The character vectors to compare
 * 	length_1, length_2		Length of strvec_1 and strvec_2
 * 	ans										return vector, must hold max(length_1, length_2)
 * 												double numbers     
 */
void levenshtein(char ** strvec_1, char ** strvec_2,
             int * length_1, int * length_2, int * ans)
{
  int max_length= *length_1 > *length_2 ? *length_1 : *length_2;  
  int str_ind;
	for (str_ind=0; str_ind < max_length; str_ind++)
  {
    char * str_1=strvec_1[str_ind % *length_1];
    char * str_2=strvec_2[str_ind % *length_2];
   	int lev_dist=levenshtein_internal(str_1, str_2, 1, 1, 1);
    ans[str_ind]=lev_dist;
// 		Rprintf("Vergleiche %s, %s\n",str_1, str_2); // Debug-Ausgabe
// 		Rprintf("Levenshtein-Distanz: %d\n",lev_dist);   
// 		Rprintf("Levenshtein-Distanz: %d\n",ans[str_ind]);   
  }
} 
// 
// int main(int argc, char ** argv)
// {
// 	if (argc==3)
// 	{
// 		printf("%d\n",calc_levdist(argv[1],argv[2]));
// 		return(0);
// 	}
// 	else
// 	{
// 		printf("Usage: levenshtein STRING_1 STRING_2");
// 		return(1);
// 	}
// }


/*
 * Below follows extract of PostgreSQL modul fuzzystrmatch. Only the relevant
 * function levenshtein_internal is retained here. Some changes were made
 * to fit the needs of an R function (R_alloc is used, PostgreSQL 
 * error handling has been removed, static keyword is removed from signature).   
 */
 
/* Definition fom PostgreSQL sources */ 

#define Min(x, y)       ((x) < (y) ? (x) : (y))


/*
 * fuzzystrmatch.c
 *
 * Functions for "fuzzy" comparison of strings
 *
 * Joe Conway <mail@joeconway.com>
 *
 * $PostgreSQL: pgsql/contrib/fuzzystrmatch/fuzzystrmatch.c,v 1.32 2010/01/02 16:57:32 momjian Exp $
 * Copyright (c) 2001-2010, PostgreSQL Global Development Group
 * ALL RIGHTS RESERVED;
 *
 * levenshtein()
 * -------------
 * Written based on a description of the algorithm by Michael Gilleland
 * found at http://www.merriampark.com/ld.htm
 * Also looked at levenshtein.c in the PHP 4.0.6 distribution for
 * inspiration.
 * Configurable penalty costs extension is introduced by Volkan
 * YAZICI <volkan.yazici@gmail.com>.
 *
 * (comments on metaphone omitted)
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

/*
 * levenshtein_internal - Calculates Levenshtein distance metric
 *                        between supplied strings. Generally
 *                        (1, 1, 1) penalty costs suffices common
 *                        cases, but your mileage may vary.
 */

int levenshtein_internal(const char *s, const char *t,
                     int ins_c, int del_c, int sub_c)
{
    int         m,
                n;
    int        *prev;
    int        *curr;
    int         i,
                j;
    const char *x;
    const char *y;

    m = strlen(s);
    n = strlen(t);

    /*
     * We can transform an empty s into t with n insertions, or a non-empty t
     * into an empty s with m deletions.
     */
    if (!m)
        return n * ins_c;
    if (!n)
        return m * del_c;

    
    /* One more cell for initialization column and row. */
    ++m;
    ++n;

    /*
     * Instead of building an (m+1)x(n+1) array, we'll use two different
     * arrays of size m+1 for storing accumulated values. At each step one
     * represents the "previous" row and one is the "current" row of the
     * notional large array.
     */
//    prev = (int *) palloc(2 * m * sizeof(int));
    prev = (int *) R_alloc(sizeof(int), 2 * m);
    curr = prev + m;

    /* Initialize the "previous" row to 0..cols */
    for (i = 0; i < m; i++)
        prev[i] = i * del_c;

    /* Loop through rows of the notional array */
    for (y = t, j = 1; j < n; y++, j++)
    {
        int        *temp;

        /*
         * First cell must increment sequentially, as we're on the j'th row of
         * the (m+1)x(n+1) array.
         */
        curr[0] = j * ins_c;

        for (x = s, i = 1; i < m; x++, i++)
        {
            int         ins;
            int         del;
            int         sub;

            /* Calculate costs for probable operations. */
            ins = prev[i] + ins_c;      /* Insertion    */
            del = curr[i - 1] + del_c;  /* Deletion     */
            sub = prev[i - 1] + ((*x == *y) ? 0 : sub_c);       /* Substitution */

            /* Take the one with minimum cost. */
            curr[i] = Min(ins, del);
            curr[i] = Min(curr[i], sub);
        }

        /* Swap current row with previous row. */
        temp = curr;
        curr = prev;
        prev = temp;
    }

    /*
     * Because the final value was swapped from the previous row to the
     * current row, that's where we'll find it.
     */
    return prev[m - 1];

}
