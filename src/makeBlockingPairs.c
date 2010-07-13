/* ========================================================================== */
/*                                                                            */
/*   makeBlockingPairs.c                                                      */
/*   (c) 2010 Andreas Borg                                                    */
/*                                                                            */
/*   Makes record pairs from a list of vectors of record ids, each vector     */
/*   containing all records with identical blocking variable                  */
/* ========================================================================== */

#include "R.h"
#include "Rinternals.h"

/** Create unordered pairs from each block of record pairs.
 * For each block of record ids in blockList, the set of unordered pairs 
 *  is generated (e.g. 3 combinations for 3 elements). Backend function to 
 *  R function makeBlockingPairs.
 * 
 * \param blockList A list of Integer vectors representing ids of records.
 * \param listLength Number of elements in blockList.
 * \param pairs Array with nPairs rows and 2 columns to hold the generated pairs.
 * \param nPairs Number of pairs in the result (precalculated in R interface).    
 */ 
void makeBlockingPairs(SEXP * blockList, int * listLength, int * pairs, int * nPairs)
{
 SEXP * listEl; /* iterates blocks in list */
 int * id1; /* iterates first record id of each pair */ 
 int * id2; /* iterates second record id of each pair */
 int * pair=pairs; /* points to first id of next pair in output*/
 int blockLength; /* length of individual block in list */
 /* Iterate over blocks (=elements in blockList) */
 for (listEl=blockList; listEl < blockList + *listLength; listEl++)
 {
  blockLength=LENGTH(*listEl); /* length of this block */
  /* Iterate from first to second to last record id */
  for (id1=INTEGER(*listEl); id1<INTEGER(*listEl)+blockLength-1; id1++)
  {
  /* Iterate from current first id to last record id */
   for (id2=id1+1; id2<INTEGER(*listEl)+blockLength; id2++)
   {
//     Rprintf("Aktuelles Paar: %d, %d\n", *id1, *id2); // Debug output
    *pair=*id1; /* assign value of id1 to pair matrix */
    *(pair + *nPairs)=*id2; /* assign value of id2 to second column of pair matrix */
    pair++; /* advance to next output pair */    
   }
  }
 }

}
