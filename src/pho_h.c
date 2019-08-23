/* pho_h.h
 * Berechnung eines phonetischen Codes für einen String.
 * 
 * nach dem Verfahren aus: Jörg Michael: "Doppelgänger gesucht - 
 * ein Programm für kontextsensitive phonetische Textumwandlung",
 * in c't 1999, Heft 25, S. 252-261.
 * 
 */     

#include <string.h>
#include <R.h>
#include "ph_ext.h"


void pho_h(char ** src, char ** dst, int * length)
{
  int l=*length;
  for (;l--;)  // count down from length-1 to 0
  {
    int str_len=strlen(src[l]);
    dst[l]=(char *) R_alloc(sizeof(char),str_len+1); // allocate dst string
    phonet (src[l], dst[l], str_len+1, 1);
  }
}
