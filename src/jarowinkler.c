#include <string.h>
#include <math.h>
#include <R.h>

/*
 * Implementierung der Jaro-Metrik
 * 
 * Eingabe:
 *  str_1, str_2    : die zu vergleichenden Zeichenketten
 *  W_1, W_2, W_t   : Gewichte f¸r Berechnung der ƒhnlichkeit
 *  r               : Radiuskoeffizient. Es werden gemeinsame Zeichen im
 *                    Radius r * max(length(str_1),length(str_2) betrachtet
 *  transpos_radius : gibt an, wie weit ein gemeinsame Zeichen voneinander
 *                    entfernt sein d¸rfen, ohne als Transposition zu z‰hlen                
 * Ausgabe:         ƒhnlichkeitsmaﬂ im Intervall [0, W_1 + W_2 + W_3]
 */

int getCommonCharacters(char * common, const char * str_1, 
                          const char * str_2, int radius);             
  
int getTranspositions(char * common_1, char * common_2, int radius);

double jaro(const char * str_1, const char * str_2, 
             double W_1, double W_2, double W_t,
             double r, int use_transpos_radius);

void jarowinkler(const char ** strvec_1, const char ** strvec_2,
             int * length_1, int * length_2,
             double * W_1, double * W_2, double * W_t,
             double * r, double * ans);

double jarowinkler_core(const char * str_1, const char * str_2,
             double W_1, double W_2, double W_t,
             double r);
						  

void jarowinkler(const char ** strvec_1, const char ** strvec_2,
             int * length_1, int * length_2,
             double * W_1, double * W_2, double * W_t,
             double * r, double * ans)
{
  //Rprintf("Einstieg in Funktion\n"); // Debug-Ausgabe
  int * use_transpos_radius=(int *) R_alloc(1,sizeof(int));
  *use_transpos_radius=0;
  int max_length= *length_1 > *length_2 ? *length_1 : *length_2;  
  for (int str_ind=0; str_ind < max_length; str_ind++)
  {
    const char * str_1=strvec_1[str_ind % *length_1];
    const char * str_2=strvec_2[str_ind % *length_2];
    ans[str_ind]=jarowinkler_core(str_1, str_2, *W_1, *W_2, *W_t, *r);
//     int str_len_1=strlen(str_1);
//     int str_len_2=strlen(str_2);
//   
//     /* Standard-Jaro-Score berechnen */
//     // Rprintf("Berechne Standard-Jaro-Gewicht\n"); // Debug-Ausgabe
//     double jaro_score=jaro(str_1, str_2, *W_1, *W_2, *W_t, *r, *use_transpos_radius);
//   
//     // Rprintf("Berechne daraus Jaro-Winkler\n"); // Debug-Ausgabe
//     /* wenn jaro() 1 oder 0 zur¸ckgibt, ist das der endg¸ltige Wert */
//     if (jaro_score==1.0 || jaro_score==0.0)
//       ans[str_ind]=jaro_score;
//       
//     /* else */
//     // Rprintf("Ermittle Anzahl der Zeichen, f¸r die die Stringanf‰nge ¸bereinstimmen\n"); // Debug-Ausgabe
//     /* Ermittle Anzahl der Zeichen, f¸r die die Stringanf‰nge ¸bereinstimmen */
//     int min_str_len=str_len_1<str_len_2 ? str_len_1 : str_len_2;
//     int max_i=0;  
//     while (str_1[max_i]==str_2[max_i] && max_i<4 && max_i<min_str_len)
//     {
//       max_i++;
//     }
//     ans[str_ind]=jaro_score + max_i * 0.1 * (1-jaro_score);
  }
} 


double jarowinkler_core(const char * str_1, const char * str_2,
             double W_1, double W_2, double W_t,
             double r)
{
  int str_len_1=strlen(str_1);
  int str_len_2=strlen(str_2);

  /* Standard-Jaro-Score berechnen */
  // Rprintf("Berechne Standard-Jaro-Gewicht\n"); // Debug-Ausgabe
  double jaro_score=jaro(str_1, str_2, W_1, W_2, W_t, r, 0);

  // Rprintf("Berechne daraus Jaro-Winkler\n"); // Debug-Ausgabe
  /* wenn jaro() 1 oder 0 zur¸ckgibt, ist das der endg¸ltige Wert */
  if (jaro_score==1.0 || jaro_score==0.0)
    return(jaro_score);
    
  /* else */
  // Rprintf("Ermittle Anzahl der Zeichen, f¸r die die Stringanf‰nge ¸bereinstimmen\n"); // Debug-Ausgabe
  /* Ermittle Anzahl der Zeichen, f¸r die die Stringanf‰nge ¸bereinstimmen */
  int min_str_len=str_len_1<str_len_2 ? str_len_1 : str_len_2;
  int max_i=0;  
  while (str_1[max_i]==str_2[max_i] && max_i<4 && max_i<min_str_len)
  {
    max_i++;
  }
  return(jaro_score + max_i * 0.1 * (1-jaro_score));

}

 
 double jaro(const char * str_1, const char * str_2, 
             double W_1, double W_2, double W_t,
             double r, int use_transpos_radius)
{
  int str_len_1=strlen(str_1);
  int str_len_2=strlen(str_2);

  /* wenn eine Zeichenkette leer ist, gib 0 zur¸ck */
  if (str_len_1==0 || str_len_2==0)
    return 0;

  int max_len=str_len_1>str_len_2 ? str_len_1 : str_len_2;

  /* Suchradius. radius==0 bedeutet, dass nur die gleiche Position betrachtet
   * wird, radius==k, dass Poitionen bis einschlieﬂlich Entfernung k betrachtet 
   * werden
   */   
  int radius;
  // Wenn beide Zeichenketten L‰nge 1 haben, setze Radius auf 0 
  if(max_len==1)
    radius=0;
  else

  radius=(int)((r * max_len - 1));
  if (radius<0)
    radius=0;
  
/* Gemeinsame Zeichen suchen */
    
  char * common_1=(char *)R_alloc(1,str_len_1+1);
  char * common_2=(char *)R_alloc(1,str_len_2+1);
  int ncommon;
  ncommon=getCommonCharacters(common_1, str_1, str_2, radius);
  /* Falls ncommon==0 ist die Ausgabe 0 */
  if (ncommon==0)
    return 0;
  // Die Anzahl der Zeichen muss nur einmal bestimmt werden
  getCommonCharacters(common_2, str_2, str_1, radius);
  
/* Anzahl der Transpositionen bestimmen */

double retVal;
if (!use_transpos_radius)
{
  int ntranspos;
  ntranspos=getTranspositions(common_1, common_2, 0);
  /*  double ntranspos;
    ntranspos=0.2 * getTranspositions(common_1, common_2, transpos_radius)
                    +0.8 * getTranspositions(common_1, common_2, 0);
   */ 
    /* Ausgabe: gewichtete Summe nach Jaro/Winkler */
    retVal=(double) W_1 * (ncommon/(double)str_len_1) 
                         + W_2 * (ncommon/(double)str_len_2)
                         + W_t * (ncommon-ntranspos)/(double)ncommon;

} else
{

  int ntranspos;
  int ntranspos_with_radius;
  ntranspos=getTranspositions(common_1, common_2, 0);
  ntranspos_with_radius=getTranspositions(common_1, common_2, 1);
  int min_str_len=str_len_1<str_len_2 ? str_len_1 : str_len_2;
  double W_r= ((double) ntranspos - ntranspos_with_radius) / min_str_len;  
  retVal=(double) W_1 * (ncommon/(double)str_len_1) 
                        + W_2 * (ncommon/(double)str_len_2)
                        + W_t * (1-W_r) * (ncommon-ntranspos)/(double)ncommon;
  
  


}
  //free(common_1);
  //free(common_2);
  return retVal;

} 


/*
 * Eingabe:
 *  str_1, str_2  : Zeichenketten
 *  radius        : Radius, bis einschlieﬂlich dem gesucht wird
 *    
 * Ausgabe:
 *  common        : gemeinsame Zeichen
 *   
 * R¸ckgabewert:  : Anzahl der Gemeinsamen Zeichen 
 * 
 * unter common muss Speicher von mindestes length(str_1) Grˆﬂe zugewiesen sein
 */      
int getCommonCharacters(char * common, const char * str_1, 
                        const char * str_2, int radius)
{
  /* Im zweiten String werden Zeichen als gelˆscht markiert,
   * kopiere deshalb um
   */   
  int str_len_1=strlen(str_1);
  int str_len_2=strlen(str_2);
  /* speichert die aktuelle Position in common */
  int common_pos=0;
  char * str_2_temp=(char *)R_alloc(1,str_len_2+1);
  strcpy(str_2_temp, str_2);
  
  // Z‰hlvariablen
  int cur_pos; /* Zeichenposition in str_1 */
  int search_pos; /* Suchposition in str_1 */


  for (cur_pos=0; cur_pos<str_len_1; cur_pos++)
  {
    // Position, ab der nach gemeinsamem Zeichen gesucht wird
    int search_start_pos=cur_pos-radius>0 ? cur_pos - radius : 0;
    int search_end_pos=cur_pos + radius + 1<str_len_2 ? 
                       cur_pos + radius + 1: str_len_2;
    for (search_pos=search_start_pos; search_pos<search_end_pos; search_pos++)
    {
      if (str_1[cur_pos]==str_2_temp[search_pos])
      {
        common[common_pos]=str_1[cur_pos];
        str_2_temp[search_pos]='\0';
        common_pos++;
        break;
      }
    }
  }
  // String mit gemeinsamen Zeichen terminieren
  common[common_pos]='\0';
  // Zwischenspeicher freigeben
  //free(str_2_temp);
  return (common_pos);
}             


int getTranspositions(char * common_1, char * common_2, int radius)
{
  int cur_pos;
  int search_pos;
  int common_len_1=strlen(common_1);
  int common_len_2=strlen(common_2);
  int ntranspositions=0;

  char * common_2_temp=(char *)R_alloc(1,common_len_2+1);
  strcpy(common_2_temp, common_2);
  
  //double transpos_distance=0;
  
  for (cur_pos=0; cur_pos<common_len_1; cur_pos++)
  {
    int istransposition=1;
    int search_start_pos=cur_pos - radius>0 ? cur_pos - radius : 0;
    int search_end_pos=cur_pos + radius + 1<common_len_2 ? 
                    cur_pos + radius + 1: common_len_2;
    for (search_pos=search_start_pos; search_pos<search_end_pos; search_pos++)
    {
      if (common_1[cur_pos]==common_2_temp[search_pos])
      {
        common_2_temp[search_pos]='\0';
/*        transpos_distance=abs(cur_pos-search_pos); */
        istransposition=0;
        break;
      }
    }
    if (istransposition)
        ntranspositions++;      
  }
//  free(common_2_temp);
  return ntranspositions/2;
  //ntranspositions/=2;
//  printf("%f\n", transpos_distance);
//  return floor(ntranspositions / 2.0 + transpos_distance);
}  
