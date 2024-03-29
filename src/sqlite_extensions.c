/* Prototyp zum Laden von Erweiterungen in SQLite mittels Funktion 
   load_extension() 
   Informationen dazu: http://www.sqlite.org/cvstrac/wiki?p=LoadableExtensions
   In Datei DESCRIPTION des Pakets muss "LinkingTo: RSQLite" benutzt werden
   */

#include "sqlite3ext.h"
#include <string.h>
#include "R.h"


SQLITE_EXTENSION_INIT1

// remove comment to enable diagnostic messages
// #define DEBUG

#ifndef max
	#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
#endif
/* Prototypes of functions found in other source files */

double jarowinkler_core(const unsigned char *str_1, const unsigned char *str_2,
             double W_1, double W_2, double W_t,
             double r);

int levenshtein_internal(const unsigned char *s, const unsigned char *t,
                     int ins_c, int del_c, int sub_c);


/* wrapper for Jaro-Winkler string comparison */
void jarowinkler_wrapper(sqlite3_context *ctx, int n_values, sqlite3_value **value)
{
	// check for NULL values, return NULL if any of the input strings is NULL
	if(sqlite3_value_type(value[0]) == SQLITE_NULL || 
  	 sqlite3_value_type(value[1]) == SQLITE_NULL)
  {
		sqlite3_result_null(ctx);
		return;
	}
  const unsigned char *str1 = sqlite3_value_text(value[0]);
  const unsigned char *str2 = sqlite3_value_text(value[1]);
  double result;
  result = jarowinkler_core(str1, str2, 1.0/3, 1.0/3, 1.0/3, 0.5);
  // Be aware not to include anything else in the Debug-Mode
  #ifdef DEBUG
    Rprintf("String 1: %s\n", str1);
    Rprintf("String 2: %s\n", str2);
    Rprintf("Ergebnis des Stringvergleichs: %f\n", result);
  #endif
  sqlite3_result_double(ctx, result);
}

/* wrapper for Levenshtein string comparison */
void levenshtein_wrapper(sqlite3_context *ctx, int n_values, sqlite3_value **value)
{
	// check for NULL values, return NULL if any of the input strings is NULL
	if(sqlite3_value_type(value[0]) == SQLITE_NULL || 
  	 sqlite3_value_type(value[1]) == SQLITE_NULL)
  {
		sqlite3_result_null(ctx);
		return;
	}
  const unsigned char *str1 = sqlite3_value_text(value[0]);
  const unsigned char *str2 = sqlite3_value_text(value[1]);
	#ifdef DEBUG
  	Rprintf("String 1: %s\n", str1);
		Rprintf("String 2: %s\n", str2);
	#endif
  double result;
  int editDistance;
  editDistance = levenshtein_internal(str1, str2, 1, 1, 1);
  /* Only the string metric based on the edit distance is used in this
     package, therefore transform right here */
  //static_cast<const unsigned char*>(str2)
  result = 1.0 - (double) editDistance / (double) max(strlen((const char*)(str1)), strlen((const char*)(str2)));
	#ifdef DEBUG
		Rprintf("Ergebnis des Stringvergleichs: %f\n", result);
	#endif
  sqlite3_result_double(ctx, result);
}


/* SQLite invokes this routine once when it loads the extension.
** Create new functions, collating sequences, and virtual table
** modules here.  This is usually the only exported symbol in
** the shared library.
*/
int sqlite3_extension_init(
  sqlite3 *db_connection,
  char **pzErrMsg,
  const sqlite3_api_routines *pApi
){
  SQLITE_EXTENSION_INIT2(pApi)

   sqlite3_create_function(
      db_connection,
      "jarowinkler",
      2,
      SQLITE_UTF8,
      NULL,
      &jarowinkler_wrapper,
      NULL,
      NULL
      );
   
	 sqlite3_create_function(
      db_connection,
      "levenshtein",
      2,
      SQLITE_UTF8,
      NULL,
      &levenshtein_wrapper,
      NULL,
      NULL
      );
  return 0;
}
      
