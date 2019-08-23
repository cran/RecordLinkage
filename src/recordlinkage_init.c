#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .C calls */
extern void levenshtein(void *, void *, void *, void *, void *);
extern void makeBlockingPairs(void *, void *, void *, void *);
extern void mygllm(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void pho_h(void *, void *, void *);
extern void soundex(void *, void *, void *);

/* .Call calls */
extern SEXP jarowinklerCALL(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {".levenshtein_sym",       (DL_FUNC) &levenshtein,       5},
    {".makeBlockingPairs_sym", (DL_FUNC) &makeBlockingPairs, 4},
    {".mygllm_sym",            (DL_FUNC) &mygllm,            9},
    {".pho_h_sym",             (DL_FUNC) &pho_h,             3},
    {".soundex_sym",           (DL_FUNC) &soundex,           3},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {".jarowinklerCALL", (DL_FUNC) &jarowinklerCALL, 6},
    {NULL, NULL, 0}
};

void R_init_RecordLinkage(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
