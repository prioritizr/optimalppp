#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _optimalppp_rcpp_branch_matrix(SEXP);
extern SEXP _optimalppp_rcpp_miqp_formulation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_optimalppp_rcpp_branch_matrix",    (DL_FUNC) &_optimalppp_rcpp_branch_matrix,    1},
    {"_optimalppp_rcpp_miqp_formulation", (DL_FUNC) &_optimalppp_rcpp_miqp_formulation, 8},
    {NULL, NULL, 0}
};

void R_init_optimalppp(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
