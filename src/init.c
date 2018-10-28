#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _optimalppp_rcpp_branch_matrix(SEXP);
extern SEXP _optimalppp_rcpp_heuristic_solution(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _optimalppp_rcpp_mip_formulation(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _optimalppp_rcpp_ppp_objective(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_optimalppp_rcpp_branch_matrix",      (DL_FUNC) &_optimalppp_rcpp_branch_matrix,      1},
    {"_optimalppp_rcpp_heuristic_solution", (DL_FUNC) &_optimalppp_rcpp_heuristic_solution, 7},
    {"_optimalppp_rcpp_mip_formulation",    (DL_FUNC) &_optimalppp_rcpp_mip_formulation,    8},
    {"_optimalppp_rcpp_ppp_objective",      (DL_FUNC) &_optimalppp_rcpp_ppp_objective,      4},
    {NULL, NULL, 0}
};

void R_init_optimalppp(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
