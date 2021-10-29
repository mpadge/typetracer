#define _GNU_SOURCE
#define USE_RINTERNALS

#include <R.h>
#include <R_ext/Error.h>
#include <R_ext/Rdynload.h>
#include <Rdefines.h>
#include <stdio.h>

SEXP reassign_function_body(SEXP fun, SEXP body) {
  if (TYPEOF(fun) != CLOSXP) Rf_error("fun must be a function");

  switch (TYPEOF(body)) {
  case PROMSXP:
  case DOTSXP:
  case ANYSXP:
  case BCODESXP:
  case WEAKREFSXP:
  case NEWSXP:
  case FREESXP:
    Rf_error("body type %d is not supported", TYPEOF(body));
  }

  SET_BODY(fun, body);

  return R_NilValue;
}

extern SEXP reassign_function_body(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"reassign_function_body_", (DL_FUNC)&reassign_function_body, 2},
    {NULL, NULL, 0}
};

void R_init_injectr(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
