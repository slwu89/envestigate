#include <Rinternals.h>
#include <R_ext/Rdynload.h>


SEXP is_hashed(SEXP env){
  return ScalarLogical((HASHTAB(env)!=R_NilValue)? TRUE: FALSE);
}

SEXP hash_table(SEXP env){
  SEXP ret, ans = R_NilValue, frame;
  SEXP ht = HASHTAB(env);
  int n, i, j;

  if (ht==R_NilValue) return ans;

  n = length(ht);

  PROTECT(ret = allocList(n));
  ans = ret;

  for (i = 0; i < n; i++){
    printf("i: %i \n",i); /* where are we? */
    frame = VECTOR_ELT(ht,i);
    j = 0;
    /* Count number of elements */
    while (frame != R_NilValue) {
      printf("i: %i, j: %i \n",i,j); /* where are we? */
      j++;
      frame = CDR(frame);
    }
    if (j == 0){
      printf("j == 0 \n");
      SETCAR(ans,R_NilValue);
    } else {
      printf("j != 0 \n");
      SETCAR(ans,allocVector(STRSXP,j));
      frame = VECTOR_ELT(ht,i);
      j = 0;
      while (frame != R_NilValue) {
        printf("i: %i, j: %i \n",i,j); /* where are we? */
        SET_STRING_ELT(CAR(ans),j,PRINTNAME(TAG(frame)));
        j++;
        frame = CDR(frame);
      }
    }
    ans = CDR(ans);
  }

  UNPROTECT(1);
  return ret;
}


SEXP R_apply_fun(SEXP f, SEXP x, SEXP rho) {
  SEXP call = PROTECT(LCONS(f, LCONS(x, R_NilValue)));
  SEXP val = R_forceAndCall(call, 1, rho);
  UNPROTECT(1);
  return val;
}



// SEXP hash_table_apply(SEXP env, SEXP fun){
//   SEXP R_fcall;
//   SEXP  frame;
//   SEXP ht = HASHTAB(env);
//   int n, i, j;
//
//   if (ht==R_NilValue) return R_NilValue;
//
//   n = length(ht);
//
//   for (i = 0; i < n; i++){
//     frame = VECTOR_ELT(ht,i);
//     if(frame != R_NilValue){
//
//     }
//   }
//
//   return R_NilValue;
// }
//
//
// SEXP lapply2(SEXP list, SEXP fn, SEXP rho)
// {
//     int n = length(list);
//     SEXP R_fcall, ans;
//
//     if(!isNewList(list)) error("'list' must be a list");
//     if(!isFunction(fn)) error("'fn' must be a function");
//     if(!isEnvironment(rho)) error("'rho' should be an environment");
//     R_fcall = PROTECT(lang2(fn, R_NilValue));
//     ans = PROTECT(allocVector(VECSXP, n));
//     for(int i = 0; i < n; i++) {
//         SETCADR(R_fcall, VECTOR_ELT(list, i));
//         SET_VECTOR_ELT(ans, i, eval(R_fcall, rho));
//     }
//     setAttrib(ans, R_NamesSymbol, getAttrib(list, R_NamesSymbol));
//     UNPROTECT(2);
//     return ans;
// }
//
// /*
//  * apply a function to all objects in an environment and return the
//  * results in a list.
//  * Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)
//  */
// /* This is a special .Internal */
// SEXP attribute_hidden do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
// {
//     SEXP env, ans, R_fcall, FUN, tmp, tmp2, ind;
//     int i, k, k2;
//     int /* boolean */ all, useNms;
//
//     checkArity(op, args);
//
//     PROTECT(env = eval(CAR(args), rho));
//     if (ISNULL(env))
// 	error(_("use of NULL environment is defunct"));
//     if( !isEnvironment(env) )
// 	error(_("argument must be an environment"));
//
//     FUN = CADR(args);
//     if (!isSymbol(FUN))
// 	error(_("arguments must be symbolic"));
//
//     /* 'all.names' : */
//     all = asLogical(eval(CADDR(args), rho));
//     if (all == NA_LOGICAL) all = 0;
//
//     /* 'USE.NAMES' : */
//     useNms = asLogical(eval(CADDDR(args), rho));
//     if (useNms == NA_LOGICAL) useNms = 0;
//
//     if (env == R_BaseEnv || env == R_BaseNamespace)
// 	k = BuiltinSize(all, 0);
//     else if (HASHTAB(env) != R_NilValue)
// 	k = HashTableSize(HASHTAB(env), all);
//     else
// 	k = FrameSize(FRAME(env), all);
//
//     PROTECT(ans  = allocVector(VECSXP, k));
//     PROTECT(tmp2 = allocVector(VECSXP, k));
//
//     k2 = 0;
//     if (env == R_BaseEnv || env == R_BaseNamespace)
// 	BuiltinValues(all, 0, tmp2, &k2);
//     else if (HASHTAB(env) != R_NilValue)
// 	HashTableValues(HASHTAB(env), all, tmp2, &k2);
//     else
// 	FrameValues(FRAME(env), all, tmp2, &k2);
//
//     SEXP Xsym = install("X");
//     SEXP isym = install("i");
//     PROTECT(ind = allocVector(INTSXP, 1));
//     /* tmp :=  `[`(<elist>, i) */
//     PROTECT(tmp = LCONS(R_Bracket2Symbol,
// 			LCONS(Xsym, LCONS(isym, R_NilValue))));
//     /* fcall :=  <FUN>( tmp, ... ) */
//     PROTECT(R_fcall = LCONS(FUN, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));
//
//     defineVar(Xsym, tmp2, rho);
//     INCREMENT_NAMED(tmp2);
//     defineVar(isym, ind, rho);
//     INCREMENT_NAMED(ind);
//
//     for(i = 0; i < k2; i++) {
// 	INTEGER(ind)[0] = i+1;
// 	SEXP tmp = R_forceAndCall(R_fcall, 1, rho);
// 	if (MAYBE_REFERENCED(tmp))
// 	    tmp = lazy_duplicate(tmp);
// 	SET_VECTOR_ELT(ans, i, tmp);
//     }
//
//     if (useNms) {
// 	SEXP names;
// 	PROTECT(names = allocVector(STRSXP, k));
// 	k = 0;
// 	if (env == R_BaseEnv || env == R_BaseNamespace)
// 	    BuiltinNames(all, 0, names, &k);
// 	else if(HASHTAB(env) != R_NilValue)
// 	    HashTableNames(HASHTAB(env), all, names, &k);
// 	else
// 	    FrameNames(FRAME(env), all, names, &k);
//
// 	setAttrib(ans, R_NamesSymbol, names);
// 	UNPROTECT(1);
//     }
//     UNPROTECT(6);
//     return(ans);
// }


/* Lifted from R sources in src/main/envir.c*/
static int hash_function(const char *s)
{
  char *p;
  unsigned h = 0, g;
  for (p = (char *) s; *p; p++) {
    h = (h << 4) + (*p);
    if ((g = h & 0xf0000000) != 0) {
      h = h ^ (g >> 24);
      h = h ^ g;
    }
  }
  return h;
}

SEXP hash_value(SEXP str){
  return ScalarInteger(hash_function(CHAR(STRING_ELT(str,0))));
}

static R_CallMethodDef callMethods[]  = {
  {"C_is_hashed", (DL_FUNC)&is_hashed, 1},
  {"C_hash_table", (DL_FUNC)&hash_table, 1},
  {"C_hash_value", (DL_FUNC)&hash_value, 1},
  {"C_apply_fun", (DL_FUNC)&R_apply_fun, 3},
  {NULL, NULL, 0}
};

void R_init_envestigate(DllInfo *info){
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
