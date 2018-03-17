#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* ################################################################################
 * BEGIN HELPER FUNCTIONS
################################################################################ */

#define NONEMPTY_(_FRAME_) \
    CHAR(PRINTNAME(TAG(_FRAME_)))[0] != '.' && CAR(_FRAME_) != R_UnboundValue

static int FrameSize(SEXP frame, int all)
{
    int count = 0;
    if (all) {
	while (frame != R_NilValue) {
	    count += 1;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame))
		count += 1;
	    frame = CDR(frame);
	}
    }
    return count;
}


#define CHECK_HASH_TABLE(table) do {		\
	if (TYPEOF(table) != VECSXP)		\
	    error("bad hash table contents");	\
    } while (0)

static int HashTableSize(SEXP table, int all)
{
    CHECK_HASH_TABLE(table);
    int count = 0;
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	count += FrameSize(VECTOR_ELT(table, i), all);
    return count;
}

static void FrameValues(SEXP frame, int all, SEXP values, int *indx)
{
    if (all) {
	while (frame != R_NilValue) {
#         define DO_FrameValues						\
	    SEXP value = CAR(frame);					\
	    if (TYPEOF(value) == PROMSXP) {				\
		PROTECT(value);						\
		value = eval(value, R_GlobalEnv);			\
		UNPROTECT(1);						\
	    }								\
	    SET_VECTOR_ELT(values, *indx, lazy_duplicate(value));	\
	    (*indx)++

	    DO_FrameValues;
	    frame = CDR(frame);
	}
    } else {
	while (frame != R_NilValue) {
	    if (NONEMPTY_(frame)) {
		DO_FrameValues;
	    }
	    frame = CDR(frame);
	}
    }
}
#undef DO_FrameValues

static void HashTableValues(SEXP table, int all, SEXP values, int *indx)
{
    CHECK_HASH_TABLE(table);
    int n = length(table);
    int i;
    for (i = 0; i < n; i++)
	FrameValues(VECTOR_ELT(table, i), all, values, indx);
}

/* ################################################################################
 * END HELPER FUNCTIONS
################################################################################ */


// /*
//  * apply a function to all objects in an environment and return the
//  * results in a list.
//  * Equivalent to lapply(as.list(env, all.names=all.names), FUN, ...)
//  */
// /* This is a special .Internal */
// SEXP do_eapply(SEXP call, SEXP op, SEXP args, SEXP rho)
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


/* ################################################################################
 * not working...clues from https://github.com/kevinushey/Kmisc/blob/3adfcb90ed7045f9b149cf7e825049b0773a7aa5/src/enumerate.c
################################################################################ */

// SEXP hash_apply(SEXP call, SEXP env, SEXP rho){
//
//   // we don't need the first symbol -- that's just the function 'enumerate'
//   SEXP args = CDR(call);
//
//   // similarly, we need to tell R that the symbol it wants to look up is FUN
//   // otherwise it can fail other symbol lookup
//   SEXP funSym = install("FUN"); args = CDR(args);
//
//   /* size of hash table */
//   int k = HashTableSize(HASHTAB(env), 0); // maybe replace with XX or envSym (not sure)
//   printf("k: %i\n",k);
//
//   /* elements of hash table to map f(...) over */
//   SEXP tmp;
//   PROTECT(tmp = allocVector(VECSXP, k));
//   int k2 = 0;
//   HashTableValues(HASHTAB(env), 0, tmp, &k2);
//
//   printf("k2: %i\n",k2);
//
//   SEXP R_fcall;
//   // R_fcall = PROTECT(LCONS(funSym,LCONS(R_DotsSymbol,R_NilValue)));
//
//   SEXP ind;
//   PROTECT(ind = allocVector(INTSXP, 1));
//
//   SEXP Xsym = install("X");
//   SEXP isym = install("i");
//   /* tmp :=  `[`(<elist>, i) */
//   PROTECT(tmp = LCONS(R_Bracket2Symbol,LCONS(Xsym, LCONS(isym, R_NilValue))));
//   /* fcall :=  <FUN>( tmp, ... ) */
//   PROTECT(R_fcall = LCONS(funSym, LCONS(tmp, LCONS(R_DotsSymbol, R_NilValue))));
//
//   defineVar(Xsym, tmp, rho);
//   INCREMENT_NAMED(tmp);
//   defineVar(isym, ind, rho);
//   INCREMENT_NAMED(ind);
//
//   for(int i = 0; i < k; i++) {
//     printf("i: %i\n",i);
//     R_forceAndCall(R_fcall, 1, rho);
//   }
//
//   UNPROTECT(4);
//   return R_NilValue;
// };
