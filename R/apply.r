#' apply_fun
#'
#' from https://ro-che.info/articles/2017-08-18-call-r-function-from-c
#'
#' @name apply_fun
#' @export
#' @param f a function
#' @param x an argument
#' @useDynLib envestigate C_apply_fun
#'
apply_fun <- function(f, x) .Call(C_apply_fun, f, x, environment())


#' hash_apply2
#'
#'
#' @name hash_apply2
#' @export
#' @useDynLib envestigate C_hash_apply2
#'
hash_apply2 <- function(env,f){
  x <- pairlist(env,match.fun(f))
  .Call(C_hash_apply2,x)
}
