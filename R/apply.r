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


#' hash_apply
#'
#'
#' X: environment, FUN function, ...
#' @name hash_apply
#' @export
#' @useDynLib envestigate C_hash_apply
#'
hash_apply <- function(X,FUN,...){
  call <- match.call(expand.dots = FALSE)
  .Call(C_hash_apply,call,X,environment())
}


#' eapply2
#'
#'
#' X: environment, FUN function, ...
#' @name eapply2
#' @export
#' @useDynLib envestigate C_eapply2
#'
eapply2 <- function(X,FUN){
  .Call(C_eapply2,X,FUN,environment())
}
