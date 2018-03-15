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
