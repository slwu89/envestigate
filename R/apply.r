
# #' hash_apply
# #'
# #'
# #' X: environment, FUN function, ...
# #' @name hash_apply
# #' @export
# #' @useDynLib envestigate C_hash_apply
# #'
# hash_apply <- function(X,FUN,...){
#   call <- match.call(expand.dots = FALSE)
#   .Call(C_hash_apply,call,X,environment())
# }


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

#' eapply3
#'
#' eapply3(X = xx,FUN = function(x=2,y=3,...){print(x);print(y);nest(...)},z=1)
#'
#' X: environment, FUN function, ...
#' @name eapply3
#' @export
#' @useDynLib envestigate C_eapply3
#'
eapply3 <- function(X,FUN,...){
  call <- match.call(expand.dots = FALSE)
  .Call(C_eapply3,call,environment())
}

#' call_function
#' @name call_function
#' @export
#' @useDynLib envestigate C_call_function
#'
call_function <- function(FUN,...){
  call <- match.call(expand.dots = FALSE)
  .Call(C_call_function,call,environment())
}
