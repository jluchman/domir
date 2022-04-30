#' @title Control function for \code{reg} argument of \code{domin}
#' 
#' @name reg_control
#' 
#' @description Supplies the model to be dominance analyzed, optional 
#' linking functions, and all arguments to the model.
#'
#' @param .fct Modeling function; pre-defined or anonymous
#' 
#' @param .lnk_fct Linking function; pre-defined or anonymous
#' 
#' @param ... Passes arguments to the function in \code{.fct}.
#'
#' @return boo
#' 
#' @details \code{domin} has three S3 methods that use either \link{\code{formula}}, \link{\code{Formula}}, or \link{\code{list}} objects
#' 
#' @export
#'
#' @examples
#' hi
#' 

reg_control <- function(.fct, .lnk_fct, ...) { 
  
  return(list(.fct = substitute(quote(.fct)), .lnk_fct = substitute(quote(.lnk_fct)), 
         .cll = "reg_control"))
  
  #return(list(.fct = .fct, .lnk_fct = NULL, .cll = as.list(match.call()), ...))
  
}