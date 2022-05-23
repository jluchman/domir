#' @title Control function for \code{model} argument of \code{domin}
#' 
#' @name model_dmn
#' 
#' @description Supplies the model to be dominance analyzed, optional 
#' linking functions, and all arguments to the model. 
#'
#' @param .fct Modeling function; pre-defined or anonymous.
#' 
#' @param .lnk_fct Linking function; pre-defined or anonymous.
#' 
#' @param ... Passes arguments to the modeling function in `.fct`.
#'
#' @return Returns the matched call/itself as a list representing the 
#' elements of the unevaluated function call.
#' 
#' @details \code{domin} has three S3 methods that use either 
#' \link{`formula`}, \link{`Formula`}, or \link{`list`} objects
#' 
#' @export

model_dmn <- function(.fct, .lnk_fct = NULL, ...) { 
  
  # Argument checking ----
  if ((!is.character(.fct) && !is.function(.fct)) || 
      (is.character(.fct) && 
       is.null(tryCatch(match.fun(.fct), 
                         error = function(x) return(NULL))))) 
    stop("Argument to .fct in model_dmn() is not a function.", call. = FALSE)
  
  if (!is.null(.lnk_fct) && ((!is.character(.lnk_fct) && !is.function(.lnk_fct)) || 
      (is.character(.lnk_fct) && 
       is.null(tryCatch(match.fun(.lnk_fct), 
                        error = function(x) return(NULL)))))) 
    stop("Argument to .lnk_fct in model_dmn() is not a function.", call. = FALSE)
  
  return(as.list(match.call()))
  
}