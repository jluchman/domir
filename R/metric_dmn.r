#' @title Control function for \code{metric} argument of \code{domin}
#' 
#' @name metric_dmn
#' 
#' @description Supplies the fit statistic/metric producing function, 
#' function to filter the result of the fit metric producing function, and 
#' any additional arguments to the fit statistic/metric producing function.
#'
#' @param .fct Fit statistic/metric producing function; pre-defined or 
#' anonymous.
#' 
#' @param .flt_fct Filter function; pre-defined or anonymous.
#' 
#' @param ... Passes arguments to the fit statistic/metric producing function 
#' in `.fct`.
#'
#' @return Returns the matched call/itself as a list representing the 
#' elements of the unevaluated function call.
#' 
#' @details \code{domin} has three S3 methods that use either 
#' \link{`formula`}, \link{`Formula`}, or \link{`list`} objects
#' 
#' @export

metric_dmn <- function(.fct, .flt_fct, ...) { 
  
  # Argument checking ----
  if ((!is.character(.fct) && !is.function(.fct)) || 
      (is.character(.fct) && 
       is.null(tryCatch(match.fun(.fct), 
                         error = function(x) return(NULL))))) 
    stop("Argument to .fct in metric_dmn() is not a function.", call. = FALSE)
  
  if (!is.null(.flt_fct) && ((!is.character(.flt_fct) && !is.function(.flt_fct)) || 
      (is.character(.flt_fct) && 
       is.null(tryCatch(match.fun(.flt_fct), 
                        error = function(x) return(NULL)))))) 
    stop("Argument to .flt_fct in metric_dmn() is not a function.", call. = FALSE)
  
  return(as.list(match.call()))
  
}