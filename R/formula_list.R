#' @title A [`list`] composed of `formulas`
#'
#' @name formula_list
#' 
#' @description
#' Structures `formula` inputs for `domir` to obtain RHS-LHS pairs.
#' 
#' @param ... `formula`s, possibly named
#' 
#' @details  
#' All `formula_list`s enforces requirements that the list are composed of 
#' individual `formula`s and that each formula is unique with its own, 
#' different, non-`NULL` dependent variable/response.
#' 
#' @return A `list` of class `formula_list`.
#' 
#' @rdname formula_list
#' 
#' @export
formula_list <- function(...) {
  
  .obj <- list(...)
  
  # Check all list elements are formulas
  not_fml <- 
    sapply(.obj, Negate(inherits), what = "formula")
 
  if ( any(not_fml) )
    stop(
      paste(c("List element", which(not_fml), "not of class 'formula'."), 
            collapse = " "),
      call. = TRUE
    )
  
  # Check all elements are unique formulas
  DV_list <-
    sapply(.obj, 
           function(elem) {
             if (attr(stats::terms(elem), "response")==1)
               as.list(attr(stats::terms(elem), "variables"))[[
                 attr(stats::terms(elem), "response")+1
               ]] 
             else NA
           } 
    )
  
  DV_problem <- 
    ( is.na(DV_list) | duplicated(DV_list, incomparables = c(NA)) ) 
  
  if ( any( DV_problem ) ) 
    stop(
      paste(c("List element", which(DV_problem), 
              "missing a response or duplicated a response."), 
            collapse = " "),
      call. = TRUE
    )
  
  class(.obj) <- c("formula_list", "list")

  return(.obj)
  
}

#' @title Translate `formula_list` into `Formula::Formula`
#'
#' @name fmllst2Fml
#' 
#' @description
#' Translates `{domir}`s `formula_list` objects into an a [`Formula::Formula`]
#' 
#' @param fmllst A `formula_list` classed object.
#' 
#' @return A `Formula::Formula` object.
#' 
#' @rdname fmllst2Fml
#' 
#' @export
fmllst2Fml <- function(fmllst) {
  if (!inherits(fmllst, "formula_list")) 
    stop("Object is not of class 'formula_list'.", call. = FALSE)
  
  if (!require("Formula")) 
    stop("Package '{Formula}' not available.", call. = FALSE)
  
  list_parsed <- 
    lapply(fmllst, domir:::formula_parse)
  
  Fml <- 
    paste(
      sapply(
        list_parsed,
        function(elem) {
          return(elem$LHS_names)
        }
      ),
      collapse = "|"
    )
  
  Fml <- 
    paste(
      c(Fml, " ~ ", 
        paste(
          sapply(
            list_parsed,
            function(elem) {
              rhs_sum <- 
                paste(elem$RHS_names, collapse = "+")
              return( rhs_sum )
            }
          ),
          collapse = "|"
        )
      ),
      collapse = ""
    )
  
  return(
    Formula::as.Formula(Fml)
  )
  
}