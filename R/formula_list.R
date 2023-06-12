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
#' individual `formula`s.
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
      paste(c("List positions", which(not_fml), "in are not of class 'formula'."), 
            collapse = " "),
      call. = FALSE
    )
  
  class(.obj) <- "formula_list"

  return(.obj)
  
}