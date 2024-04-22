#' @title A [`list`] composed of `formulas`
#'
#' @name formula_list
#'
#' @description
#' Defines a list object composed of `formula`s. The purpose of this 
#' class of object is to impose structure of the list to ensure that it 
#' can be used to obtain RHS-LHS pairs and will be able to be 
#' parsed in [`domir`].
#'
#' @param ... `formula`s, possibly named
#'
#' @details
#' The `formula_list` requires that each element of the list is a `formula` 
#' and that each `formula` is unique with a different, non-`NULL` 
#' dependent variable/response.
#'
#' @return A `list` of class `formula_list`.
#'
#' @rdname formula_list
#'
#' @export
formula_list <- function(...) {
  .obj <- list(...)
  # Check all list elements are formulas
  not_fml <- sapply(.obj, Negate(inherits), what = "formula")
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
             tryCatch(
               ( function() { if (attr(stats::terms(elem), "response")==1)
                 as.list(attr(stats::terms(elem), "variables"))[[
                   attr(stats::terms(elem), "response")+1
                 ]]
                 else NA
               })(),
               error = function(err) {
                 # avoids cryptic error
                 stop(deparse(elem), " is an invalid formula.", call. = FALSE
                 )
               }
             )
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
#' Translates [`formula_list`] objects into a [`Formula::Formula`]
#'
#' @param fmllst A `formula_list` classed object.
#'
#' @param drop_lhs An integer vector.
#'
#' Used as a selection vector to remove left hand side names prior to
#' generating the `Formula` object. This vector must be composed of
#' integers (e.g., 1L and not 1).
#'
#' This is useful for some `Formulas` that do not have a separate
#' LHS for each LHS model part (e.g., [`pscl::zeroinfl`]) but are required
#' to have separte LHS parts by `formula_list`.
#'
#' @return A `Formula::Formula` object.
#'
#' @rdname fmllst2Fml
#'
#' @export
fmllst2Fml <- function(fmllst, drop_lhs = NULL) {
  if (!inherits(fmllst, "formula_list"))
    stop("Submitted object is not of class 'formula_list'.", call. = FALSE)
 
  if (!requireNamespace("Formula"))
    stop("Package '{Formula}' not available.", call. = FALSE)
 
  list_parsed <-
    lapply(fmllst, domir::formula_parse)
 
  if (!is.null(drop_lhs)) {
    if (!is.atomic(drop_lhs) || !is.integer(drop_lhs) || !length(drop_lhs)) {
      stop(paste(
        "'drop_lhs' is not a integer vector.",
        "'drop_lhs' is a(n)", class(drop_lhs), "of mode", mode(drop_lhs),
        "with a length of", length(drop_lhs), "."
      ),
      call. = FALSE)
    }
   
    if ( !all(drop_lhs %in% 1:length(list_parsed)) ) {
      bad_drop_lhs <-
        drop_lhs[which(!(drop_lhs %in% 1:length(list_parsed)))]
      stop(paste("Values",
                 paste(bad_drop_lhs, collapse = " "),
                 "in 'drop_lhs' are not valid element positions."),
           call. = FALSE)
    }
   
    keep_lhs <- which(!(1:length(list_parsed) %in% drop_lhs))
  } else {
    keep_lhs <- 1:length(list_parsed)
  }
 
  Fml <-
    paste(
      sapply(
        list_parsed[keep_lhs],
        function(elem) {
          elem$lhs_names
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
                paste(elem$rhs_names, collapse = "+")
              if (rhs_sum == "") rhs_sum <- "1"
              rhs_sum
            }
          ),
          collapse = "|"
        )
      ),
      collapse = ""
    )
 
  Formula::as.Formula(Fml)
 
}
