#' @title Dominance analysis methods
#' @name domir
#' @description
#' Parses input object to obtain a list of names, determines which 
#' combinations of the names are valid, submits valid name 
#' combinations to a function, and computes dominance 
#' values based on the returned values from the function.
#' @param .obj A `formula` or `formula_list`.
#' Parsed to produce a list of names. All valid combinations names from 
#' the list are [`sapply`]-ed to `.fct`.
#' 
#' The combinations of names submitted to `.fct` are formatted to be of
#' the same [`class`] as `.obj` and must be submitted to
#' `.fct` as its first, unnamed argument.
#' @param .fct A [`function`] or string function name.
#' Applied to all valid combinations of names parsed from `.obj`.
#' Must return a length 1/scalar, numeric, atomic vector.
#' @param .set A `list`.
#' Each element of the list must be the same class as `.obj`.
#' Elements of the list can be named.
#' Names parsed from the elements of the list must also be in `.obj`.
#' @param .wst A `list`.
#' Each element of the list must be the same class as `.obj`.
#' Names parsed from the elements of the list must also be in `.obj`.
#' @param .all A `formula` or `formula_list`.
#' Must be the same class as `.obj`.
#' Parsed names must also be in `.obj`.
#' @param .adj Logical.
#' If `TRUE` then a model including only an intercept is submitted to `.fct`
#' and the value returned is subtracted from the values returned from all
#' subsets in the dominance analysis.
#' @param .cdl `NULL`.
#' Depreciated. Use `.cdl = FALSE` in `domir`'s `print` method to suppress 
#' display of conditional dominance values.
#' @param .cpt `NULL`.
#' Depreciated. Use `.cpt = FALSE` in `domir`'s `print` method to suppress 
#' display of complete dominance values.
#' @param .rev Logical.
#' If `TRUE` then standardized vector, ranks, and complete dominance
#' designations are reversed in their interpretation.
#' @param .cst Object of class c("SOCKcluster", "cluster") from
#' [`parallel-package`].
#'
#' When non-`NULL`, will alter the method for collecting values from all
#' combinations of names from using [`sapply`] to [`parallel::parSapply`].
#' @param .prg Logical.
#' If `TRUE` then a progress bar is displayed during collection of values
#' to indicate progress.
#' @param ... Passes arguments to other methods during method dispatch;
#' passes arguments to the function in `.fct` during function execution.
#' @return Returns an object of [`class`] "domir" composed of:
#' \describe{
#'  \item{`General_Dominance`}{Vector of general dominance values.}
#'  \item{`Standardized`}{Vector of general dominance values standardized
#'  to sum to 1.}
#'  \item{`Ranks`}{Vector of ranks applied to the general dominance values.}
#'  \item{`Conditional_Dominance`}{Matrix of conditional dominance values.
#'  Each row represents a name in `.obj`;
#'  each column represents a number of names included in `.fct`.}
#'  \item{`Complete_Dominance`}{Matrix of proportions of subsets where the
#'  name in the row has a larger value than the name in the column.
#'  These proportions determine complete dominance when a value of
#'  1 or 0.}
#'  \item{`Value`}{Value returned by `.fct` with all names included.}
#'  \item{`Value_All`}{Value of `.fct` associated with names included
#'  in `.all`;
#'  when `.adj` is `TRUE`, this value will be adjusted for `Value_Adjust`.}
#'  \item{`Value_Adjust`}{Value of `.fct` returned when no names are included.}
#'  \item{`Call`}{The matched call.}
#' }
#' @details
#' ## Name Parsing
#' `.obj` is first parsed into a list of names.
#' How the name list is parsed depends on `.obj`'s class.
#' ### `formula`
#' The `formula` method creates a name list using all terms in the formula.
#' The terms are obtained using [`terms.formula`]. 
#' All processing that is normally applied to the right hand side of a 
#' formula is implemented (see [`formula`]).
#'
#' A response/left hand side is not required but, if present, is
#' included in all `formula`s passed to `.fct`.
#' ### `formula_list`
#' The [`formula_list`] creates a name list out of response-term pairs.
#' The terms are obtained using `terms.formula` applied to each individual
#' formula in the list.
#'
#' The `formula_list` methods make it possible to implement dominance analysis
#' for parameter estimates in multivariate predictive models
#' (e.g., Luchman, Xie, & Kaplan, 2020).
#' ### Additional Details
#' `formula`s and `formula_list` elements are assumed to have an intercept
#' except if explicitly removed with a `- 1` in the `formula`(s) in `.obj`.
#' If removed, the intercept will be removed in all `formula`(s) in each
#' `sapply`-ed subset to `.fct`.
#' If [`offset`]s are included, they are passed, like intercepts, while
#' `sapply`-ing subsets to `.fct`.
#' ## Changing Combination Generation
#' Parsed names are used independently for creating combinations of names 
#' to submit to`.fct`. 
#' When using `.set`, `.wst`, and `.all`, the way combinations are 
#' created changes.
#' Names in `.set`, `.wst`, and `.all` must also be present in `.obj`.
#' ### `.set`
#' `.set` binds together names such that they are considered to be one name 
#' when creating combinations. 
#' The names in each `.set` then pool their returned value together with the 
#' other members of their set.
#'
#' By default, sets are referred to by set names in all output.
#' Unnamed `.set` list elements are called 'set@' where '@' is an integer 
#' indicating the set's position in the list. 
#' The user can give each set in the `.set` list a name that will be used in 
#' place of the default.
#' Names of `.set`s cannot contain the period character '.'.
#'
#' `.set` implements the grouped dominance analysis method described by
#' Luchman (2026).
#' ### `.wst`
#' `.wst` binds together names such that they are considered to be one name 
#' when creating combinations with other names that are not in their `.wst`.
#' All combinations of names of the members of names within a `.wst` 
#' are considered valid.
#' The names in each `.wst` then pool their returned value together with the 
#' other members of their set when compared to names outside of the `.wst` 
#' but do not pool their returned value when compared to names within their
#' `.wst`.
#' 
#' `.wst` list elements cannot be named.
#'
#' `.wst` implements the within-group dominance analysis method described by
#' Luchman (2026).
#' `.wst` and `.set` can be used together.
#' ### `.all`
#' `.all` binds together names such that they are considered to be one name 
#' when creating combinations and are always included first, before all 
#' other names.
#' The names in `.all` will then be included in 'all' valid combinations 
#' of names.
#' The names in `.all` are included after the combination with no names but 
#' before any other combinations.
#' The value associated with `.all` is also removed from the 
#' dominance analysis and is reported along with the overall value 
#' including all names.
#'
#' The `formula` method for `.all` does not allow the formula in `.all` to have
#' a left hand side.
#' ### `.adj`
#' By default, `domir` assumes that the combination of no names has a 
#' value of 0.
#' `.adj` indicates that the there is an 'adjustment' needed such that the 
#' no names combination has a non-0 value and an intercept-only model should 
#' be supplied to `.fct`.
#'
#' The `formula` method will submit an intercept-only formula to `.fct`.
#' The `formula_list` method creates a separate, intercept-only subset for each
#' of the `formula`s in the list.
#' Both the `formula` and `formula_list` methods will respect the user's
#' removal of an intercept and or inclusion of an `offset`.
#' ### Additional Details
#' All methods submit combinations of names as an object of the same class as
#' `.obj`. 
#' A `formula` in `.obj` will submit all combinations of names as
#' `formula`s to `.fct`. 
#' A `formula_list` in `.obj` will submit all
#' combinations of subsets of names as `formula_list`s to `.fct`.
#' In the case that `.fct` requires a different `class` (e.g.,
#' a character vector of names, a [`Formula::Formula`] see [`fmllst2Fml`]) the
#' subsets of names will have to be processed in `.fct` to obtain the correct
#' `class`.
#'
#' The `formula` or `formula_list` of names will be submitted to `.fct` as the 
#' first, unnamed argument.
#' ## `.fct` as Analysis Pipeline
#' `.fct` is expected to be a complete analysis pipeline that receives a
#' subset of names of the same `class` as `.obj` and uses these names in the
#' `class` as submitted to generate a returned value of the appropriate
#' type to dominance analyze.
#' At current, only atomic (i.e., non-`list`), numeric scalars (i.e.,
#' vectors of length 1) are allowed as returned values.
#' `domir` is designed for use with predictive models and assumes the returned 
#' value is a scalar-valued fit statistic/metric.
#'
#' The `.fct` argument is strict about names submitted and returned value
#' requirements for functions used. 
#' A series of checks to ensure the submitted names and returned value adhere 
#' to these requirements.
#' The checks include whether the `.obj` can be submitted to `.fct` without
#' producing an error and whether the returned value from `.fct` is a length 1,
#' atomic, numeric vector.
#' In most circumstances, the user will have to make their own named or
#' anonymous function to supply as `.fct` to satisfy the checks.
#' # Notes
#' Prior to version 1.1.0, the `formula` method allowed a `formula`
#' to be submitted to `.adj`.
#' Submitting any argument other than a logical is now defunct.
#'
#' The `formula` and `formula_list` methods can be used to pass responses,
#' intercepts, and `offset`s to all combinations of names.
#' If the user seeks to include other model components integral to
#' estimation (i.e., a random effect term in [`lme4::glmer()`]) include them as
#' [`update`][update.formula] to the submitted `formula` or `formula_list`
#' embedded in `.fct`.
#'
#' Second-order or higher terms (i.e., interactions like `~ a*b`) are parsed
#' by default but not used differently from first-order terms for generating 
#' valid combinations. 
#' The values ascribed to such names may not be valid unless the user ensures 
#' that second-order and higher term names are used appropriately in `.fct`.
#' @export
#' @examples
#' ## Linear model returning r-square
#' lm_r2 <-
#'   function(fml, data) {
#'     lm_res <- lm(fml, data = data)
#'     summary(lm_res)[["r.squared"]]
#'  }
#'
#' domir(mpg ~ am + vs + cyl, lm_r2, data = mtcars)
#'
#' ## Linear model including set
#' domir(
#'   mpg ~ am + vs + cyl + carb + gear + disp + wt,
#'   lm_r2,
#'   .set = list(~ carb + gear, ~ disp + wt),
#'   data = mtcars
#' )
#'
#' ## Multivariate regression with multivariate r-square and
#' ## all subsets variable
#' if (requireNamespace("performance", quietly = TRUE)) {
#'   mlm_rxy <-
#'     function(fml, data) {
#'       mlm_res <- lm(fml, data = data)
#'       performance::r2_mlm(mlm_res)[["Symmetric Rxy"]]
#'     }
#'
#'   domir(
#'     cbind(wt, mpg) ~ vs + cyl + am + carb,
#'     mlm_rxy,
#'     .all = ~ carb,
#'     data = mtcars
#'   )
#' }
#'
#' ## Named sets
#' domir(
#'   mpg ~ am + gear + cyl + vs + qsec + drat,
#'   lm_r2,
#'   data = mtcars,
#'   .set =
#'     list(
#'       trns = ~ am + gear,
#'       eng = ~ cyl + vs,
#'       misc = ~ qsec + drat
#'     )
#' )
#'
#' ## Linear model returning AIC
#' lm_aic <-
#'   function(fml, data) {
#'     lm_res <- lm(fml, data = data)
#'     AIC(lm_res)
#'  }
#'
#' domir(
#'   mpg ~ am + carb + cyl,
#'   lm_aic,
#'   .adj = TRUE,
#'   .rev = TRUE,
#'   data = mtcars
#'  )
#'
#' ## 'systemfit' with 'formula_list' method returning AIC
#' if (requireNamespace("systemfit", quietly = TRUE)) {
#'   domir(
#'     formula_list(mpg ~ am + cyl + carb, qsec ~ wt + cyl + carb),
#'     function(fml) {
#'       res <- systemfit::systemfit(fml, data = mtcars)
#'       AIC(res)
#'     },
#'     .adj = TRUE, .rev = TRUE
#'   )
#' }
#'
#' ## within-set or within-group dominance analysis
#' domir(
#'   mpg ~ am + gear + cyl + vs + qsec + drat,
#'   lm_r2,
#'   data = mtcars,
#'   .wst =
#'     list(
#'       ~ am + gear,
#'       ~ cyl + vs,
#'       ~ qsec + drat
#'     )
#' )
#' @references
#' \itemize{
#' \item Luchman, J. N., Lei, X., & Kaplan, S. (2020). Relative Importance
#' Analysis with Multivariate Models: Shifting the Focus from Independent
#' Variables to Parameter Estimates. Journal of Applied Structural Equation
#' Modeling, 4(2), 1-20. doi:https://doi.org/10.47263/JASEM.4(2)02
#' \item Luchman, J. N. (2026). Determining Relative Importance with
#' Independent Variable Groups: An Alternative Dominance Analysis Method.
#' Journal of Behavioral Data Science, 6(1), 1-26.
#' doi:https://doi.org/10.35566/jbds/luchman
#'}
#'
domir <- function(.obj, ...) {
  UseMethod("domir")
}
#' @rdname domir
#' @exportS3Method
domir.formula <- function(
    .obj, .fct,
    .set = NULL, .wst = NULL,
    .all = NULL, .adj = FALSE,
    .cdl = NULL, .cpt = NULL,
    .rev = FALSE, .cst = NULL, .prg = FALSE, ...) {
  domir_arg_checker(.rev, .cpt, .cdl, .prg, .cst)
  fml_parsed <- formula_parse(.obj)
  if (length(fml_parsed$rhs_names) == 0)
    stop("The formula in '.obj' must have one or more terms.", call. = FALSE)
  entire_namelist_value <- formula_output_check(.obj, .fct, TRUE, ...)
  adj_checker(.adj, fml_parsed, TRUE)
  adj_value <- est_adj_value(.adj, fml_parsed, .fct, TRUE, ...)
  all_parsed <- all_checker(.all, fml_parsed, TRUE)
  fml_parsed <- fml_all_update(all_parsed, fml_parsed, TRUE)
  all_value <- est_all_value(.all, fml_parsed, .fct, adj_value, TRUE, ...)
  set_checker(.set, fml_parsed, "'.set'", TRUE)
  sets_parsed <- set_element_checker(.set, fml_parsed, "'.set'", TRUE)
  set_checker(.wst, fml_parsed, "'.wst'", TRUE)
  wsts_parsed <- set_element_checker(.wst, fml_parsed, "'.wst'", TRUE)
  check_namelists(fml_parsed, sets_parsed, wsts_parsed, all_parsed, TRUE)
  names_for_dominance <-
    determine_dominance_names(fml_parsed, sets_parsed, wsts_parsed, TRUE)
  return_list <-
    dominance_scalar(
      fml_parsed, .fct, names_for_dominance, entire_namelist_value,
      adj_value, all_value,
      .rev, .cst, .prg, list(...), FALSE)
  names_for_printing <- determine_display_names(names_for_dominance, .set)
  return_list <- name_return_list(return_list, names_for_printing)
  if (!.adj) adj_value <- NULL
  if (is.null(.all)) all_value <- NULL
  return_list <-
    append(
      return_list,
    list(
      Value = entire_namelist_value,
      Value_All = all_value,
      Value_Adjust = adj_value,
      Call = match.call()
    )
  )
  class(return_list) <- c("domir")
  return_list
}
#' @rdname domir
#' @exportS3Method
domir.formula_list <- function(
    .obj, .fct,
    .set = NULL, .wst = NULL, .all = NULL, .adj = FALSE,
    .cdl = NULL, .cpt = NULL,
    .rev = FALSE, .cst = NULL, .prg = FALSE, ...) {
  domir_arg_checker(.rev, .cpt, .cdl, .prg, .cst)
  fmllst_parsed <- lapply(.obj, formula_parse)
  rhs_term_counts <- 
    sapply(fmllst_parsed, function(elem) length(elem$rhs_names))
  if (any(rhs_term_counts == 0)) {
    stop(
      paste("Each formula in '.obj' must have one or more terms.",
            "Formulas", paste(which(rhs_term_counts == 0), collapse = " "),
            "have no terms."),
      call. = FALSE
      )
  }
  entire_namelist_value <- formula_output_check(.obj, .fct, FALSE, ...)
  adj_checker(.adj, fmllst_parsed, FALSE)
  adj_value <- est_adj_value(.adj, fmllst_parsed, .fct, FALSE, ...)
  all_parsed <- all_checker(.all, fmllst_parsed, FALSE)
  fmllst_parsed <- fml_all_update(all_parsed, fmllst_parsed, FALSE)
  all_value <- est_all_value(.all, fmllst_parsed, .fct, adj_value, FALSE, ...) 
  set_checker(.set, fmllst_parsed, "'.set'", FALSE)
  sets_parsed <- set_element_checker(.set, fmllst_parsed, "'.set'", FALSE)
  set_checker(.wst, fmllst_parsed, "'.wst'", FALSE)
  wsts_parsed <- set_element_checker(.wst, fmllst_parsed, "'.wst'", FALSE)
  check_namelists(fmllst_parsed, sets_parsed, wsts_parsed, all_parsed, FALSE)
  names_for_dominance <-
    determine_dominance_names(fmllst_parsed, sets_parsed, wsts_parsed, FALSE)
  return_list <-
    dominance_scalar(
      fmllst_parsed, .fct, names_for_dominance, entire_namelist_value,
      adj_value, all_value,
      .rev, .cst, .prg, list(...), TRUE)
  names_for_printing <- determine_display_names(names_for_dominance, .set)
  return_list <- name_return_list(return_list, names_for_printing)
  if (!.adj) adj_value <- NULL
  if (is.null(.all)) all_value <- NULL
  return_list <-
    append(
      return_list,
      list(
        Value = entire_namelist_value,
        Value_All = all_value,
        Value_Adjust = adj_value,
        Call = match.call()
      )
    )
  class(return_list) <- c("domir")
  return_list
}
#' @title Formula parsing function
#' @description Internal formula parsing function to facilitate
#' re-construction of a formula in an external function using `reformulate()`.
#' @noRd
#' @param .obj A `formula`.
#' @returns A list composed of:
#' \describe{
#'  \item{`rhs_names`}{Character vector of names from the RHS/right hand side
#'  of the formula.}
#'  \item{`lhs_names`}{`call` vector of names from the LHS/left hand side
#'  of the formula. Note that this element is not a character vector.}
#'  \item{`intercept_lgl`}{Logical vector indicating whether the `formula`
#'  has an intercept.}
#'  \item{`offset`}{Character vector of offset terms to be included in the
#'  reconstructed `formula`.}
#'  \item{`select_lgl`}{Logical vector for use by `domir` to indicate whether
#'  names from the `rhs_names` list will be included in a submodel.}
#' }
formula_parse <- function(.obj) {
  if (is.null(.obj)) return(NULL)
  rhs_names <-
    tryCatch(
      attr(stats::terms(.obj), "term.labels"),
      error = function(err) {
        stop(deparse(.obj), " is an invalid formula.", call. = FALSE)
      }
    )
  intercept_lgl <- as.logical(attr(stats::terms(.obj), "intercept"))
  if (!is.null(attr(stats::terms(.obj), "offset"))) {
    offset_locs <- attr(stats::terms(.obj), "offset")
    offset <- rownames(attr(stats::terms(.obj), "factors"))[offset_locs]
    if (is.null(offset))
      offset <-
      sapply(
        (offset_locs + 1),
        function(loc) attr(stats::terms(.obj), "variables")[[loc]]
      )
  } else {
    offset <- NULL
  }
  if (attr(stats::terms(.obj), "response") == 1) {
    lhs_names <- attr(stats::terms(.obj), "variables")[[2]]
  } else {
    lhs_names <- NULL
  }
  select_lgl <- rep(FALSE, times = length(rhs_names))
  list(rhs_names = rhs_names,
       lhs_names = lhs_names,
       intercept_lgl = intercept_lgl,
       offset = offset,
       select_lgl = select_lgl)
}
#' @title formula output checking function
#' @description Internal function which ensures that `.fct` does not
#' produce errors when `.obj` is applied to it and that `.fct` produces an
#' atomic scalar-valued numeric result.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .obj A `formula` or `formula_list`.
#' @param .fct A `function` or string function name.
#' @param .is_fml Logical.
#' @param ... Passes arguments to the function in `.fct`.
#' @returns Result of the `.fct` call using `.obj`.
formula_output_check <- function(.obj, .fct, .is_fml, ...) {
  if (.is_fml) {
    evaluate_fml <- do.call(eval(.fct), append(.obj, list(...)))
  } else {
    evaluate_fml <- do.call(eval(.fct), append(list(.obj), list(...)))
  }
  test_model <-
    tryCatch(
      evaluate_fml,
      error = function(err) {
        stop("'.fct' produced an error when applied to '.obj'.\n",
             "The error was:\n", err, call. = FALSE)
      }
    )
  if (!is.numeric(test_model) || !is.vector(test_model) ||
      !is.atomic(test_model) || length(test_model) != 1)
    stop("Result of '.fct' is not an atomic, numeric, scalar object ",
         "(vector with a 'length()' value of 1).", call. = FALSE)
  test_model
}
#' @title `domir` argument checking function
#' @description Internal function to ensure that the arguments unrelated to
#' names are formulated correctly.
#' @noRd
#' @param .rev Logical.
#' @param .cpt `NULL`.
#' @param .cdl `NULL`.
#' @param .prg Logical.
#' @param .cst Object of class c("SOCKcluster", "cluster") from
#' [`parallel-package`]
#' @returns `NULL`
domir_arg_checker <-
  function(.rev, .cpt, .cdl, .prg, .cst) {
    lgl_args <- sapply(list(.rev, .prg), is.logical)
    if (!all(lgl_args))
      stop(
        paste(
          c(".rev", ".prg")[which(!lgl_args)],
          collapse = " "
        ),
        " must be logical.", call. = FALSE
      )
    if (!all(c(is.null(.cpt), is.null(.cdl))))
      warning("'.cpt' and '.cdl' are depreciated arguments to 'domir' as of ",
              "version 1.3.\nUse '.cdl' and '.cpt' as arguments to 'print()' ",
              "instead.", call. = FALSE)
    if (!is.null(.cst) && .prg)
      stop("Progress bars do not yet work with parallelized value ",
           "estimation.", call. = FALSE)
    if (!is.null(.cst) && !inherits(.cst, "SOCKcluster"))
      stop(
        "Object in '.cst' not a cluster from package 'parallel'.",
        call. = FALSE
      )
    NULL
  }
#' @title `.adj` argument checking function
#' @description Internal function which ensures that the arguments to `.adj`
#' methods are formulated correctly.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .adj Logical.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .is_fml Logical.
#' @returns `NULL`
adj_checker <- function(.adj, fml_parsed, .is_fml) {
  if (!is.logical(.adj) || (length(.adj) > 1))
    stop("'.adj' argument must be logical of length 1.", call. = FALSE)
  if (.is_fml) fml_parsed <- list(fml_parsed)
  rmv_intercept_locs <- sapply(fml_parsed, function(elem) elem$intercept_lgl)
  if (any(!rmv_intercept_locs) && .adj)
    stop("'.adj' cannot be estimated with intercepts removed from '.obj'.", 
         call. = FALSE)
  NULL
}
#' @title Return an intercept-only or 'no names' value
#' @description Internal function to estimates the value associated with no
#'  value generating names included.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .adj Logical.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .fct A `function` or string function name.
#' @param .is_fml Logical.
#' @param ... Passes arguments to the function in `.fct`.
#' @returns Result of `.fct` call applied to `.obj` with no names.
est_adj_value <- function(.adj, fml_parsed, .fct, .is_fml, ...) {
  if (.adj && .is_fml) {
    fml <-
      stats::reformulate(
        c("1", fml_parsed$offset),
        response = fml_parsed$lhs_names,
        intercept = fml_parsed$intercept_lgl)
    value <- formula_output_check(fml, .fct, .is_fml, ...)
  } else if (.adj && !.is_fml) {
    fml_lst <-
      lapply(
        fml_parsed,
        function(fml) {
          stats::reformulate(
            c("1", fml$offset),
            response = fml$lhs_names,
            intercept = fml$intercept_lgl)
        }
      )
    fml_lst <- do.call("formula_list", fml_lst)
    value <- formula_output_check(fml_lst, .fct, .is_fml, ...)
  } else {
    value <- 0
  }
  value
}
#' @title `.all` argument checking and parsing function
#' @description Internal function to check `.all` argument and process it using
#' `formula_parse()`.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .all A `formula` or `formula_list`.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .is_fml Logical.
#' @returns A `formula_parse` object applied to `.all` object.
all_checker <- function(.all, fml_parsed, .is_fml) {
  if (is.null(.all)) return(NULL)
  if (!inherits(.all, "formula") && .is_fml) {
    stop("'.all' must be a 'formula'.", call. = FALSE)
  } else if (!inherits(.all, "formula_list") && !.is_fml) {
    stop("'.all' must be a 'formula_list'.", call. = FALSE)
  }
  if (.is_fml) {
    all_pre_parse <- list(.all)
  } else {
    all_pre_parse <- .all
  }
  all_parsed <- lapply(all_pre_parse, function(elem) formula_parse(elem))
  if (any(sapply(all_parsed, function(elem) length(elem$rhs_names) == 0)))
    stop("Formulas in '.all' must have one or more terms.", call. = FALSE)
  if (
    any(sapply(all_parsed, function(elem) !is.null(elem$lhs_names))) && .is_fml
  ) {
    stop("Left hand side names not allowed in '.all' formulas.", call. = FALSE)
  } else if (.is_fml) {
    all_parsed[[1]]$lhs_names <- fml_parsed$lhs_names
  }
  if (any(sapply(all_parsed, function(elem) !is.null(elem$offset))))
    stop("Offsets not allowed in '.all' formulas.", call. = FALSE)
  if (any(sapply(all_parsed, function(elem) !elem$intercept_lgl)))
    stop("Removing intercepts not allowed in '.all' formulas.", call. = FALSE)
  if (.is_fml) all_parsed <- all_parsed[[1]]
  all_parsed
}
#' @title Adjusts `fml_parsed` object for names in `.all`
#' @description Internal function to adjust `.obj` object
#' processed by `formula_parse()` to include names in `.all` in all subsets
#' of names.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .all A `formula` or `formula_list`.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .is_fml Logical.
#' @returns An updated `formula_parse` object.
fml_all_update <- function(all_parsed, fml_parsed, .is_fml) {
  if (is.null(all_parsed)) return(fml_parsed)
  else if (.is_fml) {
    fml_parsed <- list(fml_parsed)
    all_parsed <- list(all_parsed)
  }
  invalid_all_names <-
    setdiff(
      Reduce(
        union,
        sapply(
          all_parsed,
          function(elem)
            paste(deparse(elem$lhs_names), "~",
                  elem$rhs_names)), NULL),
      Reduce(
        union,
        sapply(
          fml_parsed,
          function(elem) 
            paste(deparse(elem$lhs_names), "~", elem$rhs_names)), NULL)
    )
  if (length(invalid_all_names) > 0) {
    invalid_all_names <-
      ifelse(
        .is_fml,
        paste(
          sapply(
            invalid_all_names,
            function(elem)
              substr(
                elem,
                nchar(deparse(all_parsed[[1]]$lhs_names)) + 4,
                nchar(elem))
          ),
          collapse = ", "
        ),
        paste(invalid_all_names, collapse = ", ")
      )
    stop("Name(s) in '.all' not found in '.obj' formulas.",
         call. = FALSE)
  }
  fml_parsed <-
    lapply(
      fml_parsed,
      function(elem) {
        lhs_names_all <-
          unlist(sapply(all_parsed, function(el) deparse(el$lhs_names)))
        pairs_all <-
          Reduce(
            union,
            sapply(
              all_parsed,
              function(el)
                paste(deparse(el$lhs_names), "~",
                      el$rhs_names)), NULL)
        if (deparse(elem$lhs_names) %in% lhs_names_all) {
          iv_dv_pairs <-
            paste(deparse(elem$lhs_names), "~", elem$rhs_names)
          which_to_true <- iv_dv_pairs %in% pairs_all
          elem_adj <- elem
          elem_adj$select_lgl <- which_to_true
          return(elem_adj)
        } else {
          return(elem)
        }
      }
    )
  if (.is_fml) fml_parsed <- fml_parsed[[1]]
  fml_parsed
}
#' @title Return an all subsets names value
#' @description Internal function to estimates the value associated with the
#' names defined to be in all subsets.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .all `formula` or `formula_list`.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .fct A `function` or string function name.
#' @param .adj A scalar value produced by `est_adj_value()`.
#' @param .is_fml Logical.
#' @param ... Passes arguments to the function in `.fct`.
#' @returns Result of `.fct` call applied to `.obj` using only the names
#' defined in `.all`.
est_all_value <- function(.all, fml_parsed, .fct, .adj, .is_fml, ...) {
  if (!is.null(.all) && .is_fml) {
    fml <-
      stats::reformulate(
        c(fml_parsed$rhs_names[fml_parsed$select_lgl], fml_parsed$offset),
        response = fml_parsed$lhs_names,
        intercept = fml_parsed$intercept_lgl)
    value <- formula_output_check(fml, .fct, .is_fml, ...)
  } else if (!is.null(.all) && !.is_fml)  {
    fml_lst <-
      lapply(
        fml_parsed,
        function(fml) {
          rhs <- c(fml$rhs_names[fml$select_lgl], fml$offset)
          if (length(rhs) == 0) rhs <- "1"
          stats::reformulate(
            rhs,
            response = fml$lhs_names,
            intercept = fml$intercept_lgl)
        }
      )
    fml_lst <- do.call("formula_list", fml_lst)
    value <- formula_output_check(fml_lst, .fct, .is_fml, ...)
  } else {
    value <- 0
  }
  ifelse(is.null(.all), value, value - .adj)
}
#' @title `.set` and `.wst` argument checking function
#' @description Internal function to check `.set` and `.wst` arguments.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .set A list of `formula`s or `formula_list`s.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .typ A character vector. Must be '.set' or '.wst'.
#' @param .is_fml Logical.
#' @returns NULL
set_checker <- function(.set, fml_parsed, .typ, .is_fml) {
  if (is.null(.set)) return(NULL)
  if (!is.list(.set)) stop(.typ, " must be a list.", call. = FALSE)
  if (length(.set) == 0) stop(.typ, " is an empty list.", call. = FALSE)
  if (.typ == "'.wst'" && !is.null(names(.set))) {
    stop("Names for '.wst's are not allowed.", call. = FALSE)
  } else if (any(grepl("\\.", names(.set)))) {
    stop("Names for '.set' cannot contain the period character '.'.", # !! this necessary?
         call. = FALSE)
  }
  .obj <- ifelse(.is_fml, "formula", "formula_list")
  not_fmls <- sapply(.set, function(elem) {!inherits(elem, .obj)})
  if (any(not_fmls)) {
    which_not_fml <- seq_len(length(not_fmls))[not_fmls]
    stop("List element(s):\n", paste(which_not_fml, collapse = " "),
         "\nof ", .typ, " are not the same type as '.obj'.", call. = FALSE)
  }
  if (.typ == "'.set'") {
    fml_parsed <- list(fml_parsed)
    set_names <- set_labeller(.set, FALSE)
    set_namelist_overlap <-
      intersect(
        set_names,
        unlist(lapply(fml_parsed, function(elem) elem$rhs_names))
      )
    if (length(set_namelist_overlap) > 0)
      stop("Name(s) '", paste(set_namelist_overlap, collapse = "','"),
           "' overlap with names of sets. Give these sets new names.",
           call. = FALSE)
  }
  NULL
}
#' @title `.set` and `.wst` list element checking and parsing function
#' @description Internal function to check individual `.set` and `.wst`
#' arguments and process them using `formula_parse()`.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .set A list of `formula`s or `formula_list`s.
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param .typ A character vector. Must be '.set' or '.wst'.
#' @param .is_fml Logical.
#' @returns A `formula_parse` object `lapply`-ed applied to the `.set` object.
set_element_checker <- function(.set, fml_parsed, .typ, .is_fml) {
  if (is.null(.set)) return(NULL)
  if (.is_fml) {
    sets_parsed <- lapply(.set, function(elem) list(formula_parse(elem)))
  } else {
    sets_parsed <- lapply(.set, function(elem) lapply(elem, formula_parse))
  }
  sets_check <-
    lapply(
      sets_parsed,
      function(eq) {
        eq_res <-
          lapply(
            eq,
            function(elem) {
              list(
                rhs = length(elem$rhs_names) == 0,
                lhs = length(elem$lhs_names) > 0,
                offset = !is.null(elem$offset),
                intercept = elem$intercept_lgl
              )
            }
          )
        names(eq_res) <- seq_len(length(eq_res))
        eq_res
      }
    )
  rhs_counts_sets <-
    unlist(lapply(sets_check, function(eq) lapply(eq, function(elem) elem$rhs)))
  if (any(unlist(rhs_counts_sets)))
    stop("Formulas in ", .typ, " must have one or more terms.", call. = FALSE)
  bad_lhs <-
    unlist(lapply(sets_check, function(eq) lapply(eq, function(elem) elem$lhs)))
  if (any(bad_lhs) && .is_fml)
    stop("Left hand sides not allowed in ", .typ, " formulas.", call. = FALSE)
  bad_offset <-
    unlist(lapply(
      sets_check, function(eq) lapply(eq, function(elem) elem$offset)
    ))
  if (any(bad_offset))
    stop("Offsets not allowed in ", .typ, " formulas.", call. = FALSE)
  bad_intercept <-   
    unlist(lapply(
      sets_check,
      function(eq) lapply(eq, function(elem) !elem$intercept)
    ))
  if (any(bad_intercept))
    stop("Removing intercepts not allowed in ", .typ, " formulas.",
         call. = FALSE)
  if (.is_fml) sets_parsed <- lapply(sets_parsed, function(elem) elem[[1]])
  sets_parsed
}
#' @title Name checking function
#' @description Internal function to check for overlap between `.set`, `.wst`,
#' and `.all`. Also checks to ensire all names in `.set` and `.wst` are also
#' in `.obj`.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param sets_parsed `.set` processed with `element_set_checker()`.
#' @param wsts_parsed `.wst` processed with `element_set_checker()`.
#' @param all_parsed `.all` processed with `all_checker()`.
#' @param .is_fml Logical.
#' @returns NULL
check_namelists <-
  function(fml_parsed, sets_parsed, wsts_parsed, all_parsed, .is_fml) {
    if (.is_fml) {
      fml_parsed$lhs_names <- "..domir"
      fml_parsed <- list(fml_parsed)
      if (!is.null(sets_parsed)) {
        sets_parsed <-
          lapply(
            sets_parsed,
            function(elem) {
              elem$lhs_names <- "..domir"
              list(elem)
            }
          )
      }
      if (!is.null(wsts_parsed)) {
        wsts_parsed <-
          lapply(
            wsts_parsed,
            function(elem) {
              elem$lhs_names <- "..domir"
              list(elem)
            }
          )
      }
      if (!is.null(all_parsed)) {
        all_parsed$lhs_names <- "..domir"
        all_parsed <- list(all_parsed)
      }
    }
    namelist <-
      lapply(
        fml_parsed,
        function(elem) paste(elem$lhs_names, "~", elem$rhs_names, sep = "")
      )
    sets_namelists <-
      lapply(
        sets_parsed,
        function(elem) {
          lapply(
            elem,
            function(el) {
              paste(el$lhs_names, "~", el$rhs_names, sep = "")
            }
          )
        }
      )
    wsts_namelists <-
      lapply(
        wsts_parsed,
        function(elem) {
          lapply(
            elem,
            function(el) {
              paste(el$lhs_names, "~", el$rhs_names, sep = "")
            }
          )
        }
      )
    if (
      length(setdiff(unlist(namelist), unlist(wsts_namelists))) == 0 &&
      length(wsts_namelists) == 1
    )
      stop("All names in '.obj' cannot be grouped into a single '.wst'.",
           call. = FALSE)
    all_namelist <- lapply(all_parsed, function(elem) elem$rhs_names)
    if (length(intersect(unlist(all_namelist), unlist(sets_namelists))) > 0) {
      stop("The same names cannot be in both '.set' and '.all'.", call. = FALSE)
    }
    if (length(intersect(unlist(all_namelist), unlist(wsts_namelists))) > 0) {
      stop("The same names cannot be in both '.wst' and '.all'.", call. = FALSE)
    }
    if (length(intersect(unlist(sets_namelists), unlist(wsts_namelists))) > 0) {
      stop("The same names cannot be in both '.set' and '.wst'.", call. = FALSE)
    }
    if (length(setdiff(unlist(sets_namelists), unlist(namelist))) > 0) {
      stop("Names in '.set' missing from '.obj'.", call. = FALSE)
    }
    if (length(setdiff(unlist(wsts_namelists), unlist(namelist))) > 0) {
      stop("Names in '.wst' missing from '.obj'.", call. = FALSE)
    }
    if (any(duplicated(unlist(sets_namelists)))) {
      stop("Duplicated names in '.set's.", call. = FALSE)
    }
    if (any(duplicated(unlist(wsts_namelists)))) {
      stop("Duplicated names in '.wst's.", call. = FALSE)
    }
    NULL
  }

#' @title `.set` and `.wst` labelling function
#' @description Internal function to apply labels to `.set`s and `.wst`s.
#' Applies names defined in `.set` by the user.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param .set A `formula` or `formula_list` in the format required by either
#' `.set` or `.wst` arguments.
#' @param .is_wst Logical.
#' @returns Character vector.
set_labeller <- function(.set, .is_wst) {
  if (is.null(.set)) return(NULL)
  name_type <- ifelse(.is_wst, "wst", "set")
  if (is.null(names(.set))) {
    set_labels <- paste(name_type, seq_len(length(.set)), sep = "")
  } else {
    set_labels <- names(.set)
  }
  missing_set_labels <- which(set_labels == "")
  if (length(missing_set_labels) > 0)
    set_labels[missing_set_labels] <- paste("set", missing_set_labels, sep = "")
  set_labels
}
#' @title Name to `.obj` structure coordinating method
#' @description Internal function to link names to `formula`s or
#' `formula_list`s in a way that can be referred to across normal,
#' `.set`, and `.wst` structures.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param fml_parsed `formula` or `formula_list` processed with
#' `formula_parse()`.
#' @param sets_parsed `.set` processed with `set_element_checker()`.
#' @param wsts_parsed `.wst` processed with `set_element_checker()`.
#' @param .is_fml Logical.
#' @returns Data frame with three columns: 'eq', 'elem', and 'name'.
#' 'eq' is the integer formula location of the name; always 1 for `formula`s.
#' 'elem' is the integer term location in the name in the formula.
#' 'name' a character.
determine_dominance_names <-
  function(fml_parsed, sets_parsed, wsts_parsed, .is_fml) {
    if (.is_fml) {
      fml_parsed <- list(fml_parsed)
      if (!is.null(sets_parsed))
        sets_parsed <-
          lapply(sets_parsed, function(elem) list(elem))
      if (!is.null(wsts_parsed))
        wsts_parsed <-
          lapply(wsts_parsed, function(elem) list(elem))
    }
    namelist <-
      unlist(
        lapply(
          fml_parsed,
          function(elem) {
            dv <- NULL
            if (!.is_fml) dv <- paste(elem$lhs_names, "~", sep = "")
            paste(dv, elem$rhs_names[!elem$select_lgl], sep = "")
          }
        )
      )
    if (is.null(sets_parsed)) {
      sets_namelists <- NULL
    } else {
      sets_namelists <-
        lapply(
          sets_parsed,
          function(elem)
            unlist(lapply(
              elem,
              function(el) {
                dv <- NULL
                if (!.is_fml) dv <- paste(el$lhs_names, "~", sep = "")
                paste(dv, el$rhs_names, sep = "")
              }
            ))
        )
      set_names <- paste("set", seq_len(length(sets_parsed)), sep = "")
      names(sets_namelists) <- set_names
    }
  if (is.null(wsts_parsed)) {
    wsts_namelists <- NULL
  } else {
    wsts_namelists <-
      lapply(
        wsts_parsed,
        function(elem)
          unlist(lapply(
            elem,
            function(el) {
              dv <- NULL
              if (!.is_fml) dv <- paste(el$lhs_names, "~", sep = "")
              paste(dv, el$rhs_names, sep = "")
            }
          ))
      )
    wst_names <- paste("wst", seq_len(length(wsts_parsed)), sep = "")
    names(wsts_namelists) <- wst_names
  }
  submitter_list <-
    setdiff(namelist, c(unlist(sets_namelists), unlist(wsts_namelists)))
  if (length(submitter_list) > 0) {
    submitter_list <- as.list(submitter_list)
    names(submitter_list) <-
      paste("var", seq_len(length(submitter_list)), sep = "")
  } else {
    submitter_list <- list()
  }
  submitter_list <- append(submitter_list, sets_namelists)
  submitter_list <- append(submitter_list, wsts_namelists)
  number_dominance_names <-
    sum(grepl("^var|^set", names(submitter_list))) +
    sum(
      unlist(sapply(wsts_namelists, function(elem) length(elem)))
    )
  if (number_dominance_names < 2) {
    stop("At least two names or sets of names are needed for a ",
         "dominance analysis.", call. = FALSE)
  }
  submitter_list
}
#' @title Formatting for names given `.obj` structure
#' @description Internal function to to apply labels output.
#' Applies names defined in `.set` by the user.
#' Designed to accommodate both `formula` and `formula_list` objects.
#' @noRd
#' @param namelist Character vector of names from `.obj`.
#' @param .set A `formula` or `formula_list` in the format required by the
#' `.set` argument.
#' @returns Character vector.
determine_display_names <- function(namelist, .set) {
    regular_names <- namelist[grepl("var", names(namelist))]
    if (length(regular_names) > 0) names(regular_names) <- NULL
    set_names <- set_labeller(.set, FALSE)
    if (length(set_names) > 0) names(set_names) <- NULL
    wst_names <- namelist[grepl("wst", names(namelist))]
    if (length(wst_names) > 0) names(wst_names) <- NULL
    c(unlist(regular_names), set_names, unlist(wst_names))
  }
#' @title Formatting for names in returned list
#' @description Internal function to to apply names to the elements of the
#' list returned by `domir`.
#' @noRd
#' @param return_list A list of dominance analysis results.
#' @param names_for_printing Character vector.
#' @returns `return_list` with updated names.
name_return_list <- function(return_list, names_for_printing) {
  names(return_list$general) <- names_for_printing
  names(return_list$ranks) <- names_for_printing
  names(return_list$standard) <- names_for_printing
  rownames(return_list$conditional) <- names_for_printing
  colnames(return_list$conditional) <-
    paste("include_at_", seq_len(ncol(return_list$conditional)), sep = "")
  if (!is.null(return_list$complete)) {
    rownames(return_list$complete) <- paste(names_for_printing, "_>", sep = "")
    colnames(return_list$complete) <- paste(">_", names_for_printing, sep = "")
  }
  names(return_list) <-
    c("General_Dominance", "Conditional_Dominance", "Complete_Dominance",
      "Ranks", "Standardized")
  return_list
}
#' @title Print method for `domir`
#' @description Reports formatted results from `domir` class object.
#' @param x an object of class "domir".
#' @param .cdl Logical.
#' If `TRUE` then conditional dominance statistics will be reported.
#' @param .cpt Logical.
#' If `TRUE` then complete dominance proportions will be reported.
#' @param ... further arguments passed to [`print.default`].
#' @return The submitted "domir" object, invisibly.
#' @details The print method for class `domir` objects reports out the
#' following results:
#' \itemize{
#'  \item{Value when all elements are included in `obj`.}
#'  \item{Value for the elements included in `.all`, if any.}
#'  \item{Value for the elements included in `.adj`, if any.}
#'  \item{Matrix describing general dominance values, standardized
#'  general dominance values, and the ranking of the general
#'  dominance values.}
#'  \item{Matrix describing the conditional dominance values
#'  if `.cdl` is `TRUE`.}
#'  \item{Matrix describing the complete dominance designations
#'  if `.cpt` is `TRUE`.}
#'  \item{If following [`summary.domir`], matrix describing the strongest
#'  dominance designations between all elements if both `.cdl` and `.cpt`
#'  are `TRUE`.}}
#'
#'  The `domir` print method alters dimension names for readability and they
#'  do not display as stored in the `domir` object.
#'
#' @exportS3Method
print.domir <- function(x, .cdl = TRUE, .cpt = TRUE, ...) {
  # if (!is.null(as.list(x$Call)$.prg))
  #   switch(
  #     as.character(as.logical(deparse(as.list(x$Call)$.prg))),
  #     `TRUE` = cat("\n"),
  #     `FALSE` = NULL
  #   )
  # ~~ to remove: begin ~~
  temp_cpt <- as.list(x$Call)$.cpt
  temp_cdl <- as.list(x$Call)$.cdl
  if (!is.null(temp_cpt)) .cpt <- as.logical(temp_cpt)
  if (!is.null(temp_cdl)) .cdl <- as.logical(temp_cdl)
  # ~~ to remove: end ~~
  cat("\nOverall Value:     ", x[["Value"]], "\n")
  if (length(x[["Value_All"]]) > 0)
    cat("All Subset Value:  ", x[["Value_All"]], "\n")
  if (length(x[["Value_Adjust"]]) > 0)
    cat("Adjustment Value:  ",
        x[["Value_Adjust"]], "\n")
  cat("\n")
  cat("General Dominance Values:\n")
  display_std <-
    t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
  dimnames(display_std) <-
    list(names(x[["Ranks"]]), c("General Dominance", "Standardized", "Ranks"))
  print(display_std, ...)
  cat("\n")
  if (length(x[["Conditional_Dominance"]] > 0) && .cdl) {
    cat("Conditional Dominance Values:\n")
    colnames(x[["Conditional_Dominance"]]) <-
      paste("Include At:", seq_len(ncol(x[["Conditional_Dominance"]])))
    print(x[["Conditional_Dominance"]], ...)
    cat("\n")
  }
  if (length(x[["Complete_Dominance"]] > 0) && .cpt) {
    cat("Complete Dominance Proportions:\n")
    colnames(x[["Complete_Dominance"]]) <-
      gsub("^>_", "> ", colnames(x[["Complete_Dominance"]]))
    rownames(x[["Complete_Dominance"]]) <-
      gsub("_>$", " >", rownames(x[["Complete_Dominance"]]))
    print(x[["Complete_Dominance"]])
    cat("\n")
  }
  if (length(x[["Strongest_Dominance"]] > 0) && .cpt && .cdl) {
    cat("Strongest Dominance Designations:")
    print(x[["Strongest_Dominance"]])
    cat("\n")
  }
  invisible(x)
}
#' @title Summary method for `domir`
#' @description Reports dominance designation results from the `domir`
#' class object.
#' @param object an object of class "domir".
#' @param ... further arguments passed to or from other methods.
#' Not used currently.
#' @return The submitted "domir" object with an additional
#' `Strongest_Dominance` element added.
#' \describe{
#'  \item{\code{Strongest_Dominance}}{Matrix comparing the element in the first
#'  row to the element in the third row.  The second row denotes the strongest
#'  designation between the two names.}
#' }
#'
#' @details The summary method for class `domir` objects is used for obtaining
#' the strongest dominance designations (i.e., general, conditional, or
#' complete) among all pairs of dominance analyzed names.
#'
#' @exportS3Method
summary.domir <- function(object, ...) {
  if (length(object[["Strongest_Dominance"]]) == 0) {
    reverse <- as.list(object$Call)$.rev
    if (is.null(reverse)) reverse <- FALSE
    reverse_cdl <-
      ifelse(reverse,
             rep(-1, times = length(object$General_Dominance)),
             rep(1, times = length(object$General_Dominance)))
    reverse_gnl <- ifelse(reverse, -1, 1)
    pairs <- utils::combn(names(object$General_Dominance), 2)
    pairs <- rbind(pairs[1, ], rep("", times = ncol(pairs)), pairs[2, ])
    location <- 0
    for (IV1 in 1:(length(object$General_Dominance) - 1)) {
      for (IV2 in (IV1 + 1):length(object$General_Dominance)) {
        location <- location + 1
        if (length(object[["Complete_Dominance"]] > 0)) {
          if ((object$Complete_Dominance[IV1, IV2] %in% c(0, 1))) {
            pairs[2, location] <-
              ifelse(object$Complete_Dominance[IV1, IV2] == 1,
                     "completely dominates",
                     "is completely dominated by")
            next
          }
        }
        if (length(object[["Conditional_Dominance"]] > 0)) {
          if (all(object$Conditional_Dominance[IV1, ] * reverse_cdl >
                    object$Conditional_Dominance[IV2, ] * reverse_cdl)) {
            pairs[2, location] <- "conditionally dominates"
            next
          } else if (all(object$Conditional_Dominance[IV1, ] * reverse_cdl <
                           object$Conditional_Dominance[IV2, ] * reverse_cdl)) {
            pairs[2, location] <- "is conditionally dominated by"
            next
          }
        }
        pairs[2, location] <-
          ifelse(object$General_Dominance[[IV1]] * reverse_gnl >
                   object$General_Dominance[[IV2]] * reverse_gnl,
                 "generally dominates",
                 ifelse(object$General_Dominance[[IV1]] * reverse_gnl <
                          object$General_Dominance[[IV2]] * reverse_gnl,
                        "is generally dominated by",
                        "has no dominance designation with"))
      }
    }
    rownames(pairs) <- rep("", times = 3)
    colnames(pairs) <- rep("", times = ncol(pairs))
    res <- append(object, list(Strongest_Dominance = pairs))
    class(res) <- c("domir")
    return(res)
  } else {
    return(object)
  }
}
