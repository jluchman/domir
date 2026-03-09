#' @title Dominance analysis methods
#' @name domir
#' @description
#' Parses input object to obtain list of names, determines all required
#' combinations of subsets of the name list, submits name list subsets to
#' a function as the input type, and computes dominance decomposition
#' statistics based on the returned values from the function.
#'
#' @param .obj A `formula` or `formula_list`.
#'
#' Parsed to produce list of names. Combinations of subsets the name list are
#' [`sapply`]-ed to `.fct`.
#'
#' The name list subsets submitted to `.fct` are formatted to be of
#' the same [`class`] as `.obj` and are submitted to
#' `.fct` as the first, unnamed argument.
#'
#' @param .fct A [`function`] or string function name.
#'
#' Applied to all subsets of elements as received from `.obj`.
#' Must return a length 1/scalar, numeric, atomic vector.
#'
#' @param .set A `list`.
#'
#' Must be comprised of elements of the same class as `.obj`.
#' Elements of the list can be named.
#'
#' @param .wst A `list`.
#'
#' Must be comprised of elements of the same class as `.obj`.
#'
#' @param .all A `formula` or `formula_list`.
#'
#' Must be the same class as `.obj`.
#'
#' @param .adj Logical.
#'
#' If `TRUE` then a model including only an intercept is submitted to `.fct`
#' and the value returned is subtracted from the values returned from all
#' subsets in the dominance analysis.
#'
#' @param .cdl `NULL`.
#'
#' Depreciated. Use `print(.cdl = FALSE)` to suppress display of conditional 
#' dominance statistics.
#'
#' @param .cpt `NULL`.
#'
#' Depreciated. Use `print(.cpt = FALSE)` to suppress display of complete 
#' dominance proportions.
#'
#' @param .rev Logical.
#'
#' If `TRUE` then standardized vector, ranks, and complete dominance
#' designations are reversed in their interpretation.
#'
#' @param .cst Object of class c("SOCKcluster", "cluster") from
#' [`parallel-package`].
#'
#' When non-`NULL`, will alter the method for collecting values from all
#' combinations of names from using [`sapply`] to [`parallel::parSapply`].
#'
#' @param .prg Logical.
#'
#' If `TRUE` then a progress bar is displayed during collection of values
#' to indicate progress.
#'
#' @param ... Passes arguments to other methods during method dispatch;
#' passes arguments to the function in `.fct` during function execution.
#'
#' @return Returns an object of [`class`] "domir" composed of:
#' \describe{
#'  \item{`General_Dominance`}{Vector of general dominance values.}
#'  \item{`Standardized`}{Vector of general dominance values normalized
#'  to sum to 1.}
#'  \item{`Ranks`}{Vector of ranks applied to the general dominance values.}
#'  \item{`Conditional_Dominance`}{Matrix of conditional dominance values.
#'  Each row represents an element in `.obj`;
#'  each column represents a number of elements from `.obj` in a subset.}
#'  \item{`Complete_Dominance`}{Matrix of proportions of subsets where the
#'  name in the row has a larger value than the name in the column.
#'  These proportions determine complete dominance when a value of
#'  1 or 0.}
#'  \item{`Value`}{Value returned by `.fct` with all elements (i.e.,
#'  from `.obj`, `.all`, and `.adj`.}
#'  \item{`Value_All`}{Value of `.fct` associated with elements included
#'  in `.all`;
#'  when elements are in `.adj`, will be adjusted for `Value_Adjust`.}
#'  \item{`Value_Adjust`}{Value of `.fct` associated with elements in `.adj`.}
#'  \item{`Call`}{The matched call.}
#' }
#'
#' @details
#' ## Element Parsing
#'
#' `.obj`s is parsed into a name list that is used to determine
#' the required number of combinations of subsets of the name list
#' included the dominance analysis.  How the name list is obtained
#' depends on `.obj`'s class.
#'
#' ### `formula`
#'
#' The `formula` creates a name list using all terms in the formula.
#' The terms are obtained using [`terms.formula`]. All processing
#' that is normally applied to the right hand side of a formula is
#' implemented (see [`formula`]).
#'
#' A response/left hand side is not required but, if present, is
#' included in all `formula`s passed to `.fct`.
#'
#' ### `formula_list`
#'
#' The [`formula_list`] creates a name list out of response-term pairs.
#' The terms are obtained using `terms.formula` applied to each individual
#' formula in the list.
#'
#' ### Additional Details
#'
#' By default, names obtained from `.obj` are all considered separate
#' 'value-generating names' with the same priority.
#' Each value-generating name will be a separate element when
#' computing combination subsets and will be compared to all other
#' value-generating names.
#'
#' `formula`s and `formula_list` elements are assumed to have an intercept
#' except if explicitly removed with a `- 1` in the `formula`(s) in `.obj`.
#' If removed, the intercept will be removed in all `formula`(s) in each
#' `sapply`-ed subset to `.fct`.
#'
#' If [`offset`]s are included, they are passed, like intercepts, while
#' `sapply`-ing subsets to `.fct`.
#'
#' ## Changing Element Parsing
#'
#' All methods' default behavior that considers all value-generating names
#' to be of equal priority can be overriden using `.set`, `.wst`, and `.all`
#' arguments.
#'
#' Names in `.set`, `.wst`, and `.all` must also be present in `.obj`.
#'
#' ### `.set`
#'
#' `.set` binds together value-generating names such that
#' they are of equal priority and are never separated when submitted to
#' `.fct`.
#' Thus, the elements in `.set` bound together contribute jointly to the
#' returned value and are considered, effectively, a single
#' value-generating name.
#'
#' If list elements in `.set` are named, this name will be used in all
#' returned results as the name of the set of value-generating names bound
#' together. Names of `.set`s cannot contain the period character '.'.
#'
#' `.set` thus considers the value-generating names an 'inseparable set' in the
#' dominance analysis and are always included or excluded together 
#' simultaneously.
#' 
#' ### `.wst`
#'
#' `.wst` binds together value-generating names such that
#' they are inseparable when being compared to other value generating names, 
#' but use an additional process that ascribes value to each member of the 
#' set.
#' The elements in `.wst` are bound together and contribute jointly to the
#' returned value and but are not considered a single value generating 
#' name.
#'
#' `.wst` thus considers the value-generating names a 'union' in the
#' dominance analysis which pools their impact on the value and requires that 
#' members of the set are included contiguously but not always together.
#'
#' ### `.all`
#'
#' `.all` gives immediate priority to value-generating names.
#' The value-generating names in `.all` are bound together, are
#' ascribed their full amount of the returned value from `.fct`, and
#' are not adjusted for contribution of other value-generating names.
#'
#' The value of `.fct` ascribed to the value-generating names bound
#' together in `.all` is returned separately from, and not directly
#' compared to, the other value-generating names.
#'
#' The `formula` method for `.all` does not allowthe submitted formula to have
#' a left hand side.
#'
#' `.all` includes the value-generating names in 'all subsets' submitted to
#' the dominance analysis which effectively removes the value associated with
#' this set of names.
#'
#' ### `.adj`
#'
#' `.adj` indicates that an intercept-only model should be supplied to `.fct`.
#' This intercept-only subset is given most immediate priority and the
#' value of `.fct` ascribed to it is removed from all other
#' value-generating names and sets including those in `.all`.
#'
#' The `formula` method will submit an intercept-only formula to `.fct`.
#' The `formula_list` method creates a separate, intercept-only subset for each
#' of the `formula`s in the list.
#' Both the `formula` and `formula_list` methods will respect the user's
#' removal of an intercept and or inclusion of an `offset`.
#'
#' `.adj` then 'adjusts' the returned value for a non-0 value-returning
#' null model when no value generating names are included. This is often
#' useful when a predictive model's fit metric is not 0 when no
#' predictive factors are included in the model.
#'
#' ### Additional Details
#'
#' All methods submit combinations of names as an object of the same class as
#' `.obj`. A `formula` in `.obj` will submit all combinations of names as
#' `formula`s to `.fct`. A `formula_list` in `.obj` will submit all
#' combinations of subsets of names as `formula_list`s to `.fct`.
#' In the case that `.fct` requires a different `class` (e.g.,
#' a character vector of names, a [`Formula::Formula`] see [`fmllst2Fml`]) the
#' subsets of names will have to be processed in `.fct` to obtain the correct
#' `class`.
#'
#' The all subsets of names will be submitted to `.fct` as the first, unnamed
#' argument.
#'
#' ## `.fct` as Analysis Pipeline
#'
#' `.fct` is expected to be a complete analysis pipeline that receives a
#' subset of names of the same `class` as `.obj` and uses these names in the
#' `class` as submitted to generate a returned value of the appropriate
#' type to dominance analyze. Typically, the returned value is a
#' scalar fit statistic/metric extracted from a predictive model.
#'
#' At current, only atomic (i.e., non-`list`), numeric scalars (i.e.,
#' vectors of length 1) are allowed as returned values.
#'
#' The `.fct` argument is strict about names submitted and returned value
#' requirements for functions used. A series of checks to ensure the submitted
#' names and returned value adhere to these requirements.
#' The checks include whether the `.obj` can be submitted to `.fct` without
#' producing an error and whether the returned value from `.fct` is a length 1,
#' atomic, numeric vector.
#' In most circumstances, the user will have to make their own named or
#' anonymous function to supply as `.fct` to satisfy the checks.
#'
#' # Notes
#'
#' ## `formula` method
#'
#' Prior to version 1.1.0, the `formula` method allowed a `formula`
#' to be submitted to `.adj`.
#' Submitting an intercept-only `formula` as opposed to a
#' logical has been depreciated and submitting a `formula` with more than an
#' intercept is defunct.
#'
#' The `formula` and `formula_list` methods can be used to pass responses,
#' intercepts, and `offset`s to all combinations of names.
#' If the user seeks to include other model components integral to
#' estimation
#' (i.e., a random effect term in [`lme4::glmer()`]) include them as
#' [`update`][update.formula] to the submitted `formula` or `formula_list`
#' imbedded in `.fct`.
#'
#' Second-order or higher terms (i.e., interactions like `~ a*b`) are parsed
#' by default but not used differently from first-order terms for producing
#' subsets. The values ascribed to such terms may not be valid unless
#' the user ensures that second-order and
#' higher terms are used appropriately in `.fct`.
#'
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
#'
#' ## Linear model including set
#' domir(
#'   mpg ~ am + vs + cyl + carb + gear + disp + wt,
#'   lm_r2,
#'   .set = list(~ carb + gear, ~ disp + wt),
#'   data = mtcars
#' )
#'
#'
#' ## Multivariate regression with multivariate r-square and
#' ## all subsets variable
#' mlm_rxy <-
#'   function(fml, data, dvnames) {
#'     mlm_res <- lm(fml, data = data)
#'     mlm_pred <- predict(mlm_res)
#'     cancor(mlm_pred, data[dvnames])$cor[[1]]^2
#'   }
#'
#' domir(
#'   cbind(wt, mpg) ~ vs + cyl + am + carb,
#'   mlm_rxy,
#'   .all = ~ carb,
#'   data = mtcars,
#'   dvnames = c("wt", "mpg")
#' )
#'
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
# notes: review below for integration with .wst ----
# notes: use this revision time to simplify functions here? ----
# notes: domir is getting complex ----
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
  # TODO 3: formula_parse in external file as it is used by 'formula_list'? ----
  # ? or ? remove '@export' and call it in formula list with ':::'?
  fml_parsed <- formula_parse(.obj)
  #print(fml_parsed) # ~~
  if (length(fml_parsed$rhs_names) == 0)
    stop("The formula in '.obj' must have one or more terms.", call. = FALSE)
  entire_namelist_value <- formula_output_check(.obj, .fct, TRUE, ...)
  print(entire_namelist_value) # ~~
  adj_checker(.adj, fml_parsed, TRUE)
  adj_value <- est_adj_value(.adj, fml_parsed, .fct, TRUE, ...)
  print(adj_value) # ~~
  all_parsed <- all_checker(.all, fml_parsed, TRUE)
  fml_parsed <- fml_all_update(all_parsed, fml_parsed, TRUE)
  all_value <- est_all_value(.all, fml_parsed, .fct, adj_value, TRUE, ...)
  print(all_value) # ~~
  set_checker(.set, fml_parsed, "'.set'", TRUE)
  sets_parsed <- set_element_checker(.set, fml_parsed, "'.set'", TRUE)
  print(sets_parsed) # ~~
  set_checker(.wst, fml_parsed, "'.wst'", TRUE)
  wsts_parsed <- set_element_checker(.wst, fml_parsed, "'.wst'", TRUE)
  print(wsts_parsed) # ~~
  check_namelists(fml_parsed, sets_parsed, wsts_parsed, all_parsed, TRUE) # updating here
  stop("That's it!")
  names_for_dominance <- determine_dominance_names(fml_parsed, .set, .wst)
  return_list <-
    dominance_scalar2(
      fml_parsed, .fct, names_for_dominance, entire_namelist_value, 
      adj_value, all_value, #.cdl, .cpt, 
      .rev, .cst, .prg, list(...))
  print(names_for_dominance) # ~~
  names_for_printing <- determine_display_names(names_for_dominance, .set)
  print(names_for_printing) # ~~
  return_list <- name_return_list(return_list, names_for_printing)
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
  rhs_term_counts <- sapply(fmllst_parsed, function(elem) length(elem$rhs_names))
  if (any(rhs_term_counts == 0)) {
    stop(
      paste("Each formula in '.obj' must have one or more terms.",
            "Formulas", paste(which(rhs_term_counts == 0), collapse = " "),
            "have no terms."),
      call. = FALSE
      )
  }
  entire_namelist_value <- formula_output_check(.obj, .fct, FALSE, ...)
  print(entire_namelist_value) # ~~
  adj_checker(.adj, fmllst_parsed, FALSE)
  adj_value <- est_adj_value(.adj, fmllst_parsed, .fct, FALSE, ...)
  print(adj_value) # ~~
  all_parsed <- all_checker(.all, fmllst_parsed, FALSE)
  print(all_parsed) # ~~
  fmllst_parsed <- fml_all_update(all_parsed, fmllst_parsed, FALSE)
  all_value <- est_all_value(.all, fmllst_parsed, .fct, adj_value, FALSE, ...)   
  print(all_value) # ~~
  set_checker(.set, fmllst_parsed, "'.set'", FALSE)
  set_element_checker(.set, fmllst_parsed, "'.set'", FALSE)
  set_checker(.wst, fmllst_parsed, "'.wst'", FALSE)
  set_element_checker(.wst, fmllst_parsed, "'.wst'", FALSE)
  check_namelists(fmllst_parsed, .set, .wst, .all, FALSE) # updating here - second
  stop("That's it!")
  # subset adjustment and check ----
  # adjust subsets for '.set' or '.all' names
  if (!is.null(selector_locations_sets) || !is.null(all_model)) {
    selector_locations_sets_full <-
      lapply(
        selector_locations_sets,
        function(elem) {
          lapply(unlist(elem), function(loc) unlist(selector_locations[loc]))
        }
      )
    selector_locations <- selector_locations[!rmv_frm_subst]
    selector_locations <-
      append(selector_locations, selector_locations_sets_full)
  }
  # check number of subsets
  if (length(selector_locations) < 2)
    stop("At least two names or sets of names are needed ",
         "for a dominance analysis.",
         call. = FALSE)
  # define meta-function to coordinate .fct calls ----
  meta_domir_fml_lst <-
    function(Selector_lgl,
             list_parsed, .fct, selector_locations,
             args_2_fct, ...) {
      # determine which pairs are to be included;
      # note the loop over a list of locations when sets are included
      for (elem in selector_locations[Selector_lgl]) {
        if (is.list(elem)) {
          for (pos in seq_len(length(elem))) {
            list_parsed[[elem[[pos]]]] <- TRUE
          }
        } else {
          list_parsed[[elem]] <- TRUE
        }
      }
      # reconstruct the selected formula_list
      # Note the extra condition in case one formula is empty in a
      # specific subset
      fml_lst <-
        lapply(
          list_parsed,
          function(elem) {
            if (all(!elem$select_lgl)) {
              stats::reformulate(
                c("1", elem$offset),
                response = elem$lhs_names,
                intercept = elem$intercept_lgl)
            } else {
              stats::reformulate(
                c(elem$rhs_names[elem$select_lgl], elem$offset),
                response = elem$lhs_names,
                intercept = elem$intercept_lgl)
            }
          }
        )
      class(fml_lst) <- c("formula_list", "list")
      # submit formula_list to '.fct'
      do.call(.fct, append(list(fml_lst), args_2_fct))
    }
  # define arguments to `dominance_scalar` ----
  args_list <-
    list(RHS = selector_locations,
         list_parsed = list_parsed,
         .fct = .fct,
         selector_locations = selector_locations,
         .all = all_model, .adj = adj_model,
         args_2_fct = list(...))
  return_list <-
    dominance_scalar(
      meta_domir_fml_lst,
      args_list, full_model,
      .cdl, .cpt, .rev,
      .cst, .prg)
  # finalize returned values and attributes ----
  if (is.null(.set)) {
    IV_Labels <- lhs_rhs_pairlist[!rmv_frm_subst]
  } else {
    IV_Labels <- c(lhs_rhs_pairlist[!rmv_frm_subst], set_labels)
  }
  names(return_list$General_Dominance) <- IV_Labels
  names(return_list$General_Dominance_Ranks) <- IV_Labels
  if (.cdl)
    dimnames(return_list$Conditional_Dominance) <-
    list(names(return_list$General_Dominance),
         paste0("include_at_", seq_len(length(return_list$General_Dominance))))
  if (.cpt)
    dimnames(return_list$Complete_Dominance) <-
    list(paste0(names(return_list$General_Dominance), "_>"),
         paste0(">_", names(return_list$General_Dominance)))

  if (!.rev) {
    Standardized <-
      return_list$General_Dominance /
      (return_list$Value -
         ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0))
  } else {
    Standardized <-
      -return_list$General_Dominance /
      -(return_list$Value -
          ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0))
  }
  return_list <- list(
    "General_Dominance" = return_list$General_Dominance,
    "Standardized" = Standardized,
    "Ranks" = return_list$General_Dominance_Ranks,
    "Complete_Dominance" = return_list$Complete_Dominance,
    "Conditional_Dominance" = return_list$Conditional_Dominance,
    "Value" = return_list$Value,
    "Value_All" =
      return_list$All_result -
      ifelse(is.null(return_list$Adj_result), 0, return_list$Adj_result),
    "Value_Adjust" = return_list$Adj_result,
    "Call" = match.call()
  )
  # apply class 'domir'
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
#' @export
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
  return(test_model)
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
      warning("'.cpt' and '.cdl' are depreciated as arguments to 'domir' as of ", 
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
    stop("'.adj' cannot be estimated with intercepts removed from '.obj'.", call. = FALSE)
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
            paste(as.character(elem$lhs_names), "~", 
                  elem$rhs_names)), NULL),
      Reduce(
        union, 
        sapply(
          fml_parsed, 
          function(elem) 
            paste(as.character(elem$lhs_names), "~", 
                  elem$rhs_names)), NULL)
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
                nchar(as.character(all_parsed[[1]]$lhs_names)) + 4, 
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
          unlist(sapply(all_parsed, function(el) as.character(el$lhs_names)))
        pairs_all <- 
          Reduce(
            union, 
            sapply(
              all_parsed, 
              function(el) 
                paste(as.character(el$lhs_names), "~", 
                      el$rhs_names)), NULL)
        if (as.character(elem$lhs_names) %in% lhs_names_all) {
          iv_dv_pairs <- 
            paste(as.character(elem$lhs_names), "~", elem$rhs_names)
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
          stats::reformulate(
            c(fml$rhs_names[fml$select_lgl], fml$offset),
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
    set_names <- set_labeller2(.set, FALSE)
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
      if (!is.null(fml_parsed)) fml_parsed <- list(fml_parsed)
      if (!is.null(sets_parsed)) 
        sets_parsed <- 
          lapply(sets_parsed, function(elem) list(elem))
      if (!is.null(wsts_parsed)) 
        wsts_parsed <- 
          lapply(wsts_parsed, function(elem) list(elem))
      if (!is.null(all_parsed)) all_parsed <- list(all_parsed)
    }
    namelist <- lapply(fml_parsed, function(elem) elem$rhs_names)
    sets_namelists <- 
      lapply(sets_parsed, function(elem) lapply(elem, function(el) el$rhs_names))
    wsts_namelists <- 
      lapply(wsts_parsed, function(elem) lapply(elem, function(el) el$rhs_names))
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
    NULL
  }

#' TBD
#' @noRd
set_labeller2 <- function(.set, .is_wst) {
  if (is.null(.set)) return(NULL)
  name_type <- ifelse(.is_wst, "wst", "set")
  if (is.null(names(.set))) {
    set_labels <- paste(name_type, seq_len(length(.set)))
  } else {
    set_labels <- names(.set)
  }
  missing_set_labels <- which(set_labels == "")
  if (length(missing_set_labels) > 0)
    set_labels[missing_set_labels] <- paste("set", missing_set_labels, sep = "")
  set_labels
}
#' TBD
#' @noRd
#' report_duplicates <- function(list1, list2, name1, name2) {
#'   dups <- intersect(list1, list2)
#'   ifelse(
#'     length(dups) > 0,
#'     paste(
#'       paste(dups, collapse = ", "), 
#'       "; from ", name1, " and ", name2, sep = ""
#'     ),
#'     ""
#'   )
#' }
#' TBD
#' @noRd
#' report_singletons <- function(list1, list2, name) {
#'   single <- setdiff(list1, list2)
#'   ifelse(
#'     length(single) > 0,
#'     paste(
#'       paste(single, collapse = ", "), 
#'       "; from ", name, sep = ""
#'     ),
#'     ""
#'   )
#' }
#' TBD
#' @noRd
determine_dominance_names <- function(fml_parsed, .set, .wst) {
  # prep inputs
  namelist <- fml_parsed$rhs_names[!fml_parsed$select_lgl]
  if (is.null(.set)) {
    sets_namelists <- NULL
  } else {
    sets_parsed <- lapply(.set, formula_parse)
    sets_namelists <- lapply(sets_parsed, function(elem) {elem$rhs_names})
    set_names <- paste("set", seq_len(length(.set)), sep = "")
    names(sets_namelists) <- set_names
  }
  if (is.null(.wst)) {
    wsts_namelists <- NULL
  } else {
    wsts_parsed <- lapply(.wst, formula_parse)
    wsts_namelists <- lapply(wsts_parsed, function(elem) {elem$rhs_names})
    wst_names <- paste("wst", seq_len(length(.wst)), sep = "")
    names(wsts_namelists) <- wst_names
  }
  # construct namelist
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
#' TBD
#' @noRd
determine_display_names <- 
  function(namelist, .set) {
    regular_names <- namelist[grepl("var", names(namelist))]
    set_names <- set_labeller2(.set)
    wst_names <- namelist[grepl("wst", names(namelist))]
    c(unlist(regular_names), set_names, unlist(wst_names))
  }
#' TBD
# meta_domir_fml2 <- #update name eventually
#   function(submodel_names_lgl, namelist, fml_parsed, .fct, args_2_fct) {
#     for (elem in namelist[submodel_names_lgl]) {
#       fml_parsed$select_lgl[elem] <- TRUE
#     }
#     fml <-
#       stats::reformulate(
#         c(fml_parsed$rhs_names[fml_parsed$select_lgl], fml_parsed$offset),
#         response = fml_parsed$lhs_names,
#         intercept = fml_parsed$intercept_lgl
#       )
#     do.call(.fct, append(list(fml), args_2_fct))
#   }
#' TBD
#' @noRd
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








# ---- old helpers ----
# adjustment to returned value function ----
est_adj_model <- function(fml_prs, .fct, .adj, .sty, ...) {
  if (!is.logical(.adj) || (length(.adj) > 1))
    stop("'.adj' argument must be logical of length 1.", call. = FALSE)
  if (!.adj) return(NULL)
  need_inter <-
    switch(
      .sty,
      formula = !fml_prs$intercept,
      formula_list = !all(sapply(fml_prs, function(elem) elem$intercept))
    )
  if (need_inter) 
    stop("'.adj' cannot be estimated with removed intercepts.", call. = FALSE)
  adj_fml <-
    switch(
      .sty,
      formula =
        stats::reformulate(
          c("1", fml_prs$offset),
          response = fml_prs$lhs_names,
          intercept = fml_prs$intercept_lgl
        ),
      formula_list =
        ( function() {
          adj_fml_lst <-
            lapply(
              fml_prs,
              function(elem) {
                stats::reformulate(
                  c("1", fml_prs$offset),
                  response = elem$lhs_names,
                  intercept = elem$intercept_lgl
                )
              }
            )
      class(adj_fml_lst) <- c("formula_list", "list")
      adj_fml_lst
        })()
  )
formula_output_check(adj_fml, .fct, ...)
}
# all subsets value ----
est_all_model <-
  function(fml_prs, .fct, .all, .sty, lhs_rhs, sel_loc, ...) {
    if (is.null(.all)) return(NULL)
    all_prs <-
      switch(
        .sty,
        formula = 
          ( function() {
            if (!inherits(.all, "formula"))
              stop("'.all' must be a 'formula'.", call. = FALSE)
            formula_parse(.all)
          })(),
        formula_list = 
          ( function() {
            if (!inherits(.all, "formula_list"))
              stop("'.all' must be a 'formula_list'.", call. = FALSE)
            lapply(.all, formula_parse)
          })()
      )
    chk_all_fml <-
      function(.all, .sty) {
        # empty formulas check
        if (length(.all$rhs_names) == 0)
          stop("'.all' must have one or more terms.", call. = FALSE)
        # no lhs in '.all' check
        if (!is.null(.all$lhs_names) && .sty == "formula")
          stop("Left hand side names not allowed in '.all' formula.",
               call. = FALSE)
        # no offsets in '.all' check
        if (!is.null(.all$offset))
          stop("Offsets not allowed in '.all'.", call. = FALSE)
        # no removed intercepts in '.all' check
        if (!.all$intercept_lgl)
          stop("Removing intercepts not allowed in '.all'.", call. = FALSE)
      }
    switch(
      .sty,
      formula =  chk_all_fml(all_prs, "formula"),
      formula_list = lapply(all_prs, chk_all_fml, "formula_list")
    )
    # names in '.all' are in '.obj' check
    valid_all_lgl <-
      switch(
        .sty,
        formula = all_prs$rhs_names %in% fml_prs$rhs_names,
        formula_list =
          ( function() {
            all_pairs <-
              lapply(
                all_prs,
                function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
              )
            (unlist(all_pairs) %in% lhs_rhs)
          })()
      )
    if (!all(valid_all_lgl)) {
      bad_all_names <-
        switch(
          .sty,
          formula = paste(all_prs$rhs_names[!valid_all_lgl], collapse = " "),
          formula_list =
            ( function() {
              all_pairs <-
                lapply(
                  all_prs,
                  function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
                )
              paste(unlist(all_pairs)[!valid_all_lgl], collapse = " ")
            })
        )
      stop("Name(s) ", bad_all_names, " in '.all' not found on in '.obj'.",
           call. = FALSE)
    }
    all_fml <-
      switch(
        .sty,
        formula =
          stats::reformulate(
            c(all_prs$rhs_names, fml_prs$offset),
            response = fml_prs$lhs_names,
            intercept = fml_prs$intercept_lgl),
        formula_list =
          ( function() {
            adj_fml_prs <- fml_prs
            all_pairs <-
              lapply(
                all_prs,
                function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
              )
            for (loc in which(lhs_rhs %in% unlist(all_pairs))) {
              adj_fml_prs[[sel_loc[[loc]]]] <- TRUE
            }
            all_fml_lst <-
              lapply(
                adj_fml_prs,
                function(fml) {
                  if (any(fml$select_lgl)) {
                    stats::reformulate(
                      c(fml$rhs_names[fml$select_lgl], fml$offset),
                      response = fml$lhs_names,
                      intercept = fml$intercept_lgl)
                  } else {
                    stats::reformulate(
                      c("1", fml$offset),
                      response = fml$lhs_names,
                      intercept = fml$intercept_lgl)
                  }
                }
              )
            class(all_fml_lst) <- c("formula_list", "list")
            all_fml_lst
          }
          )()
      )
    formula_output_check(all_fml, .fct, ...)
  }
# process sets ----
proc_set_fml <- function(fml_prs, .set, .sty, lhs_rhs, .stp) {
  if (is.null(.set)) return(NULL)
  # check '.set' is a list
  if (!is.list(.set)) 
    stop("'", .stp, "' must be a list.", call. = FALSE)
  # check '.set' is not an empty list
  if (length(.set) == 0) 
    stop("'", .stp, "' is empty.", call. = FALSE)
  # valid formulas in '.set' check
  not_fmls <-
    switch(
      .sty,
      formula = sapply(.set, function(elem) {!inherits(elem, "formula")}),
      formula_list = 
        sapply(
          .set, 
          function(elem) {
            !inherits(elem, "formula_list") || (length(elem) == 0) 
          }
        )
    )
  if (any(not_fmls)) {
    which_not_fml <- seq_len(length(not_fmls))[not_fmls]
    stop("List element(s) ",paste(which_not_fml, collapse = " "), " of '",
         .stp, "' are not valid '", .sty, "'s.",
         call. = FALSE)
  }
  # obtain RHS, LHS, intercept, and offsets from '.set's
  sets_prs <-
    switch(
      .sty,
      formula = lapply(.set, formula_parse),
      formula_list = lapply(.set, function(elem) {lapply(elem, formula_parse)})
    )
  # > 0 terms in '.set' check
  chk_set_fml <- function(sets_prs, fml_prs) { # consider unifying checks for '.all' and '.set' formulas
    rhs_counts_sets <-
      sapply(sets_prs, function(elem) length(elem$rhs_names) == 0)
    if (any(rhs_counts_sets)) {
      which_int_only <- seq_len(length(rhs_counts_sets))[rhs_counts_sets]
      stop("Formulas in '", .stp, "' must have one or more terms. ",
           "Formula(s) in list element(s) ",
           paste(which_int_only, collapse = " "),
           " have no terms.",
           call. = FALSE)
    }
    # no lhs in '.set' check
    set_lhs <- sapply(sets_prs, function(elem) length(elem$lhs_names) == 0)
    if (!all(set_lhs) && .sty == "formula") {
      which_have_lhs <- seq_len(length(set_lhs))[!set_lhs]
      stop("Left hand sides not allowed in '", .stp, "'. ",
           "Formula(s) in list element(s) ",
           paste(which_have_lhs, collapse = " "),
           " have left hand sides.",
           call. = FALSE)
    }
    #  no offsets in '.set' check
    set_offset <- sapply(sets_prs, function(elem) is.null(elem$offset))
    if (!all(set_offset)) {
      which_have_offset <- seq_len(length(set_offset))[!set_offset]
      stop("Offsets not allowed in '", .stp, "'. ",
           "Formula(s) in list element(s) ",
           paste(which_have_offset, collapse = " "),
           " have offsets.",
           call. = FALSE)
    }
    # no removed intercepts in '.set' check
    set_intercept <- sapply(sets_prs, function(elem) elem$intercept_lgl)
    if (!all(set_intercept)) {
      which_rmv_intercept <- seq_len(length(set_intercept))[!set_intercept]
      stop("Removing intercepts not allowed in '", .stp, "'. ",
           "Formula(s) in list element(s) ",
           paste(which_rmv_intercept, collapse = " "),
           " remove the intercept.",
           call. = FALSE)
    }
  }
  switch(
    .sty,
    formula = chk_set_fml(sets_prs),
    formula_list =
      ( function() {
        if (!is.list(.set)) stop("'", .stp, "' must be a 'list'.", 
                                 call. = FALSE)
        lapply(sets_prs, chk_set_fml)
      })()
  )
  # no duplicated names in '.set' check
  mk_nm_mat <-
    function(elem, lst, sty) {
      names <-
        switch(
          sty,
          formula = lst[[elem]]$rhs_names,
          formula_list =
            paste0(lst[[elem]]$lhs_names, "~", lst[[elem]]$rhs_names)
        )
      reps <- rep(elem, times = length(names))
      comb_vec <- c(reps, names)
      matrix(comb_vec, nrow = 2, byrow = TRUE)
    }
  set_dups <-
    switch(
      .sty,
      formula =
        lapply(
          seq_len(length(sets_prs)),
          mk_nm_mat,
          lst = sets_prs, sty = "formula"
        ),
      formula_list =
        lapply(
          sets_prs,
          function(elem) {
            lapply(
              seq_len(length(elem)),
              mk_nm_mat,
              lst = elem, sty = "formula_list"
            )
          }
        )
    )
  set_dups <- as.data.frame(set_dups)
  which_dups <- duplicated(unlist(set_dups[2, ]))
  if (any(which_dups)) {
    dups <- unique(unlist(set_dups[2, which_dups])) # unique() here as, when there are 3 duplicates, reporting on duplicates is repeated
    report_dups <-
      function(name) {
        which_have_name <- which(unlist(set_dups[2, ]) == name)
        fmls <- paste(unlist(set_dups[1, which_have_name]), collapse = " ")
        paste(fmls, "contain", name)
      }
    dups_report <- sapply(dups, report_dups, USE.NAMES = FALSE)
    stop("Names are duplicated across formulas in '", .stp, "'. ",
         paste(dups_report, collapse = ", "), ".",
         call. = FALSE)
  }
  # names in '.set' match names in '.obj' check
  which_valid_names <-
    switch(
      .sty,
      formula =
        lapply(
          sets_prs,
          function(elem, rhs) elem$rhs_names %in% rhs,
          rhs = fml_prs$rhs_names
        ),
      formula_list =
        lapply(
          sets_prs,
          function(set) {
            unlist(
              lapply(
                set,
                function(elem, rhs) {
                  paste0(elem$lhs_names, "~", elem$rhs_names) %in% rhs
                },
                rhs = lhs_rhs
              )
            )
          }
        )
    )
  if (!all(unlist(which_valid_names))) {
    set_names <-
      switch(
        .sty,
        formula = lapply(sets_prs, function(elem) elem$rhs_names),
        formula_list =
          lapply(
            sets_prs,
            function(set) {
              unlist(
                lapply(
                  set,
                  function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
                )
              )
            }
          )
      )
    elem_rpt <- !sapply(which_valid_names, all)
    valid_report <-
      sapply(
        seq_len(length(sets_prs))[elem_rpt],
        function(elem) {
          bad_names <- set_names[[elem]][!which_valid_names[[elem]]]
          bad_names <- paste(bad_names, collapse =)
          paste(c(bad_names, " in '", .sty, "' element ", elem), collapse = "")
        }
      )
    stop("Names in '", .stp, "' are not present in '.obj.' ",
         paste(valid_report, collapse = ", "), ".",
         call. = FALSE)
  }
  # group together locations of the .$select_lgl elements for '.set's
  switch(
    .sty,
    formula =
      lapply(sets_prs,
             function(elem) which(fml_prs$rhs_names %in% elem$rhs_names)),
    formula_list =
      lapply(
        sets_prs,
        function(set) {
          lapply(
            set,
            function(elem) {
              which(lhs_rhs %in%  paste0(elem$lhs_names, "~", elem$rhs_names))
            }
          )
        }
      )
  )
}
# '.set' labelling function
set_labeller <- function(.set, rhs_names) {
  if (is.null(.set)) return(NULL)
  if (is.null(names(.set))) {
    set_labels <- paste0("set", seq_len(length(.set)))
  } else {
    set_labels <- names(.set)
  }
  missing_set_labels <- which(set_labels == "")
  if (length(missing_set_labels) > 0)
    set_labels[missing_set_labels] <- paste0("set", missing_set_labels)
  repeat_names <- set_labels[which(set_labels %in% rhs_names)]
  if (length(repeat_names) > 0)
    stop("Formula names ",
         paste(repeat_names, collapse = " "),
         " are also names in '.obj'.\n",
         "These formulas in '.set' must be renamed.",
         call. = FALSE)
  set_labels
}
#' @title Print method for `domir`
#' @description Reports formatted results from `domir` class object.
#' @param x an object of class "domir".
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
#'  \item{Matrix describing the conditional dominance values, if computed}
#'  \item{Matrix describing the complete dominance designations, if evaluated}
#'  \item{If following [`summary.domir`], matrix describing the strongest
#'  dominance designations between all elements.}}
#'
#'  The `domir` print method alters dimension names for readability and they
#'  do not display as stored in the `domir` object.
#'
#' @exportS3Method
# `domir` methods ----
print.domir <- function(x, ...) {
  if (!is.null(as.list(x$Call)$.prg))
    switch(
      as.character(as.logical(deparse(as.list(x$Call)$.prg))),
      `TRUE` = cat("\n"),
      `FALSE` = NULL
    )
  cat("Overall Value:     ", x[["Value"]], "\n")
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
  if (length(x[["Conditional_Dominance"]] > 0)) {
    cat("Conditional Dominance Values:\n")
    colnames(x[["Conditional_Dominance"]]) <-
      paste("Include At:", seq_len(ncol(x[["Conditional_Dominance"]])))
    print(x[["Conditional_Dominance"]], ...)
    cat("\n")
  }
  if (length(x[["Complete_Dominance"]] > 0)) {
    cat("Complete Dominance Proportions:\n")
    colnames(x[["Complete_Dominance"]]) <-
      gsub("^>_", "> ", colnames(x[["Complete_Dominance"]]))
    rownames(x[["Complete_Dominance"]]) <-
      gsub("_>$", " >", rownames(x[["Complete_Dominance"]]))
    print(x[["Complete_Dominance"]])
    cat("\n")
  }
  if (length(x[["Strongest_Dominance"]] > 0)) {
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
#'  designation between the two elements.}
#' }
#'
#' @details The summary method for class `domir` objects is used for obtaining
#' the strongest dominance designations (i.e., general, conditional, or
#' complete) among all pairs of dominance analyzed elements.
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
