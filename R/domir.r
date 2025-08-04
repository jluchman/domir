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
#' @param .wst Not yet used.
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
#' @param .cdl Logical.
#'
#' If `FALSE` then conditional dominance matrix is not computed.
#'
#' @param .cpt Logical.
#'
#' If `FALSE` then complete dominance matrix is not computed.
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
#'  The se proportions determine complete dominance when a value of
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
#' to be of equal priority can be overriden using `.set` and `.all` arguments.
#'
#' Names in `.set` and `.all` must also be present in `.obj`.
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
#' together.
#'
#' `.set` thus considers the value-generating names an 'inseparable set' in the
#' dominance analysis and are always included or excluded together.
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
domir <- function(.obj, ...) { # increment to 2.0? or just 1.3?
  UseMethod("domir")
}
#' @rdname domir
#' @exportS3Method
domir.formula <- function(
    .obj, .fct,
    .set = NULL, .wst = NULL,
    .all = NULL, .adj = FALSE,
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, # depreciate '.cdl' and '.cpt' - move to print method
    .cst = NULL, .prg = FALSE, ...) {
  # process arguments and prepare for sub-model estimation ----
  domir_arg_checker(.wst, .rev, .cpt, .cdl, .prg, .cst)
  # TODO 3: formula_parse in external file ----
  fml_parsed <- formula_parse(.obj)
  if (length(fml_parsed$rhs_names) == 0)
    stop("The formula in '.obj' must have one or more terms.", call. = FALSE)
  entire_namelist_value <- formula_output_check(.obj, .fct, ...)
  print(entire_namelist_value) # ~~
  fml_adj_checker(.adj, fml_parsed)
  adj_value <- est_adj_value(.adj, fml_parsed, .fct, ...)
  print(adj_value) # ~~
  fml_parsed <- fml_all_update(.all, fml_parsed)
  all_value <- est_all_value(.all, fml_parsed, .fct, adj_value, ...)
  print(all_value) # ~~
  fml_set_checker(.set, fml_parsed, "'.set'")
  fml_set_checker(.wst, fml_parsed, "'.wst'")
  check_namelists(fml_parsed, .set, .wst, .all)
  names_for_dominance <- determine_dominance_names(fml_parsed, .set, .wst)
  return_list <-
    dominance_scalar2(
      fml_parsed, .fct, names_for_dominance, entire_namelist_value, 
      adj_value, all_value, .cdl, .cpt, .rev, .cst, .prg, list(...))
  # ended here: 8/4 ----
  # link return list to reporting - redo or comment all between there an here out?
  stop("sorry, that's it!", call. = FALSE)
  # ---- ended here: 8/4 ----
  
  
  # ~~ take processes involving this and make them into function
  keep_in_nmlst_lgl <- rep(TRUE, times = length(fml_parsed$rhs_names))
  # create vector of 'select_lgl' locations;
  # used as a convenience to select elements from this list that
  # can contain 1 or more 'select_lgl' locations
  selector_locations <- lapply(which(!fml_parsed$select_lgl), invisible)
  adj_value <- est_adj_model(fml_parsed, .fct, .adj, "formula", ...)
  all_value <-
    est_all_model(fml_parsed, .fct, .all, "formula",
                  NULL, selector_locations, ...)
  # activate names in '.all' as TRUE in 'select_lgl';
  # indicate that they are to be removed in name removal list
  pos_all_name <- which(fml_parsed$rhs_names %in% formula_parse(.all)$rhs_names)
  fml_parsed$select_lgl[pos_all_name] <- TRUE
  keep_in_nmlst_lgl[pos_all_name] <- FALSE
  # process '.set' ----
  # indicate that names in '.set' are to be removed in name removal list
  selector_locations_sets <- 
    proc_set_fml(fml_parsed, .set, "formula", NULL, ".set")
  keep_in_nmlst_lgl[unlist(selector_locations_sets)] <- FALSE
  # apply labels to '.set'
  set_labels <- set_labeller(.set, fml_parsed$rhs_names)
  # process '.wst' ----
  # indicate that names in '.wst' are to be removed in name removal list
  selector_locations_wsts <- 
    proc_set_fml(fml_parsed, .wst, "formula", NULL, ".wst")
  keep_in_nmlst_lgl[unlist(selector_locations_wsts)] <- FALSE
  # subset adjustment and check ----
  # use name removal list to pare down selector_location list;
  # add '.set's and '.wst's as selector locations
  if (!is.null(selector_locations_sets) || !is.null(all_value) || 
      !is.null(selector_locations_wsts)) {
    selector_locations <- selector_locations[keep_in_nmlst_lgl]
    selector_locations <- append(selector_locations, selector_locations_sets)
    selector_locations <- append(selector_locations, selector_locations_wsts)
  }
  # flag locations of .'wst's within selector_location list
  if (is.null(selector_locations_wsts)) {
    which_wsts <- NULL
  } else {
    which_wsts <-
      ((length(selector_locations)-length(selector_locations_wsts))+1):
      length(selector_locations)
  }
  # check number of subsets
  if ((length(selector_locations) < 2) &&
      ifelse(is.null(selector_locations_wsts), 
             TRUE, sum(sapply(selector_locations_wsts, length)) < 2))
    stop("At least two names, within-set names, or sets of names are needed ",
         "for a dominance analysis.",
         call. = FALSE)
  # define formula-based meta-function to coordinate .fct calls ----
  meta_domir_fml <-
    function(Selector_lgl, fml_parsed, .fct, RHS, args_2_fct, ...) {
      # indicate which names have been selected for inclusion
      # by 'Selector_lgl' implemented/passed by `dominance_scalar()`
      for (elem in RHS[Selector_lgl]) {
        fml_parsed$select_lgl[elem] <- TRUE
      }
      # reconstruct the formula with selected names for application to
      # value generating function
      fml <-
        stats::reformulate(
          c(fml_parsed$rhs_names[fml_parsed$select_lgl], fml_parsed$offset),
          response = fml_parsed$lhs_names,
          intercept = fml_parsed$intercept_lgl
        )
      # submit formula_list to '.fct'
      do.call(.fct, append(list(fml), args_2_fct))
    }
  # define arguments to `dominance_scalar` ----
  # TODO 2: restructure 'args_list' into directly submitted args to 'dominance _scalar()' ----
  args_list <-
    list(RHS = selector_locations,
         fml_parsed = fml_parsed,
         .fct = .fct,
         .all = all_value, .adj = adj_value,
         .wst = which_wsts,
         args_2_fct = list(...))
  # implement dominance analysis
  return_list <-
    dominance_scalar(meta_domir_fml, args_list, entire_namelist_value, 
                     .cdl, .cpt, .rev, .cst, .prg)
  # finalize returned values and attributes ----
  if (is.null(.set)) {
    IV_labels <- fml_parsed$rhs_names[keep_in_nmlst_lgl]
  } else {
    IV_labels <- c(fml_parsed$rhs_names[keep_in_nmlst_lgl], set_labels)
  }
  # notes: !! the below is a patch, needs reformat for both sets and wsets !! ----
  # begin
  if (!is.null(.wst)) {
    wst_names <- fml_parsed$rhs_names[unlist(selector_locations_wsts)]
    IV_labels <- c(fml_parsed$rhs_names[keep_in_nmlst_lgl], wst_names)
  }
  # end
  names(return_list$General_Dominance) <- IV_labels
  names(return_list$General_Dominance_Ranks) <- IV_labels
  if (.cdl) {
    dimnames(return_list$Conditional_Dominance) <-
      list(names(return_list$General_Dominance),
           paste0("include_at_",
                  seq_len(ncol(return_list$Conditional_Dominance))))
  }
  if (.cpt) {
    dimnames(return_list$Complete_Dominance) <-
    list(paste0(names(return_list$General_Dominance), "_>"),
         paste0(">_", names(return_list$General_Dominance)))
  }
  if (!.rev) {
    Standardized <-
      return_list$General_Dominance /
      (
        return_list$Value -
          ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0)
      )
  } else {
    Standardized <-
      -return_list$General_Dominance /
      -(
        return_list$Value -
          ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0)
      )
  }
  # apply class and return
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
  class(return_list) <- c("domir")
  return_list
}

#' @rdname domir
#' @exportS3Method
domir.formula_list <- function(
    .obj, .fct,
    .set = NULL, .wst = NULL, .all = NULL, .adj = FALSE,
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE,
    .cst = NULL, .prg = FALSE, ...) {
  # !! documentation for this function only focuses on key differences
  # from 'formula'-based domir; most differences are in applying same
  # processes to a list of formulas as opposed to an individual formula !!
  # check domir arguments ----
  domir_arg_checker(.wst, .rev, .cpt, .cdl, .prg, .cst)
  # process and check 'formula_list' ----
  list_parsed <- lapply(.obj, formula_parse)
  # no empty formulas check
  rhs_term_counts <- sapply(list_parsed, function(elem) length(elem$rhs_names))
  if (any(rhs_term_counts == 0)) {
    stop(
      paste("Each formula in '.obj' must have one or more terms.",
            "Formulas", paste(which(rhs_term_counts == 0), collapse = " "),
            "have no terms."),
      call. = FALSE
      )
  }
  # this lhs ~ rhs pair list is generated for when printing 'formula_list'
  # results and for checking pairs against '.set' and '.all' pairs
  lhs_rhs_pairlist <-
    unlist(
      lapply(
        list_parsed,
        function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
        )
      )
  # note the use of the 'lhs_rhs_pairlist' for the removal list
  rmv_frm_subst <- rep(FALSE, times = length(lhs_rhs_pairlist))
  # confirm .fct works as applied to .obj and return all names value
  full_model <- formula_output_check(.obj, .fct, ...)
  # selector locations for 'formula_list' is more complex than for
  # 'formula' as it includes 3 location entries;
  # the three entries in the selector locations below are the indexes for
  # (formula/lhs, select_lgl, rhs_term); this combination of 3 indexes is
  # convenient for selecting a single pair in the more nested/complex
  # 'formula_list' structure using the same 'Select_lgl' generated by
  # `dominance_scalar()`
  selector_locations <- vector(mode = "list", length = sum(rhs_term_counts))
  pos <- 1
  for (elem in seq_len(length(list_parsed))) {
    for (loc in seq_len(rhs_term_counts[[elem]])) {
      selector_locations[[pos]] <- c(elem, 5, loc)
      pos <- pos + 1
    }
  }
  # estimate '.adj' value ----
  adj_model <- est_adj_model(list_parsed, .fct, .adj, "formula_list", ...)
  # estimate '.all' value ----
  all_model <-
    est_all_model(list_parsed, .fct, .all, "formula_list",
                  lhs_rhs_pairlist, selector_locations, ...)
  # the below processes mimic those from the 'formula' method in that they
  # indicate which pairs are always included and remove those pairs from
  # subset processing with the name removal list
  all_prs <- lapply(.all, formula_parse)
  all_pairs <-
    lapply(
      all_prs,
      function(elem) paste0(elem$lhs_names, "~", elem$rhs_names)
    )
  for (loc in which(lhs_rhs_pairlist %in% unlist(all_pairs))) {
    list_parsed[[selector_locations[[loc]]]] <- TRUE
    rmv_frm_subst[[loc]] <- TRUE
  }
  # process '.set' ----
  selector_locations_sets <-
    proc_set_fml(list_parsed, .set, "formula_list", lhs_rhs_pairlist)
  rmv_frm_subst[unlist(selector_locations_sets)] <- TRUE
  set_labels <- set_labeller(.set, lhs_rhs_pairlist)
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
  # TODO 2: restructure 'args_list' into directly submitted args to 'dominance _scalar()' ----
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
# `domir.formula` internal helper functions ----
#' @title Internal formula parsing function
#' @description Internal formula parsing function to facilitate re-construction
#' of a formula using `reformulate()`
#'
#' Not intended to be called by the user.
#'
#' @keywords internal
#' 
#' @param .obj A `formula`.
#' 
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
#'
#' @name formula_parse
#'
#' @rdname formula_parse
#'
#' @export
formula_parse <- function(.obj) {
  if (is.null(.obj)) return(NULL)
  # obtain 'right hand side'/RHS name vector from `.obj`
  rhs_names <- 
    tryCatch(
      attr(stats::terms(.obj), "term.labels"),
      error = function(err) {
        stop(deparse(.obj), " is an invalid formula.", call. = FALSE)
      }
    )
  # intercept logical - confirms inclusion of intercept
  intercept_lgl <- as.logical(attr(stats::terms(.obj), "intercept"))
  # obtain offsets
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
  # obtain 'left hand side'/LHS (if included)
  if (attr(stats::terms(.obj), "response") == 1) {
    lhs_names <- attr(stats::terms(.obj), "variables")[[2]]
  } else {
    lhs_names <- NULL
  }
  # set logical selection vector; defaults to not selected
  select_lgl <- rep(FALSE, times = length(rhs_names))
  list(rhs_names = rhs_names,
       lhs_names = lhs_names,
       intercept_lgl = intercept_lgl,
       offset = offset,
       select_lgl = select_lgl)
}
#' @title Internal formula output checking function
#' @description Internal function intended to ensure that `.fct` does not 
#' produce errors when `.obj` is applied to it and that `.fct` produces an 
#' atomic scalar-valued numeric result.
#' 
#' Not intended to be called by the user.
#' 
#' @param .obj A `formula`.
#' 
#' @param .fct A `function` or string function name.
#' 
#' @param ... Passes arguments to the function in `.fct`.
#' 
#' @returns The result of the `.fct` call using `.obj`.
#'
#' @keywords internal
formula_output_check <- function(.obj, .fct, ...) {
  test_model <-
    tryCatch(
      do.call(eval(.fct), append(.obj, list(...))),
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
#' @title ? group all .adj functions together?
#' @description ?
#' 
#' Not intended to be called by the user.
#' 
#' @param ?
#' 
#' @returns ?
#'
#' @keywords internal
fml_adj_checker <- function(.adj, fml_parsed) {
  if (!is.logical(.adj) || (length(.adj) > 1))
    stop("'.adj' argument must be logical of length 1.", call. = FALSE)
  if (!fml_parsed$intercept) 
    stop("'.adj' cannot be estimated with removed intercepts.", call. = FALSE)
}
#' TBD
est_adj_value <- function(.adj, fml_parsed, .fct, ...) {
  value <- 0
  if (.adj) {
    fml <- 
      stats::reformulate(
        c("1", fml_parsed$offset),
        response = fml_parsed$lhs_names,
        intercept = fml_parsed$intercept_lgl)
    value <- formula_output_check(fml, .fct, ...)
  }
  value
}
#' TBD
fml_all_update <- function(.all, fml_parsed) {
  if (is.null(.all)) return(fml_parsed)
  if (!inherits(.all, "formula")) 
    stop("'.all' must be a 'formula'.", call. = FALSE)
  all_parsed <- formula_parse(.all)
  if (length(all_parsed$rhs_names) == 0)
    stop("'.all' must have one or more terms.", call. = FALSE)
  if (!is.null(all_parsed$lhs_names))
    stop("Left hand side names not allowed in '.all' formula.", call. = FALSE)
  if (!is.null(all_parsed$offset))
    stop("Offsets not allowed in '.all'.", call. = FALSE)
  if (!all_parsed$intercept_lgl)
    stop("Removing intercepts not allowed in '.all'.", call. = FALSE)
  valid_all_names <- all_parsed$rhs_names %in% fml_parsed$rhs_names
  if (!all(valid_all_names)) {
    bad_names <- paste(all_parsed$rhs_names[!valid_all_names], collapse = ", ")
    stop("Name(s):\n", bad_names, "\nin '.all' not found in '.obj' formula.",
         call. = FALSE)
  }
  pos_all_names <- 
    which(fml_parsed$rhs_names %in% all_parsed$rhs_names)
  fml_parsed$select_lgl[pos_all_names] <- TRUE
  return(fml_parsed)
}
#' TBD
est_all_value <- function(.all, fml_parsed, .fct, .adj, ...) {
  value <- 0
  if (!is.null(.all)) {
    all_parsed <- formula_parse(.all)
    fml <- 
      stats::reformulate(
        c(all_parsed$rhs_names, fml_parsed$offset),
        response = fml_parsed$lhs_names,
        intercept = fml_parsed$intercept_lgl)
    value <- formula_output_check(fml, .fct, ...)
  }
  value - .adj
}
#' TBD
fml_set_checker <- function(.set, fml_parsed, .typ) {
  # check sets overall
  if (is.null(.set)) return(NULL)
  if (!is.list(.set)) stop(.typ, " must be a list.", call. = FALSE)
  if (length(.set) == 0) stop(.typ, " is empty.", call. = FALSE)
  not_fmls <- sapply(.set, function(elem) {!inherits(elem, "formula")})
  if (any(not_fmls)) {
    which_not_fml <- seq_len(length(not_fmls))[not_fmls]
    stop("List element(s):\n", paste(which_not_fml, collapse = " "),
         "\nof ", .typ, " are not valid formulas.", call. = FALSE)
  }
  # check individual sets
  sets_parsed <- lapply(.set, formula_parse)
  rhs_counts_sets <- 
    sapply(sets_parsed, function(elem) length(elem$rhs_names) == 0)
  if (any(rhs_counts_sets)) {
    which_int_only <- seq_len(length(rhs_counts_sets))[rhs_counts_sets]
    stop("Formulas in ", .typ, " must have one or more terms. Formula(s) ", 
         "in list element(s):\n", paste(which_int_only, collapse = ", "),
         "\nhave no terms.", call. = FALSE)
  }
  set_lhs <- sapply(sets_parsed, function(elem) length(elem$lhs_names) == 0)
  if (!all(set_lhs)) {
    which_have_lhs <- seq_len(length(set_lhs))[!set_lhs]
    stop("Left hand sides not allowed in ", .typ, ". Formula(s) in list ",
         "element(s):\n", paste(which_have_lhs, collapse = " "),
         "\nhave left hand sides.", call. = FALSE)
  }
  set_offset <- sapply(sets_parsed, function(elem) is.null(elem$offset))
  if (!all(set_offset)) {
    which_have_offset <- seq_len(length(set_offset))[!set_offset]
    stop("Offsets not allowed in ", .typ, ". Formula(s) in list element(s):\n",
         paste(which_have_offset, collapse = " "), "\nhave offsets.",
         call. = FALSE)
  }
  set_intercept <- sapply(sets_parsed, function(elem) elem$intercept_lgl)
  if (!all(set_intercept)) {
    which_rmv_intercept <- seq_len(length(set_intercept))[!set_intercept]
    stop("Removing intercepts not allowed in ", .typ, ". Formula(s) in list ",
         "element(s):\n", paste(which_rmv_intercept, collapse = " "),
         "\nremove their intercept.", call. = FALSE)
  }
}
#' TBD
check_namelists <- function(fml_parsed, .set, .wst, .all) {
  # prep inputs
  namelist <- fml_parsed$rhs_names
  sets_parsed <- lapply(.set, formula_parse)
  sets_namelists <- lapply(sets_parsed, function(elem) {elem$rhs_names})
  wsts_parsed <- lapply(.wst, formula_parse)
  wsts_namelists <- lapply(wsts_parsed, function(elem) {elem$rhs_names})
  if (
    (length(namelist) == length(unlist(wsts_namelists)) && 
     length(wsts_namelists) == 1)
  ) 
    stop("All names in '.obj' cannot be groupd into a single '.wst'.", call. = FALSE)
  all_parsed <- lapply(list(.all), formula_parse)
  all_namelist <- lapply(all_parsed, function(elem) {elem$rhs_names})
  set_names <- set_labeller2(.set) # !! remove '2' when competing `set_labeller` function removed
  set_names <- paste("`.set ", set_names, "`", sep = "")
  # check name duplication
  if (length(.wst) > 0) {
    wst_names <- paste("`.wst #", seq_len(length(.wst)), "`", sep = "")
  } else {
    wst_names <- character(0)
  }
  all_name <- ifelse(length(.all) > 0, list("`.all`"), list())
  name_vec <- function(vec, nm) {
    names(vec) <- rep(nm, times = length(vec))
    vec
  }
  sets_namelists <- 
    mapply(name_vec, sets_namelists, set_names, SIMPLIFY = FALSE)
  names(sets_namelists) <- NULL
  wsts_namelists <- 
    mapply(name_vec, wsts_namelists, wst_names, SIMPLIFY = FALSE)
  all_namelist <- 
    mapply(name_vec, all_namelist, all_name, SIMPLIFY = FALSE)
  if (!is.null(names(.wst))) 
    stop("Named list elements not allowed with '.wst'.", call. = FALSE)
  between_opt_name_dups <- 
    duplicated(c(unlist(sets_namelists), unlist(wsts_namelists), all_namelist))
  if (any(between_opt_name_dups)) {
    between_namelists <- 
      c(unlist(sets_namelists), unlist(wsts_namelists), all_namelist)
    between_combos <- 
      data.frame(t(utils::combn(seq_len(length(between_namelists)), 2)))
    between_names <- append(sets_namelists, wsts_namelists)
    between_names <- append(between_names, list(all_namelist))
    between_names <- names(unlist(between_names))
    which_dup_names <-
      mapply(
        report_duplicates,
        between_namelists[between_combos[[1]]], 
        between_namelists[between_combos[[2]]],
        between_names[between_combos[[1]]], 
        between_names[between_combos[[2]]]
      )
    stop(
      "Names are duplicated across formulas in the following:\n",
      paste(which_dup_names[which_dup_names != ""], collapse = "\n"),
      call. = FALSE)
  }
  # check missing names
  unmatched_names <- 
    setdiff(c(unlist(sets_namelists), unlist(wsts_namelists)), namelist)
  if (length(unmatched_names) > 0) {
    between_namelists <- 
      c(unlist(sets_namelists), unlist(wsts_namelists))
    between_names <- append(sets_namelists, wsts_namelists)
    between_names <- names(unlist(between_names))
    which_single_names <-
      mapply(
        report_singletons,
        between_namelists, 
        rep(list(namelist), times = length(between_namelists)),
        between_names
      )
    stop(
      "Names in the following are not in '.obj':\n",
      paste(which_single_names[which_single_names != ""], collapse = "\n"),
      call. = FALSE)
  }
  # check '.set' names against namelist
  set_names <- substr(set_names, 7, nchar(set_names)-1)
  set_name_dups <- intersect(namelist, set_names)
  if (length(set_name_dups) > 0) {
    stop(
    "Set names: ", paste(set_name_dups, collapse = ", "), 
    "; are also names in '.obj'. Please rename them.",
    call. = FALSE)
  }
}
#' TBD
set_labeller2 <- function(.set) {
  if (is.null(names(.set))) {
    set_labels <- paste0("set", seq_len(length(.set)))
  } else {
    set_labels <- names(.set)
  }
  missing_set_labels <- which(set_labels == "")
  if (length(missing_set_labels) > 0)
    set_labels[missing_set_labels] <- paste0("set", missing_set_labels)
  set_labels
}
#' TBD
report_duplicates <- function(list1, list2, name1, name2) {
  dups <- intersect(list1, list2)
  ifelse(
    length(dups) > 0,
    paste(
      paste(dups, collapse = ", "), 
      "; from ", name1, " and ", name2, sep = ""
    ),
    ""
  )
}
#' TBD
report_singletons <- function(list1, list2, name) {
  single <- setdiff(list1, list2)
  ifelse(
    length(single) > 0,
    paste(
      paste(single, collapse = ", "), 
      "; from ", name, sep = ""
    ),
    ""
  )
}
#' TBD
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
meta_domir_fml2 <- #update name eventually
  function(submodel_names_lgl, namelist, fml_parsed, .fct, args_2_fct) {
    for (elem in namelist[submodel_names_lgl]) {
      fml_parsed$select_lgl[elem] <- TRUE
    }
    fml <-
      stats::reformulate(
        c(fml_parsed$rhs_names[fml_parsed$select_lgl], fml_parsed$offset),
        response = fml_parsed$lhs_names,
        intercept = fml_parsed$intercept_lgl
      )
    do.call(.fct, append(list(fml), args_2_fct))
  }










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
#' TBD
domir_arg_checker <-
  function(.wst, .rev, .cpt, .cdl, .prg, .cst) {
    lgl_args <- sapply(list(.rev, .cdl, .cpt, .prg), is.logical)
    if (!all(lgl_args))
      stop(
        paste(
          c(".rev", ".cdl", ".cpt", ".prg")[which(!lgl_args)],
          collapse = " "
        ),
        " must be logical.", call. = FALSE
      )
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
