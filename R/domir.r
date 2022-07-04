#' @title Dominance analysis methods
#'
#' @name domir
#'
#' @description 
#' 
#' Parses input object to obtain valid elements, determines all required 
#' combinations/subsets of elements (depends on input type), submits subsets to 
#' a function, and computes dominance decomposition 
#' statistics based on the returned values from the function.
#' 
#' @param .obj A [`formula`], [`Formula`][Formula::Formula], or [`list`]. 
#' 
#' Parsed to produce subsets of elements to submit to `.fct`. Always submits 
#' subsets of `.obj` the same type to `.fct` as the same type or class as 
#' the input object.
#' 
#' @param .fct A function/closure or string function name.
#' 
#' Applied to all subsets of elements as received from `.obj`.  
#' Must return a length 1 (scalar), numeric, atomic vector.
#' 
#' @param .set A `list`.
#' 
#' Must be comprised of elements of the same type or class as `.obj`.  
#' Elements of the list can be named.
#'
#' @param .wst Not yet used.
#' 
#' @param .all A `formula`, `Formula`, or `list`. 
#' 
#' Must be the same type or class as `.obj`.
#' 
#' @param .adj A `formula`, `Formula`, or `list`.
#' 
#' Must be the same type or class as `.obj`.
#' 
#' @param .cdl Logical.  
#' 
#' If `FALSE` then conditional dominance matrix is not computed and 
#' method to produce general dominance statistics changes.
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
#' @param ... Passes arguments to other methods; passes arguments to 
#' function in `.fct`.
#'
#' @return Returns an object of [`class`] "domir" which is a composed of the 
#' following elements:
#' 
#' \describe{
#'  \item{`General_Dominance`}{Vector of general dominance values.}
#'  \item{`Standardized`}{Vector of general dominance values normalized 
#'  to sum to 1.}
#'  \item{`Ranks`}{Vector of ranks applied to the general dominance values.}
#'  \item{`Conditional_Dominance`}{Matrix of conditional dominance values.  
#'  Each row represents an element in `.obj`; 
#'  each column represents a number of elements from `.obj` in a subset.}
#'  \item{`Complete_Dominance`}{Logical matrix of complete dominance 
#'  designations. 
#'  The `.obj` elements represented in each row indicates dominance status; 
#'  the `.obj` elements represented in each column indicates 
#'  dominated-by status.}
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
#' 
#' ## Element Parsing
#' 
#' `.obj`s elements are parsed and used to determine the required number of 
#' subsets included the dominance analysis.  How the elements are parsed is 
#' determined depends on `.obj`s type or class method.
#' 
#' ### `formula`
#' 
#' The `formula` method applies the standard [`terms`] function parsing 
#' which separates term names on the right hand side of the `formula`.  All 
#' terms separated by `+` are considered a separate element for generating 
#' subsets.
#' 
#' Any terms on the left hand side of `.obj` are retained and passed through 
#' to all subsets.
#' 
#' ### `Formula` and `list`
#' 
#' The `Formula` and `list` methods are not yet implemented.
#' 
#' ### Additional Details
#' 
#' By default, each parsed element in `.obj` will be used as a separate 
#' element to generate subsets and will obtain a separate contribution to 
#' the returned value.
#' 
#' ## Changing Element Parsing
#' 
#' All methods' default behavior of using all elements to generate subsets 
#' can be overriden using `.set`, `.all`, and `.adj` arguments. 
#' 
#' Elements in `.set`, `.all`, and `.adj` must also be present in `.obj`.  
#' The entries in three arguments change `.obj`s parsing behavior but still 
#' depend on `.obj` as the primary input object.
#' 
#' ### `.set`
#' 
#' `.set` binds together elements in `.obj` such that they form 
#' a single new element. The elements in `.obj` bound together contribute 
#' jointly to the returned value.
#' 
#' If elements in `.set` are named, the `.set` element's name will be used 
#' in the "domir" object returned and all printed results.
#' 
#' The `formula` method for `.set` does not allow any element to have a left 
#' hand side.
#' 
#' ### `.all`
#' 
#' `.all` binds elements in `.obj` to all subsets. The elements in `.obj` bound 
#' together by `.all` are given precedence in ascribing the returned value and 
#' contribute jointly to `Value_All`. `Value_All` is determined prior to 
#' conducting the dominance analysis and its value is removed from the returned 
#' values for all subsets.
#' 
#' The `formula` method for `.all` does not allow a left hand side.
#' 
#' ### `.adj`
#' 
#' `.adj` binds elements in `.obj` to all subsets. The elements in `.obj` bound 
#' together by `.adj` are considered external to the dominance analysis but 
#' are adjusted for given they affect the returned value.  Elements in `.adj` 
#' contribute jointly to `Value_Adjust` and have a higher precedence than 
#' those bound in `.all`. `Value_Adjust` is determined prior to conducting 
#' the dominance analysis and its value is removed from the returned 
#' values for all subsets as well as from `Value_All`.
#' 
#' The `formula` method for `.adj` does not allow a left hand side but 
#' allows constants/intercepts (i.e., `~ 1`) to be included as a valid 
#' element in the right hand size even when not explicitly included in `.obj`.
#' 
#' ### Additional Details
#' 
#' All element parsing methods for `domir` will submit subsets generated as an 
#' object of the same type as `.obj` (i.e., a `formula` in `.obj` will be 
#' submitted as a `formula`) to the `.fct` as the first, unnamed argument.
#' 
#' ## `.fct` as Analysis Pipeline
#' 
#' The function in `.fct` will be called repeatedly, once for each subset 
#' of elements created from `.obj`.
#' 
#' `.fct` is expected to be a complete analysis pipeline that receives a 
#' subset of elements from `.obj`, uses the subset of elements from `.obj` in 
#' the type/class as received to generate a predictive model, and 
#' extracts a returned value of the appropriate type to dominance analyze.  
#' 
#' At current, only atomic (i.e., non-`list`), numeric scalars (i.e., 
#' vectors of length 1) are allowed as returned values.
#' 
#' `domir` is intended to be strict about input and output requirements for 
#' functions in `.fct` and applies a series of checks to ensure the input and 
#' output adhere to these requirements.  In most circumstances, 
#' the user will have to make their own named or anonymous function to 
#' supply to `.fct` to meet `domir`s requirements.
#' 
#' @export
#' @examples
#' ## Basic linear model with r-square
#' 
#' lm_r2 <- function(fml, data) { 
#'   lm_res <- lm(fml, data = data)
#'   r2 <- summary(lm_res)[["r.squared"]]
#'   return(r2) }
#' 
#' domir(mpg ~ am + vs + cyl, 
#'   lm_r2,
#'   data = mtcars)
#' 
#' 
#' ## Linear model including set
#' 
#' domir(mpg ~ am + vs + cyl + carb + gear + disp + wt,
#'  lm_r2,
#'  .set = list(~ carb + gear, ~ disp + wt),
#'  data = mtcars)
#'
#'
#' ## Multivariate linear model with multivariate r-square
#' ## and all subsets variable
#' 
#' mlm_rxy <- function(fml, data, dvnames) {
#'   mlm_res <- lm(fml, data = data)
#'   mlm_pred <- predict(mlm_res)
#'   mlm_rxy <- cancor(mlm_pred, data[dvnames])$cor[[1]]^2
#'   return(mlm_rxy)
#'   }
#'        
#' domir(cbind(wt, mpg) ~ vs + cyl + am + carb,
#'   mlm_rxy, 
#'   .all = ~ carb,
#'   data = mtcars, 
#'   dvnames = c("wt", "mpg"))
#'
#'
#' ## Named sets
#' 
#' domir(mpg ~ am + gear + cyl + vs + qsec + drat,
#'   lm_r2,
#'   data = mtcars, 
#'   .set = list(trns = ~ am + gear, 
#'     eng = ~ cyl + vs, misc = ~ qsec + drat))
#'   
#' ## Linear model using AIC
#' 
#' lm_aic <- function(fml, data) { 
#'   lm_res <- lm(fml, data = data)
#'   aic <- AIC(lm_res)
#'   return(aic) }
#' 
#' domir(mpg ~ am + carb + cyl, 
#'   lm_aic, 
#'   .adj = ~ 1,
#'   .rev = TRUE,
#'   data = mtcars)

domir <- function(.obj, ...) {
  
  UseMethod("domir")
  
}

#' @rdname domir
#' @exportS3Method 
domir.formula <- function(
    .obj, .fct,
    .set = NULL, .wst = NULL, .all = NULL, .adj = NULL, 
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, 
    ...) {
  
  if (!is.null(.wst)) {
    
    .NotYetUsed(".wst")
    
  }
  
  # Process formula ----
  # obtain rhs name vector from `.obj`
  RHS_names <- 
    attr(stats::terms(.obj), "term.labels") 
  
  # Intercept logical - for `reformulate` later
  Intercept <- 
    as.logical( 
      attr(stats::terms(.obj), "intercept") 
    ) 
  
  # Obtain lhs (if included)
  if (attr(stats::terms(.obj), "response") == 1)
    LHS_names <- 
    attr(stats::terms(.obj), "variables")[[
      attr(stats::terms(.obj), "response") + 1
    ]]
  
  else LHS_names <- NULL

  
  # Process '.all' ----
  if (!is.null(.all)) {
    
    if (!inherits(.all, "formula")) {
      
      stop("'.all' must be a 'formula'.", call. = FALSE)
      
    }
    
    if (attr(stats::terms(.all), "response") == 1) {
      
      stop("'.all' must not have a response/left hand side.", call. = FALSE)
      
    }
    
    if (length(attr(stats::terms(.all), "term.labels")) == 0) {
      
      stop("'.all' cannot be an intercept-only model.", 
           call. = FALSE)
      
    }
    
    # name vector from '.all'
    All_names <- attr(stats::terms(.all), "term.labels")
    
    # remove names from `RHS_names` list if in `.all`
    all_remove_loc <- 
      unlist(
        lapply(All_names, 
                    function(x) which(RHS_names %in% x))
        )
    
    if (length(all_remove_loc) != length(unlist(All_names))) {
      
      wrong_all_terms <- 
        paste(
          unlist(All_names)[
            which(!(unlist(All_names) %in% 
                      RHS_names))], 
          collapse = " ")
      
      stop("Names ", wrong_all_terms,
           " in '.all' do not match any names in '.obj'.", 
           call. = FALSE)
    }
    
    RHS_names <- RHS_names[-all_remove_loc]
    
  }
  
  else All_names <- NULL
  
  # Process '.adj' ----
  if (!is.null(.adj)) {
    
    if (!inherits(.adj, "formula")) {
      
      stop("'.adj' must be a 'formula'.", call. = FALSE)
      
    }
    
    if (attr(stats::terms(.adj), "response") == 1) {
      
      stop("'.adj' must not have a response/left hand side.", call. = FALSE)
      
    }
    
    # if .adj is an intercept-only model
    if (length(attr(stats::terms(.adj), "term.labels")) == 0) {
      
      Adj_names <- "1"
      
    }
    
    else {
      
      # name vector in '.adj'
      Adj_names <- attr(stats::terms(.adj), "term.labels")
      
      # remove names from `RHS_names` list if in `.adj`
      adj_remove_loc <- 
        unlist(
          lapply(Adj_names, 
                      function(x) which(RHS_names %in% x))
          )
      
      if (length(adj_remove_loc) != length(unlist(Adj_names))) {
        
        wrong_adj_terms <- 
          paste(
            unlist(Adj_names)[
              which(!(unlist(Adj_names) %in% 
                        RHS_names))], 
            collapse = " ")
        
        stop("Names ", wrong_adj_terms,
             " in '.adj' do not match any names in '.obj'.", 
             call. = FALSE)
      }
      
      RHS_names <- RHS_names[-adj_remove_loc]
      
    }
    
  }
  
  else Adj_names <- NULL
  
  # Process '.set' ----
  if (!is.null(.set)) {
    
    if (!is.list(.set)) {
      stop("'.set' must be a 'list'.", call. = FALSE)
    }
    
    if (!all(sapply(.set, inherits, "formula"))) {
      
      elements_not_formula <- 
        paste(which((!sapply(.set, inherits, "formula"))), 
              collapse = " ")
      
      stop("Each element of list in '.set' must be a 'formula'.\n", 
           "Elements ", elements_not_formula, 
           " are not a 'formula'.", call. = FALSE)
    }
    
    if (any(sapply(.set, function(x) 
      attr(stats::terms(x), "response") == 1))) {
      
      elements_has_response <- 
        paste(which(sapply(.set, 
                           function(x) attr(stats::terms(x), "response")) > 0 ), 
              collapse = " ")
      
      stop("Elements in '.set' must not have responses/left hand sides.\n",
           "Elements ", elements_has_response, " have responses.", 
           call. = FALSE)
      
    }
    
    if (any(sapply(.set, function(x) 
      length(attr(stats::terms(x), "term.labels")) == 0))) {
      
      elements_intercept_only <- 
        paste(
          which(sapply(.set, 
                       function(x) 
                         length(attr(stats::terms(x), "term.labels"))) > 0 ), 
          collapse = " ")
      
      stop("Elements in '.set' must not be intercepts-only.\n",
           "Elements ", elements_intercept_only, " have no terms.", call. = FALSE)
      
    }
    
    # name vectors from each element of '.set'
    Set_names <- 
      lapply(.set, 
             function(x) attr(stats::terms(x), "term.labels"))
    
    # remove IVs from `RHS_names` list if in `.set`
    set_remove_loc <- 
      unlist(
        lapply(Set_names, 
                    function(x) which(RHS_names %in% x))
        )
    
    if (length(set_remove_loc) != length(unlist(Set_names))) {
      
      wrong_set_terms <- 
        paste(
          unlist(Set_names)[
            which(!(unlist(Set_names) %in% 
                      RHS_names))], 
          collapse = " ")
      
      stop("Names ", wrong_set_terms,
           " in '.set' do not match any names ", 
           "in .'obj'.", 
           call. = FALSE)
    }
    
    RHS_names <- RHS_names[-set_remove_loc]
    
    # apply labels to '.set'
    if (!is.null(names(.set)))    
      Set_labels <- names(.set)
    
    else Set_labels <- paste0("set", 1:length(.set))
    
    missing_Set_labels <- which(Set_labels == "")
    
    if (length(missing_Set_labels) > 0)
      Set_labels[missing_Set_labels] <- paste0("set", missing_Set_labels)
    
    if (any(Set_labels %in% RHS_names)) {
      
      repeat_names <- Set_labels[which(Set_labels %in% RHS_names)]
      
      stop("Set element names ",
           paste(repeat_names, collapse = " "), 
           " are also the names of elements in '.obj'.\n",
           "Please rename these '.set' elements.", call. = FALSE)
    }
    
  }
  
  else Set_names <- NULL
  
  # Too few subsets error
  if ((length(RHS_names) + length(Set_names)) < 2) 
    stop("At least two subsets are needed for a dominance analysis.",
         call. = FALSE)
  
  # Define meta-function to coordinate .fct calls ----
  
  meta_domir <- 
    function(Selector_lgl, 
             RHS, LHS, 
             .fct, .all, .adj, 
             intercept, args_2_fct) { 
      
      # logical vector to select subset of names
      selected_names <- 
        unlist(RHS[Selector_lgl])
      
      # reconstruct formula to submit to '.fct'
      formula_to_use <-
        stats::reformulate(
          c(selected_names, .all, .adj), 
          response = LHS, intercept = intercept)
      
      # submit formula and arguments to '.fct'
      returned_scalar <- 
        do.call(.fct, 
                append(formula_to_use, args_2_fct) ) 
      
      return(returned_scalar)
      
    }
  
  # Check '.fct' inputs ----

  # does '.fct' work?
  test_model <- 
    tryCatch(
      do.call(eval(.fct), append(.obj, list(...))), 
      error = function(err) 
        stop("'.fct' produced an error when applied to '.obj'.\n", 
             "Also, check arguments passed to '.fct'", 
             call. = FALSE)
      )
  
  # is '.fct's returned value an atomic numeric scalar?
  if (!is.numeric(test_model) || !is.vector(test_model) || 
      !is.atomic(test_model) || length(test_model) != 1) 
    stop("result of '.fct' is not an atomic numeric, scalar/",
         "vector of length of 1 value.", call. = FALSE)
  
  # is '.fct's returned value regarded as a list?
  if (is.list(test_model)) 
    stop("result of '.fct' is a list.  It must be flattened/unlisted.", 
         call. = FALSE)
  
  # Define arguments to `dominance_scalar` ----
  
  args_list <-  
    list(RHS = append(RHS_names, Set_names), 
         LHS = deparse(LHS_names),
         .fct = .fct,
         .all = All_names, 
         .adj = Adj_names, 
         intercept = Intercept,
         args_2_fct = list(...))
  
  cons_args <-
    list(RHS = RHS_names, 
         LHS = deparse(LHS_names),
         .fct = .fct,
         .all = NULL, 
         .adj = Adj_names, 
         intercept = Intercept, 
         args_2_fct = list(...))
  
  # Call `dominance_scalar` ----
  return_list <- 
    dominance_scalar(meta_domir, args_list, cons_args, test_model, 
                 .cdl, .cpt, .rev)
  
  # Finalize returned values and attributes ----
  
  if (is.null(.set)) 
    IV_Labels <- RHS_names
  
  else IV_Labels <- c(RHS_names, Set_labels)
  
  names(return_list$General_Dominance) <- 
    IV_Labels
  
  names(return_list$General_Dominance_Ranks) <- 
    IV_Labels
  
  if (.cdl)
    dimnames(return_list$Conditional_Dominance) <- 
    list(names(return_list$General_Dominance), 
         paste0("subset_size_", 1:length(return_list$General_Dominance)))
  
  if (.cpt)
    dimnames(return_list$Complete_Dominance) <- 
    list(paste0("Dmnates_", names(return_list$General_Dominance)),  
         paste0("Dmnated_", names(return_list$General_Dominance)))
  
  if (.rev == FALSE) 
    Standardized <- 
    return_list$General_Dominance / 
    (
      return_list$Value - 
        ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0)
    ) 
  
  else Standardized <- 
    -return_list$General_Dominance / 
    -(
      return_list$Value - 
        ifelse(length(return_list$Adj_result) > 0, return_list$Adj_result, 0)
    )
  
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
  
  return(return_list)
  
}

#' @rdname domir
#' @exportS3Method 
domir.Formula <- function(...) {
  
  .NotYetImplemented()
  
}

#' @rdname domir
#' @exportS3Method 
domir.list <- function(...) {
  
  .NotYetImplemented()
  
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
#'  \item{Value for the elements included in `.all` if any.}  
#'  \item{Value for the elements included in `.adj` if any.}
#'  \item{Matrix describing general dominance values, standardized 
#'  general dominance values, and the ranking of the general 
#'  dominance values.}
#'  \item{Matrix describing the conditional dominance values; when computed}
#'  \item{Matrix describing the complete dominance designations; when 
#'  determined}
#'  \item{If following `summary.domir`, matrix describing the strongest 
#'  dominance designations between all elements.}}
#'  
#'  The `domir` print method alters dimension names for readability and they 
#'  do not display as stored in the `domir` object.
#'  
#' @exportS3Method 

print.domir <- function(x, ...) {
  
  cat("Overall Value:     ", x[["Value"]], "\n")
  
  if (length(x[["Value_All"]]) > 0) 
    cat("All Subset Value:  ", x[["Value_All"]], "\n")
  
  if (length(x[["Value_Adjust"]]) > 0) 
    cat("Adjustment Value:  ", 
        x[["Value_Adjust"]], "\n")
  
  cat("\n")
  
  cat("General Dominance Values:\n")
  
  Display_Std <- 
    t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
  
  dimnames(Display_Std) <- 
    list(names(x[["Ranks"]]), c("General Dominance", "Standardized", "Ranks"))
  
  print(Display_Std, ...)
  
  cat("\n")
  
  if (length(x[["Conditional_Dominance"]] > 0)) {
    
    cat("Conditional Dominance Values:\n")
    
    colnames(x[["Conditional_Dominance"]]) <- 
      paste("Subset Size:", 1:ncol(x[["Conditional_Dominance"]]))
    
    print(x[["Conditional_Dominance"]], ...)
    
    cat("\n")
    
  }
  
  if (length(x[["Complete_Dominance"]] > 0)) {
    
    cat("Complete Dominance Designations:\n")
    
    colnames(x[["Complete_Dominance"]]) <- 
      gsub("^Dmnated_", "Dmnated?", colnames(x[["Complete_Dominance"]]))
    
    rownames(x[["Complete_Dominance"]]) <- 
      gsub("^Dmnates_", "Dmnates?", rownames(x[["Complete_Dominance"]]))
    
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
#' @details The summary method for class `domir` is used for obtaining the 
#' strongest dominance designations (i.e., general, conditional, or complete) 
#' among the elements.
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
    
    pairs <- rbind(pairs[1,], rep("", times = ncol(pairs)), pairs[2,])
    
    location <- 0
    
    for (IV1 in 1:(length(object$General_Dominance) - 1)) {
      
      for (IV2 in (IV1+1):length(object$General_Dominance)) {
        
        location <- location + 1
        
        if (length(object[["Complete_Dominance"]] > 0)) {
          
          if (!is.na(object$Complete_Dominance[IV1, IV2])) {
            pairs[2, location] <- 
              ifelse(object$Complete_Dominance[IV1, IV2], 
                     "completely dominates",
                     "is Completely dominated by")
            
            next
            
          }
          
        }
        
        if (length(object[["Conditional_Dominance"]] > 0)) {
          
          if (all(object$Conditional_Dominance[IV1,]*reverse_cdl > 
                  object$Conditional_Dominance[IV2,]*reverse_cdl)) { 
            
            pairs[2, location] <- "conditionally dominates"
            
            next
            
          }
          
          else if (all(object$Conditional_Dominance[IV1,]*reverse_cdl < 
                       object$Conditional_Dominance[IV2,]*reverse_cdl)) {
            
            pairs[2, location] <- "is conditionally dominated by"
            
            next
            
          }
          
        }
        
        pairs[2, location] <- 
          ifelse(object$General_Dominance[[IV1]]*reverse_gnl > 
                   object$General_Dominance[[IV2]]*reverse_gnl, 
                 "generally dominates", 
                 ifelse(object$General_Dominance[[IV1]]*reverse_gnl < 
                          object$General_Dominance[[IV2]]*reverse_gnl,
                        "is generally dominated by", 
                        "has no dominance designation with"))
        
      }
      
    }
    
    rownames(pairs) <- rep("", times = 3)
    
    colnames(pairs) <- rep("", times = ncol(pairs))
    
    res <- append(object, list(Strongest_Dominance = pairs))
    
    class(res) <- c("domir")
    
    return(res)
    
  }
  
  else return(object)
  
}
