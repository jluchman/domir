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
#' subsets of `.obj` that are of the same [`class`] to `.fct` and are always 
#' submitted as the first, unnamed argument.
#' 
#' @param .fct A [`function`]/closure or string function name.
#' 
#' Applied to all subsets of elements as received from `.obj`.  
#' Must return a length 1 (scalar), numeric, atomic vector.
#' 
#' @param .set A `list`.
#' 
#' Must be comprised of elements of the same class as `.obj`.  
#' Elements of the list can be named.
#'
#' @param .wst Not yet used.
#' 
#' @param .all A `formula`, `Formula`, or `list`. 
#' 
#' Must be the same class as `.obj`.
#' 
#' @param .adj A `formula`, `Formula`, or `list`.
#' 
#' Must be the same class as `.obj`.
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
#' the function in `.fct`.
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
#' determined depends on `.obj`s class method.
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
#' allows the intercept (i.e., `~ 1`) to be included as a valid 
#' element in the right hand size even when not explicitly included in `.obj`.
#' 
#' ### Additional Details
#' 
#' All element parsing methods for `domir` will submit subsets generated as an 
#' object of the same class as `.obj` (i.e., a `formula` in `.obj` will be 
#' submitted as a `formula`) to the `.fct` as the first, unnamed argument.
#' 
#' ## `.fct` as Analysis Pipeline
#' 
#' The function in `.fct` will be called repeatedly; once for each subset 
#' of elements created from `.obj`.
#' 
#' `.fct` is expected to be a complete analysis pipeline that receives a 
#' subset of elements from `.obj`, uses the subset of elements from `.obj` in 
#' the class as received to generate a predictive model, and 
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
#' # Notes
#' 
#' ## `formula` method
#' 
#' `formula` objects and all arguments for the `formula` method do not allow 
#' [`offset`] terms. When the user needs to include offset terms in their model, 
#' they could be included by the user in the `.fct` as an 
#' [`update`][update.formula] to the submitted `formula` object or through 
#' other similar means.
#' 
#' `formula`s with second-order or higher terms (i.e., interactions like
#'  `~ a*b`) are not, by default, used differently from first-order terms. 
#'  The dominance analysis results from models using such terms may not produce 
#'  useful results unless the user ensures that second-order and higher terms 
#'  are used appropriately in `.fct`.
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
  # Obtain 'right hand side'/RHS name vector from `.obj`
  RHS_names <- 
    attr(stats::terms(.obj), "term.labels") 
  
  # Intercept logical - for `reformulate` later
  Intercept <- 
    as.logical( 
      attr(stats::terms(.obj), "intercept") 
    ) 
  
  # Offsets are not allowed - too complex to accommodate
  if (!is.null(attr(stats::terms(.obj), "offset"))) {
    
    stop("'offset()' terms are not allowed in formula-class '.obj's.\n", 
         "Offset terms can be included using 'update()' in the call to .fct.\n", 
         "For example, 'function(fml) {fml_ofst <- update(fml, . ~ . + offset(x)); ...}'.", 
         call. = FALSE)
    
  }

  
  # Obtain 'left hand side'/LHS (if included)
  if (attr(stats::terms(.obj), "response") == 1)
    LHS_names <- 
    rownames(attr(stats::terms(.obj), "factors"))[[
      attr(stats::terms(.obj), "response")
    ]]
  
  else LHS_names <- NULL
  
  # Define formula argument checks function ----
  fml_check <- function(fml, name, chk_int) {
    
    # enforce formula class
    if (!inherits(fml, "formula")) {
      
      stop(name, " must be a 'formula'.", call. = FALSE)
      
    }
    
    # enforce no response/lhs
    if (attr(stats::terms(fml), "response") == 1) {
      
      stop(name, " must not have a response/left hand side.", call. = FALSE)
      
    }
    
    # enforce no offset term
    if (!is.null(attr(stats::terms(fml), "offset"))) {
      
      stop("'offset()' terms not allowed in ", name, ".", 
           call. = FALSE)
      
    }
    
    # enforce no empty model - when applicable
    if ((length(attr(stats::terms(fml), "term.labels")) == 0) && chk_int) {
      
      stop(name, " cannot be an intercept-only/empty formula.", 
           call. = FALSE)
      
    }
    
  }
  
  # Define function to remove rhs names
  rhs_name_remover <- function(obj, rhs, name) {
    
    # find locations of names to remove (if any)
    remove_loc <- 
      unlist( lapply(obj, 
               function(x) which(rhs %in% x)) )
    
    # error if some of the names did not match 
    if (length(remove_loc) != length(unlist(obj))) {
      
      wrong_terms <- 
        paste( unlist(obj)[ which(!(unlist(obj) %in% rhs)) ], 
          collapse = " ")
      
      stop("Names ", wrong_terms, " in ", name, 
           " do not match any names in '.obj'.", 
           call. = FALSE)
    }
    
    return(rhs[-remove_loc])
    
  }
  
  # Process '.all' ----
  if (!is.null(.all)) {
    
    # apply checks to '.all' argument
    fml_check(.all, "'.all'", TRUE)
    
    # name vector from '.all'
    All_names <- attr(stats::terms(.all), "term.labels")
    
    # check names in '.all' and remove from rhs
    RHS_names <- 
      rhs_name_remover(All_names, RHS_names, "'.all'")
    
  }
  
  else All_names <- NULL
  
  # Process '.adj' ----
  if (!is.null(.adj)) {
    
    # apply checks to '.adj' argument
    fml_check(.adj, "'.adj'", FALSE)
    
    # if '.adj' is an intercept-only model - checks/include
    if (
      (length(attr(stats::terms(.adj), "term.labels")) == 0) && 
      (attr(stats::terms(.adj), "intercept") == 1) && Intercept
    ) Adj_names <- "1"
    
    else if (
      (length(attr(stats::terms(.adj), "term.labels")) == 0) && 
      (attr(stats::terms(.adj), "intercept") == 1) && !Intercept
    ) stop("'.adj' cannot add an intercept when it is removed \n", 
    "from the formula in '.obj'.", 
           call. = FALSE)
    
    else if (
      (length(attr(stats::terms(.adj), "term.labels")) == 0) && 
      (attr(stats::terms(.adj), "intercept") == 0)
    ) stop("'.adj' cannot be an empty formula.", call. = FALSE)

    else {
      
      # name vector in '.adj'
      Adj_names <- attr(stats::terms(.adj), "term.labels")
      
      # check names in '.adj' and remove from rhs
      RHS_names <- 
        rhs_name_remover(Adj_names, RHS_names, "'.adj'")
      
    }
    
  }
  
  else Adj_names <- NULL
  
  # Process '.set' ----
  if (!is.null(.set)) {
    
    if (!is.list(.set)) {
      stop("'.set' must be a 'list'.", call. = FALSE)
    }
    
    # apply checks to all elements of '.set' argument
    .mapply(fml_check, list(.set, 
            paste0("Position ", 1:(length(.set)), " of '.set'."), 
            rep(TRUE, times = length(.set))),
            NULL)
    
    # name vectors from each element of '.set'
    Set_names <- 
      lapply(.set, 
             function(x) attr(stats::terms(x), "term.labels"))
    
    # recursively check names in '.set'-s and remove from rhs
    for (set in 1:length(Set_names)) {
      
      RHS_names <- 
        rhs_name_remover(Set_names[[set]], RHS_names, 
                         paste0("Position ", set, " of '.set'."))
      
    }
    
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
         LHS = LHS_names,
         .fct = .fct,
         .all = All_names, 
         .adj = Adj_names, 
         intercept = Intercept,
         args_2_fct = list(...))
  
  cons_args <-
    list(RHS = RHS_names, 
         LHS = LHS_names,
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
                     "is completely dominated by")
            
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
