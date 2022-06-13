#' @title Dominance analysis methods
#'
#' @name domir
#'
#' @description 
#' 
#' Parses input depending on method, computes all valid subsets, 
#' submits subsets to a function, and computes dominance decomposition 
#' statistics based on the returned values from the function.
#' 
#' @param .obj A [`formula`], [`Formula`], or [`list`]. 
#' 
#' Parsed to produce subsets.  Always submits subsets of the same type 
#' to `.fct`.  
#' 
#' @param .fct A function/closure or string function name.
#' 
#' Applied to all subsets received from `.obj`.  Must return a length 1 
#' (scalar), numeric vector.
#' 
#' @param .set A `list`.
#' 
#' Must be comprised of elements of the same class as `.obj`.  Elements of 
#' the list can be named.
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
#' function in `.fct`.
#'
#' @return Returns an object of [`class`] "domir" which is a list composed of:
#' 
#' \describe{
#'  \item{`General_Dominance`}{Vector of general dominance statistics.}
#'  \item{`Standardized`}{Vector of general dominance statistics normalized 
#'  to sum to 1.}
#'  \item{`Ranks`}{Vector of ranks applied to the general dominance statistics.}
#'  \item{`Conditional_Dominance`}{Matrix of Conditional dominance statistics.  
#'  Each row represents a term; each column represents an order of terms.}
#'  \item{`Complete_Dominance`}{Logical matrix of Complete dominance 
#'  designations. The term represented in each row indicates dominance status; 
#'  the terms represented in each columns indicates dominated-by status.}
#'  \item{`Fit_Statistic_Overall`}{Value of fit statistic for the full 
#'  model.}
#'  \item{`Fit_Statistic_All_Subset`}{Value of fit statistic associated 
#'  with terms in `.all`.}
#'  \item{`Fit_Statistic_Constant_Model`}{Value of fit statistic associated 
#'  with terms in `.adj`.}
#'  \item{`Call`}{The matched call.}
#' }
#'
#' @details 
#' 
#' `.obj` is used to determine the number of subsets included the 
#' dominance analysis.  How the subsets are determined depends on `.obj`s 
#' class method.
#' 
#' The `formula` method applies the standard [`terms`] function parsing 
#' which separates elements on the right hand side of the `formula`.  All 
#' elements separated by `+` are considered a separate term that contributes to 
#' determining subsets. The `Formula` and `list` methods are not 
#' yet implemented.
#' 
#' All methods for `domir` will submit subsets generated to the 
#' function in `.fct` as the first, unnamed argument.
#'
#' All subset generation methods' default behavior of using all elements 
#' as separate subset factors can be overriden using `.set`, `.all`, 
#' and `.adj` arguments. 
#' 
#' `.set` binds together elements in `.obj` such that the form their own term. 
#' 
#' `.all` includes elements in `.obj` all subsets including their contribution 
#' to the result.
#' 
#' `.adj` includes elements in `.obj` all subsets removing their contribution 
#' to the result.
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
#' ## Multivariate linear model with custom multivariate r-square function 
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
    .set = NULL, .all = NULL, .adj = NULL, 
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, 
    ...) {
  
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
                           function(x) attr(terms(x), "response")) > 0 ), 
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
      
      # submit formula and arguments to '.fct' - expect
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
        stop("'.fct' produced an error when ", 
             "applied to '.obj'.", call. = FALSE)
      )
  
  # is '.fct's returned value a numeric scalar?
  if (!is.numeric(test_model) && length(test_model) != 1) 
    stop("result of '.fct' is not a numeric, scalar/",
         "length of 1 value.", call. = FALSE)
  
  # Define arguments to `domir_scalar` ----
  
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
  
  # Call `domir_scalar` ----
  return_list <- 
    domir_scalar(meta_domir, args_list, cons_args, test_model, 
                 .cdl, .cpt, .rev)
  
  # Finalize returned values and attributes ----
  
  if (is.null(.set)) 
    IV_Labels <- RHS_names
  
  else IV_Labels <- c(RHS_names, Set_labels)
  
  names(return_list$General_Dominance) <- IV_Labels
  
  names(return_list$General_Dominance_Ranks) <- IV_Labels
  
  if (.cdl)
    dimnames(return_list$Conditional_Dominance) <- 
    list(names(return_list$General_Dominance), 
         paste0("IVs_", 1:length(return_list$General_Dominance)))
  
  if (.cpt)
    dimnames(return_list$Complete_Dominance) <- 
    list(paste0("Dmnates_", names(return_list$General_Dominance)),  
         paste0("Dmnated_", names(return_list$General_Dominance)))
  
  if (.rev == FALSE) # Standardized if metric increases...
    Standardized <- 
    return_list$General_Dominance / 
    (
      return_list$FitStat - 
        ifelse(length(return_list$Cons_Result) > 0, return_list$Cons_Result, 0)
    ) # ...then use normal standardization...
  
  else Standardized <- 
    -return_list$General_Dominance / 
    -(
      return_list$FitStat - 
        ifelse(length(return_list$Cons_Result) > 0, return_list$Cons_Result, 0)
    ) # ...otherwise reverse the general dominance stats to standardize
  
  return_list <- list(
    "General_Dominance" = return_list$General_Dominance,
    "Standardized" = Standardized,
    "Ranks" = return_list$General_Dominance_Ranks,
    "Complete_Dominance" = return_list$Complete_Dominance,
    "Conditional_Dominance" = return_list$Conditional_Dominance,
    "Fit_Statistic_Overall" = return_list$FitStat,
    "Fit_Statistic_All_Subset" = 
      return_list$All_Result - 
      ifelse(is.null(return_list$Cons_Result), 0, return_list$Cons_Result),
    "Fit_Statistic_Constant_Model" = return_list$Cons_Result,
    "Call" = match.call()
  )
  
  # apply class 'domir'
  class(return_list) <- c("domir") 
  
  return(return_list)
  
}

#' @rdname domir
#' @exportS3Method 
domir.Formula <- function(
    .obj, .fct,
    .set = NULL, .all = NULL, .adj = NULL, 
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, 
    ...) {
  
  stop("'domir' 'Formula' method not yet implemented.", call. = FALSE)
  
}

#' @rdname domir
#' @exportS3Method 
domir.list <- function(
    .obj, .fct,
    .set = NULL, .all = NULL, .adj = NULL, 
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, 
    ...) {
  
  stop("'domir' 'list' method not yet implemented.", call. = FALSE)
  
}

#' @title Print method for \code{domir}
#' @description Reports formatted results from \code{domir} class object.
#' @param x an object of class "domin".
#' @param ... further arguments passed to or from other methods. Not used currently.
#' @return The "domin" object with altered column and row names for Conditional and Complete dominance results as displayed in the console.
#' @details The print method for class \code{domin} objects reports out the following results:
#' \itemize{
#'  \item{Fit statistic for the full model.  The fit statistic for the all subset model is reported here if there are any entries in \code{.all}.  The fit statistic for the constant model is reported here if there are any entries in \code{consmodel}.}
#'  \item{Matrix describing general dominance statistics, standardized general dominance statistics, and the ranking of the general dominance statistics}
#'  \item{If \code{.cdl} is \code{TRUE}, matrix describing the Conditional dominance designations}
#'  \item{If \code{.cpt} is \code{TRUE}, matrix describing the Complete dominance designations}
#'  \item{If following \code{summary.domin}, matrix describing the strongest dominance designations between all independent variables}
#'  \item{If there are entries in \code{.set} and/or \code{.all} the terms included in each set as well as the terms in all subset are reported}}
#'  The \code{domin} print method alters dimension names for readability and they do not display as stored in the original \code{domin} object.
#' @exportS3Method 

print.domir <- function(x, ...) {
  
  cat("Overall Fit Statistic:     ", x[["Fit_Statistic_Overall"]], "\n")
  
  if (length(x[["Fit_Statistic_All_Subset"]]) > 0) 
    cat("All Subset Fit Statistic: ", x[["Fit_Statistic_All_Subset"]], "\n")
  
  if (length(x[["Fit_Statistic_Constant_Model"]]) > 0) 
    cat("Constant Model Fit Statistic: ", x[["Fit_Statistic_Constant_Model"]], "\n")
  
  cat("\n")
  
  cat("General Dominance Statistics:\n")
  
  Display_Std <- 
    t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
  
  dimnames(Display_Std) <- 
    list(names(x[["Ranks"]]), c("General Dominance", "Standardized", "Ranks"))
  
  print(Display_Std)
  
  cat("\n")
  
  if (length(x[["Conditional_Dominance"]] > 0)) {
    
    cat("Conditional Dominance Statistics:\n")
    
    colnames(x[["Conditional_Dominance"]]) <- 
      paste("IVs:", 1:ncol(x[["Conditional_Dominance"]]))
    
    print(x[["Conditional_Dominance"]])
    
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
  
  # if (length(x[["Subset_Details"]][["Set"]]) > 0) {
  # 
  #   cat("Components of Set:\n")
  # 
  #   for (set in 1:length(x[["Subset_Details"]][["Set"]])) {
  # 
  #     cat(paste0("set", set),":", deparse(x[["Subset_Details"]][["Set"]][[set]]), "\n")
  # 
  #   }
  # 
  #   cat("\n")
  # 
  # }
  
  # if (length(x[["Subset_Details"]][["All"]]) > 0) {
  #   
  #   cat(".all subset variables:", x[["Subset_Details"]][["All"]])
  #   
  # }
  
  invisible(x)
  
}


#' @title Summary method for \code{domin}
#' @description Reports dominance designation results from the \code{domin} class object.
#' @param object an object of class "domin".
#' @param ... further arguments passed to or from other methods. Not used currently.
#' @return The originally submitted "domin" object with an additional \code{Strongest_Dominance} element added.
#' \describe{
#'  \item{\code{Strongest_Dominance}}{Matrix comparing the independent variable in the first row to the independent variable in the third row.  The second row denotes the strongest designation between the two independent variables.}
#' }
#' @details The summary method for class \code{domin} is used for obtaining the strongest dominance designations (i.e., general, Conditional, or Complete) among the independent variables.
#' @exportS3Method

summary.domir <- function(object, ...) {
  
  if (length(object[["Strongest_Dominance"]]) == 0) {
    
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
                     "Completely dominates",
                     "is Completely dominated by")
            
            next
            
          }
          
        }
        
        if (length(object[["Conditional_Dominance"]] > 0)) {
          
          if (all(object$Conditional_Dominance[IV1,] > object$Conditional_Dominance[IV2,])) { 
            
            pairs[2, location] <- "conditionally dominates"
            
            next
            
          }
          
          else if (all(object$Conditional_Dominance[IV1,] < object$Conditional_Dominance[IV2,])) {
            
            pairs[2, location] <- "is conditionally dominated by"
            
            next
            
          }
          
        }
        
        pairs[2, location] <- 
          ifelse(object$General_Dominance[[IV1]] > object$General_Dominance[[IV2]], 
                 "generally dominates", 
                 ifelse(object$General_Dominance[[IV1]] < object$General_Dominance[[IV2]],
                        "is generally dominated by", 
                        "has no dominance designation with"))
        
      }
      
    }
    
    rownames(pairs) <- rep("", times = 3)
    
    colnames(pairs) <- rep("", times = ncol(pairs))
    
    res <- append(object, list(Strongest_Dominance = pairs))
    
    class(res) <- c("domin", "list")
    
    return(res)
    
  }
  
  else return(object)
  
}
