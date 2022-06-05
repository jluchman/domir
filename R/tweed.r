#' @title The Working Elastic Edition of Dominance analysis (tweed)
#'
#' @name tweed
#'
#' @description Computes dominance statistics for predictive modeling 
#' functions that accept a `formula`, `Formula`, or `list`. 
#' 
#' @param .obj An object of class [formula], [Formula], or [list]. 
#' 
#' The object submitted to the `.obj` argument will be used to determine 
#' the sub-models that will be used in the dominance analysis.
#' 
#' `formula`-class entries to `.obj` must include the all independent 
#' variables to be used in the dominance analysis.  Each independent variable 
#' separated by `+` will be used as an additional subsettor and will contribute 
#' to the total number of models estimated.  A response is not required.
#' 
#' All methods for `tweed` will submit an object of the same type to the 
#' function in `.fct` as the first, unnamed argument.
#' 
#' All methods' default behavior of using all entries as separate subsettors 
#' can be overriden using `.set`, `.all`, and `.adj` arguments. 
#' 
#' @param .fct A function.
#' 
#' Controls the predictive modeling function used in the dominance analysis. 
#' 
#' @param .set A list with each element comprised of elements of the same class 
#' as `.obj`.
#' 
#' The independent variables referred to in element of the list submitted to 
#' `.set` combine elements present in `.obj` by binding them into 
#' inseparable set in the dominance analysis. All bound independent variables 
#' are considered a single subsettor.
#' 
#' @param .all An entry with the same class as `.obj`.
#' 
#' The independent variables referred to in this entry are not used as 
#' subsettors.  Rather the value associated with these terms is removed from 
#' the dominance analysis.  The user can think about the independent 
#' variables referred to in this vector is used like a set of covariates.
#' 
#' The entries in \code{.all} are removed from and considered an additional component that explains the fit metric.  As a result, the general dominance statistics will no longer sum to the overall fit metric and the standardized vector will no longer sum to 1.
#' 
#' @param .adj A vector of variable/factor names, \code{formula} coercible strings, or other formula terms (i.e., 1 to indicate an intercept).  The entries in this vector are concatenated (when of length > 1) and, like the entries of \code{.all}, are not used in the dominance analysis; this vector is used as an adjustment to the baseline value of the overall fit statistic.  
#' 
#' The use of \code{consmodel} changes the interpretation of the the general and Conditional dominance statistics.  When \code{consmodel} is used, the general and Conditional dominance statistics are reflect the difference between the constant model and the overall fit statistic values.
#'  
#' Typical usage of \code{consmodel} is to pass "1" to set the intercept as the baseline and control for its value when the baseline model's fit statistic value is not 0 (e.g., if using the AIC or BIC as a fit statistic; see examples).
#' 
#' @param .cdl Logical.  If \code{FALSE} then Conditional dominance matrix is not computed.
#' 
#' If Conditional dominance is not desired as an importance criterion, avoiding computing the Conditional dominance matrix can save computation time.
#' 
#' @param .cpt Logical.  If \code{FALSE} then Complete dominance matrix is not computed.
#' 
#' If Complete dominance is not desired as an importance criterion, avoiding computing Complete dominance designations can save computation time.
#' 
#' As such, this vector is used to set a baseline for the fit statistic when it is non-0. 
#' @param .rev Logical. If \code{TRUE} then standardized vector, ranks, and Complete dominance Designations are reversed in their interpretation.  
#' 
#' This argument should be changed to \code{TRUE} if the fit statistic used decreases with better fit to the data (e.g., AIC, BIC). 
#' @param ... Passes arguments to other methods.
#'
#' @return Returns an object of \code{\link{class}} "domin".
#' An object of class "domin" is a list composed of the following elements:
#' \describe{
#'  \item{\code{General_Dominance}}{Vector of general dominance statistics.}
#'  \item{\code{Standardized}}{Vector of general dominance statistics normalized to sum to 1.}
#'  \item{\code{Ranks}}{Vector of ranks applied to the general dominance statistics.}
#'  \item{\code{Conditional_Dominance}}{Matrix of Conditional dominance statistics.  Each row represents a term; each column represents an order of terms.}
#'  \item{\code{Complete_Dominance}}{Logical matrix of Complete dominance designations. The term represented in each row indicates dominance status; the terms represented in each columns indicates dominated-by status.}
#'  \item{\code{Fit_Statistic_Overall}}{Value of fit statistic for the full model.}
#'  \item{\code{Fit_Statistic_All_Subset}}{Value of fit statistic associated with terms in \code{.all}.}
#'  \item{\code{Fit_Statistic_Constant_Model}}{Value of fit statistic associated with terms in \code{consmodel}.}
#'  \item{\code{Call}}{The matched call.}
#'  \item{\code{Subset_Details}}{List containing the full model and descriptions of terms in the full model by source.}
#' }
#'
#' @details \code{domin} automates the computation of all possible combination of entries to the dominance analysis (DA), the creation of \code{formula} objects based on those entries, the modeling calls/fit statistic capture, and the computation of all the dominance statistics for the user.
#' 
#' \code{domin} accepts only a "deconstructed" set of inputs and "reconstructs" them prior to formulating a coherent predictive modeling call.
#' 
#' One specific instance of this deconstruction is in generating the number of entries to the DA. The number of entries is taken as all the \code{terms} from \code{.obj} and the separate list element vectors from \code{.set}. The entries themselves are concatenated into a single formula, combined with the entries in \code{.all}, and submitted to the predictive modeling function in \code{reg}.  Each different combination of entries to the DA forms a different \code{formula} and thus a different model to estimate.
#' 
#' For example, consider this \code{domin} call:
#' 
#' \code{domin(y ~ x1 + x2, lm, list(summary, "r.squared"), .set = list(c("x3", "x4")), .all = c("c1", "c2"), data = mydata))}
#' 
#' This call records three entries and results in seven (i.e., \eqn{2^3 - 1}) different combinations:
#' 
#' \enumerate{
#' \item x1
#' \item x2
#' \item x3, x4
#' \item x1, x2
#' \item x1, x3, x4
#' \item x2, x3, x4
#' \item x1, x2, x3, x4
#' }
#' 
#' \code{domin} parses \code{.obj} to obtain all the terms in it and combines them with \code{.set}.  When parsing \code{.obj}, only the processing that is available in the \code{stats} package is applied.  Note that \code{domin} is not programmed to process terms of order > 1 (i.e., interactions/products) appropriately (i.e., only include in the presence of lower order component terms).
#' 
#' From these combinations, the predictive models are constructed and called. The predictive model call includes the entries in \code{.all}, applies the appropriate formula, and reconstructs the function itself. The seven combinations above imply the following series of predictive model calls:
#' 
#' \enumerate{
#' \item \code{lm(y ~ x1 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x2 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x3 + x4 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x1 + x2 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x1 + x3 + x4 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x2 + x3 + x4 + c1 + c2, data = mydata})
#' \item \code{lm(y ~ x1 + x2 + x3 + x4 + c1 + c2, data = mydata})
#' }
#'
#' It is possible to use a \code{domin} with only set (i.e., no IVs in \code{.obj}; see examples below). There must be at least two entries to the DA for \code{domin} to run.
#'
#' all the called predictive models are submitted to the fit extractor function implied by the entries in \code{fitstat}. Again applying the example above, all seven predictive models' objects would be individually passed as follows:
#' 
#' \code{summary(lm_obj)["r.squared"]}
#' 
#' where \code{lm_obj} is the model object returned by \code{lm}. 
#' 
#' The entries to \code{fitstat} must be as a list and follow a specific structure: 
#' \code{list(fit_function, element_name, ...)}
#' \describe{
#'  \item{\code{fit_function}}{First element and function to be applied to the object produced by the \code{reg} function}
#'  \item{\code{element_name}}{Second element and name of the element from the object returned by \code{fit_function} to be used as a fit statistic.  The fit statistic must be scalar-valued/length 1}
#'  \item{\code{...}}{Subsequent elements and are additional arguments passed to \code{fit_function}}
#' }
#' 
#' In the case that the model object returned by \code{reg} includes its own fit statistic without the need for an extractor function, the user can apply an anonymous function following the required format to extract it.
#' 
#' @export
#' @examples
#' ## Basic linear model with r-square
#' 
#' domin(mpg ~ am + vs + cyl, 
#'   lm, 
#'   list("summary", "r.squared"), 
#'   data = mtcars)
#' 
#' 
#' ## Linear model including set
#' 
#' domin(mpg ~ am + vs + cyl, 
#'   lm, 
#'   list("summary", "r.squared"), 
#'   data = mtcars, 
#'   .set = list(c("carb", "gear"), c("disp", "wt")))
#'
#'
#' ## Multivariate linear model with custom multivariate r-square function 
#' ## and all subset variable
#' 
#' Rxy <- function(obj, names, data) {
#'    return(list("r2" = cancor(predict(obj), 
#'        as.data.frame(mget(names, as.environment(data))))[["cor"]][1]^2)) 
#'        }
#'        
#' domin(cbind(wt, mpg) ~ vs + cyl + am, 
#'   lm, 
#'   list("Rxy", "r2", c("mpg", "wt"), mtcars), 
#'   data = mtcars, 
#'   .all = c("carb"))
#'
#'
#' ## Set only
#' 
#' domin(mpg ~ 1, 
#'   lm, 
#'   list("summary", "r.squared"), 
#'   data = mtcars, 
#'   .set = list(c("am", "vs"), c("cyl", "disp"), c("qsec", "carb")))
#'   
#' ## Constant model using AIC
#' 
#' domin(mpg ~ am + carb + cyl, 
#'   lm, 
#'   list(function(x) list(aic = extractAIC(x)[[2]]), "aic"), 
#'   data = mtcars, 
#'   .rev = TRUE, consmodel = "1")

tweed <- function(.obj, ...) {
  
  UseMethod("tweed")
  
}

# ~~ bring back argument passing ... ~~ ----

#' @rdname tweed
#' @exportS3Method 
tweed.formula <- function(
    .obj, .fct,
    .set = NULL, .all = NULL, .adj = NULL, 
    .cdl = TRUE, .cpt = TRUE, .rev = FALSE, 
    ...) {
  
  # Process formula ----
  # obtain IV name vector from `.obj`
  Indep_Vars <- attr(stats::terms(.obj), "term.labels") 
  
  # Intercept flag
  Intercept <- as.logical(attr(stats::terms(.obj), "intercept") ) 
  
  # Obtain DV (if one is included)
  Dep_Var <- 
    switch(sign(attr(stats::terms(.obj), "response")) + 1,
           NULL, 
           attr(
             stats::terms(.obj), "variables"
           )[[attr(stats::terms(.obj), "response") + 1]]
    )
  
  # Process .all ----
  if (!is.null(.all)) {
    
    if (!inherits(.all, "formula")) {
      stop("'.all' argument must be a formula.", call. = FALSE)
    }
    
    if (attr(stats::terms(.all), "response") == 1) {
      
      stop("Formulas in '.all' argument must not have a",
           "response/left hand side.", call. = FALSE)
    }
    
    # gather IVs from each set
    all_processed <- attr(stats::terms(.all), "term.labels")
    
    # remove IVs from `Indep_Vars` list if in `.set`
    all_remove_loc <- 
      unlist(lapply(all_processed, 
                    function(x) which(Indep_Vars %in% x)))
    
    if (length(all_remove_loc) != length(unlist(all_processed))) {
      
      wrong_all_terms <- 
        paste(
          unlist(all_processed)[
            which(!(unlist(all_processed) %in% 
                      Indep_Vars))], 
          collapse = " ")
      
      stop("Terms ", wrong_all_terms,
           " in '.all' argument do not match any independent variables ", 
           "in formula.", 
           call. = FALSE)
    }
    
    Indep_Vars <- Indep_Vars[-all_remove_loc]
    
  }
  
  else all_processed <- NULL
  
  # Process .adj ----
  #TBD - note consmodel argument depreciation
  
  # Process .set ----
  if (!is.null(.set)) {
    
    if (!is.list(.set)) {
      stop("'.set' argument must be submitted as list.", call. = FALSE)
    }
    
    if (!all(sapply(.set, inherits, "formula"))) {
      
      elements_not_formula <- 
        paste(which((!sapply(.set, inherits, "formula"))), 
              collapse = " ")
      
      stop("Each element of list in '.set' must be a formula.\n", 
           "Elements ", elements_not_formula, 
           " are of different class.", call. = FALSE)
    }
    
    if (any(sapply(.set, function(x) attr(stats::terms(x), "response") == 1))) {
      
      elements_has_response <- 
        paste(which(sapply(.set, 
                           function(x) attr(terms(x), "response")) > 0 ), 
              collapse = " ")
      
      stop("Formulas in '.set' argument must not have ",
           "responses/left hand sides.\n",
           "Elements ", elements_has_response, " have responses.", call. = FALSE)
    }
    
    # gather IVs from each set
    set_processed <- 
      lapply(.set, 
             function(x) attr(stats::terms(x), "term.labels"))
    
    # remove IVs from `Indep_Vars` list if in `.set`
    set_remove_loc <- 
      unlist(lapply(set_processed, 
                    function(x) which(Indep_Vars %in% x)))
    
    if (length(set_remove_loc) != length(unlist(set_processed))) {
      
      wrong_set_terms <- 
        paste(
          unlist(set_processed)[
            which(!(unlist(set_processed) %in% 
                      Indep_Vars))], 
          collapse = " ")
      
      stop("Terms ", wrong_set_terms,
           " in '.set' argument do not match any independent variables ", 
           "in formula.", 
           call. = FALSE)
    }
    
    Indep_Vars <- Indep_Vars[-set_remove_loc]
    
    # apply names to .set
    if (!is.null(names(.set)))    
      set_names <- names(.set)
    else set_names <- paste0("set", 1:length(.set))
    
    missing_set_names <- which(set_names == "")
    
    if (length(missing_set_names) > 0)
      set_names[missing_set_names] <- paste0("set", missing_set_names)
    
    if (any(set_names %in% Indep_Vars)) {
      repeat_names <- set_names[which(set_names %in% Indep_Vars)]
      
      stop("Set names ",
           paste(repeat_names, collapse = " "), 
           " are also the names of indiviual independent variables.\n",
           "Please rename these .set.", call. = FALSE)
    }
    
  }
  
  else set_processed <- NULL
  
  # Total subsetors count
  Total_Indep_Vars <- length(Indep_Vars) + length(set_processed)
  
  # Too few subsettors error
  if (Total_Indep_Vars < 2) 
    stop("At least two independent variables or .set needed for ", 
         "a dominance analysis.", call. = FALSE)
  
  # Define function to call regression models ----
  
  # assumes receipt of a logical matrix indicating which IVs to use as 
  # well as IV list and DV also regression function, metric function, 
  # with all and consmodel arguments along with an intercept argument 
  # for reformulate
  doModel_Fit <- 
    function(Indep_Var_Combin_lgl, Indep_Vars, Dep_Var, 
             reg, all, consmodel, intercept, addl_args) { 
      
    Indep_Var_Combination <- unlist(Indep_Vars[Indep_Var_Combin_lgl]) # select vector of IVs
    
    formula_to_use <-
      stats::reformulate(c(Indep_Var_Combination, all, consmodel), # 'reformulate' formula to submit to .fct
                         response = Dep_Var, intercept = intercept)
    
    Fit_Value <- do.call(reg, append(formula_to_use, addl_args) ) 
    
    return(Fit_Value)
    
    }
  
  # here ----
  # ~~ test inputs then submit the results of the full test as an entry ~~

  # Check '.fct' inputs ----
    test_model <- 
    tryCatch(do.call(eval(.fct), append(.obj, list(...))), 
             error = function(err) 
               stop("'.fct' produced an error when ", 
                    "applied to '.obj'.", call. = FALSE))
  
  if (!is.numeric(test_model) && length(test_model) != 1) 
    stop("result of '.fct' is not a numeric, scalar/",
         "length of 1 value.", call. = FALSE)
  
  ## ~~ build test_metric_sub into domin_scalar call ----
  
  # Define submission to `domin_scalar` ----
  
  # here3 ----
  # fix and test inputs for pre and post linking functions
  component_list <- 
    list(fitting_fun = doModel_Fit, 
         args_list = 
           list(Indep_Vars = append(Indep_Vars, set_processed), 
                Dep_Var = deparse(Dep_Var), #this has to be optional - does null deparse right?
                reg = .fct,
                all = all_processed, 
                consmodel = .adj, 
                intercept = Intercept,
                addl_args = list(...)), 
         cons_args = 
           list(Indep_Vars = Indep_Vars, 
                Dep_Var = deparse(Dep_Var), #this has to be optional - does null deparse right?
                reg = .fct,
                all = NULL, 
                consmodel = .adj, 
                intercept = Intercept, 
                addl_args = list(...)),
         Total_Combination_N = Total_Indep_Vars) # this should include `doModel_fit` and arguments to it -- only pass on args directly relevant to `domin_scalar` all other stuff should be passed as encapsulated arguments that `fitting_fun` can 
  # here3 ----
  
  return_list <- domin_scalar(component_list, .cdl, .cpt, .rev)
  
  # Finalize returned values and attributes ----
  
  if (is.null(.set)) IV_Labels <- Indep_Vars
  else IV_Labels <- c(Indep_Vars, set_names)
  
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
    "Call" = match.call() #,
    # "Subset_Details" = list(
    #   "Full_Model" = 
    #     stats::reformulate(c(Indep_Vars, .all, .adj), 
    #                        response = Dep_Var, intercept = Intercept),
    #   "Formula" = attr(stats::terms(.obj), "term.labels"),
    #   "All" = .all,
    #   "Set" = .set,
    #   "Constant" = .adj
    # )
  )
  
  # apply class 'domin'
  class(return_list) <- c("domin") 
  
  return(return_list)
  
}

#' @rdname tweed
#' @exportS3Method 
tweed.Formula <- function(.obj, reg, fitstat, .set = NULL, .all = NULL, 
                          .cdl = TRUE, .cpt = TRUE, consmodel = NULL, .rev = FALSE, ...) {
  
  stop("'tweed' 'Formula' method not yet implemented.", call. = FALSE)
  
}

#' @rdname tweed
#' @exportS3Method 
tweed.list <- function(.obj, reg, fitstat, .set = NULL, .all = NULL, 
                       .cdl = TRUE, .cpt = TRUE, consmodel = NULL, .rev = FALSE, ...) {
  
  stop("'tweed' 'list' method not yet implemented.", call. = FALSE)
  
}

#' @title Print method for \code{domin}
#' @description Reports formatted results from \code{domin} class object.
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

print.tweed <- function(x, ...) {
  
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

summary.tweed <- function(object, ...) {
  
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
