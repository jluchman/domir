#' Dominance analysis supporting \code{formula}-based modeling functions
#'
#' Computes dominance statistics for predictive modeling functions that accept a \code{\link{formula}}.
#' 
#' @param formula_overall An object of class \code{\link{formula}} or that can be coerced to class \code{formula} for use in the modeling function in \code{reg}.  The \code{\link{terms}} on the right hand side of this formula are used as separate entries to the dominance analysis.
#' 
#' A valid \code{formula_overall} entry is necessary, even if only submitting entries in \code{sets}, to define a valid left hand side of the prediction equation (see examples).  The function called in \code{reg} must accept one or more responses on the left hand side.
#' @param reg A function implementing the predictive (or "reg"ression) model called. 
#' 
#' String function names (e.g., "lm"), function names (e.g., \code{lm}), or anonymous functions (e.g., \code{function(x) lm(x)}) are acceptable entries.  This argument's contents are passed to \code{\link{do.call}} and thus any function call \code{do.call} would accept is valid.
#' 
#' The predictive model in \code{reg} must accept a \code{formula} object as its first argument or must be adapted to do so with a wrapper function.
#' @param fitstat List providing arguments to call a fit statistic extracting function (see details). The \code{fitstat} list must be of at least length two. 
#' 
#' The first element of \code{fitstat} must be a function implementing the fit statistic extraction. String function names (e.g., "summary"), function names (e.g., \code{summary}), or anonymous functions (e.g., \code{function(x) summary(x)}) are acceptable entries. This element's contents are passed to \code{\link{do.call}} and thus any function call \code{do.call} would accept is valid.
#' 
#'The second element of \code{fitstat} must be the named element of the list or vector produced by the fit extractor function called in the first element of \code{fitstat}.  This element must be a string (e.g., "r.squared").
#' 
#' All list elements beyond the second are submitted as additional arguments to the fit extractor function call.
#' 
#' The fit statistic extractor function in the first list element of \code{fitstat} must accept the model object produced by the predictive modeling function in \code{reg} as its first argument or be adapted to do so with a wrapper function.
#' 
#' The fit statistic produced must be scalar valued (i.e., vector of length 1).
#' @param sets A list with each element comprised of vectors containing variable/factor names or \code{formula} coercible strings. 
#' 
#' Each separate list element-vector in \code{sets} is concatenated (when the list element-vector is of length > 1) and used as an entry to the dominance analysis along with the terms in \code{formula_overall}.
#' @param all A vector of variable/factor names or \code{formula} coercible strings.  The entries in this vector are concatenated (when of length > 1) but are not used in the dominance analysis.  Rather the value of the fit statistic associated with these terms is removed from the dominance analysis; this vector is used like a set of covariates.
#' 
#' The entries in \code{all} are removed from and considered an additional component that explains the fit metric.  As a result, the general dominance statistics will no longer sum to the overall fit metric and the standardized vector will no longer sum to 1.
#' @param complete Logical.  If \code{FALSE} then complete dominance matrix is not computed.
#' 
#' If complete dominance is not desired as an importance criterion, avoiding computing complete dominance designations can save computation time.
#' @param consmodel A vector of variable/factor names, \code{formula} coercible strings, or other formula terms (i.e., 1 to indicate an intercept).  The entries in this vector are concatenated (when of length > 1) and, like the entries of \code{all}, are not used in the dominance analysis; this vector is used as an adjustment to the baseline value of the overall fit statistic.  
#' 
#' The use of \code{consmodel} changes the interpretation of the the general and conditional dominance statistics.  When \code{consmodel} is used, the general and conditional dominance statistics are reflect the difference between the constant model and the overall fit statistic values.
#'  
#' Typical usage of \code{consmodel} is to pass "1" to set the intercept as the baseline and control for its value when the baseline model's fit statistic value is not 0 (e.g., if using the AIC or BIC as a fit statistic; see examples).
#' 
#' As such, this vector is used to set a baseline for the fit statistic when it is non-0. 
#' @param reverse Logical. If \code{TRUE} then standardized vector, ranks, and complete dominance Designations are reversed in their interpretation.  
#' 
#' This argument should be changed to \code{TRUE} if the fit statistic used decreases with better fit to the data (e.g., AIC, BIC). 
#' @param ... Additional arguments passed to the function call in the \code{reg} argument.
#'
#' @return Returns an object of \code{\link{class}} "domin".
#' An object of class "domin" is a list composed of the following elements:
#' \describe{
#'  \item{\code{General_Dominance}}{Vector of general dominance statistics.}
#'  \item{\code{Standardized}}{Vector of general dominance statistics normalized to sum to 1.}
#'  \item{\code{Ranks}}{Vector of ranks applied to the general dominance statistics.}
#'  \item{\code{Conditional_Dominance}}{Matrix of conditional dominance statistics.  Each row represents a term; each column represents an order of terms.}
#'  \item{\code{Complete_Dominance}}{Logical matrix of complete dominance designations. The term represented in each row indicates dominance status; the terms represented in each columns indicates dominated-by status.}
#'  \item{\code{Fit_Statistic_Overall}}{Value of fit statistic for the full model.}
#'  \item{\code{Fit_Statistic_All_Subsets}}{Value of fit statistic associated with terms in \code{all}.}
#'  \item{\code{Fit_Statistic_Constant_Model}}{Value of fit statistic associated with terms in \code{consmodel}.}
#'  \item{\code{Call}}{The matched call.}
#'  \item{\code{Subset_Details}}{List containing the full model and descriptions of terms in the full model by source.}
#' }
#'
#' @details \code{domin} automates the computation of all possible combination of entries to the dominance analysis (DA), the creation of \code{formula} objects based on those entries, the modeling calls/fit statistic capture, and the computation of all the dominance statistics for the user.
#' 
#' \code{domin} accepts only a "deconstructed" set of inputs and "reconstructs" them prior to formulating a coherent predictive modeling call.
#' 
#' One specific instance of this deconstruction is in generating the number of entries to the DA. The number of entries is taken as all the \code{terms} from \code{formula_overall} and the separate list element vectors from \code{sets}. The entries themselves are concatenated into a single formula, combined with the entries in \code{all}, and submitted to the predictive modeling function in \code{reg}.  Each different combination of entries to the DA forms a different \code{formula} and thus a different model to estimate.
#' 
#' For example, consider this \code{domin} call:
#' 
#' \code{domin(y ~ x1 + x2, lm, list(summary, "r.squared"), sets = list(c("x3", "x4")), all = c("c1", "c2"), data = mydata))}
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
#' \code{domin} parses \code{formula_overall} to obtain all the terms in it and combines them with \code{sets}.  When parsing \code{formula_overall}, only the processing that is available in the \code{stats} package is applied.  Note that \code{domin} is not programmed to process terms of order > 1 (i.e., interactions/products) appropriately (i.e., only include in the presence of lower order component terms).
#' 
#' From these combinations, the predictive models are constructed and called. The predictive model call includes the entries in \code{all}, applies the appropriate formula, and reconstructs the function itself. The seven combinations above imply the following series of predictive model calls:
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
#' It is possible to use a \code{domin} with only sets (i.e., no IVs in \code{formula_overall}; see examples below). There must be at least two entries to the DA for \code{domin} to run.
#'
#' All the called predictive models are submitted to the fit extractor function implied by the entries in \code{fitstat}. Again applying the example above, all seven predictive models' objects would be individually passed as follows:
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
#' ## Linear model including sets
#' 
#' domin(mpg ~ am + vs + cyl, 
#'   lm, 
#'   list("summary", "r.squared"), 
#'   data = mtcars, 
#'   sets = list(c("carb", "gear"), c("disp", "wt")))
#'
#'
#' ## Multivariate linear model with custom multivariate r-square function 
#' ## and all subsets variable
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
#'   all = c("carb"))
#'
#'
#' ## Sets only
#' 
#' domin(mpg ~ 1, 
#'   lm, 
#'   list("summary", "r.squared"), 
#'   data = mtcars, 
#'   sets = list(c("am", "vs"), c("cyl", "disp"), c("qsec", "carb")))
#'   
#' ## Constant model using AIC
#' 
#' domin(mpg ~ am + carb + cyl, 
#'   lm, 
#'   list(function(x) list(aic = extractAIC(x)[[2]]), "aic"), 
#'   data = mtcars, 
#'   reverse = TRUE, consmodel = "1")

domin <- 
  function(formula_overall, reg, fitstat, sets = NULL, all = NULL, 
           complete = TRUE, consmodel = NULL, reverse = FALSE, ...) {
    
# Initial exit/warning conditions ---- 
    
if (!methods::is(formula_overall, "formula")) 
  stop(paste(formula_overall, "is not a formula object.  Coerce it to formula before use in domin."))
    
if (!is.list(fitstat)) 
  stop("fitstat is not a list.  Please submit it as a list object.")
    
if (length(sets) > 0 & !is.list(sets)) 
  stop("sets is not a list.  Please submit it as a list object.")
    
if (is.list(all)) 
  stop("all is a list.  Please submit it as a vector.")
  
if (!attr(stats::terms(formula_overall), "response")) 
    stop(paste(deparse(formula_overall), "missing a response.  Please supply a valid response."))
  
if (any(attr(stats::terms(formula_overall), "order") > 1))
    warning(paste(deparse(formula_overall), "contains second or higher order terms. domin may not handle them correctly."))
    
if (length(fitstat) < 2) 
  stop("fitstat requires at least two elements.")
  
# Process variable lists ----
    
Indep_Vars <- 
    attr(stats::terms(formula_overall), "term.labels") # obtain IV name vector from `formula_overall`

intercept <- as.logical(attr(stats::terms(formula_overall), "intercept") ) # does the model have an intercept?  Needed for `reformulate`

if (length(sets) > 0) { # if there are sets...
    
    set_aggregated <- 
        sapply(sets, paste0, collapse=" + ") # ...paste together IV names from each set in `formula` format as a vector...
    
    Indep_Vars <- 
        append(Indep_Vars, set_aggregated) # ...append sets vector to end of IV name vector
    
}

Dep_Var <- 
    attr(stats::terms(formula_overall), "variables")[[2]] # pull out DV name from `formula_overall`

Total_Indep_Vars <- length(Indep_Vars) # count number of IVs and sets in model

    ## IV-based exit conditions ----
if (Total_Indep_Vars < 2) 
    stop(paste("Total of", Total_Indep_Vars, "independent variables or sets. At least 2 needed for useful dominance analysis."))

# Create independent variable/set combination list ----
    
Combination_List <- 
    lapply( (1:length(Indep_Vars)), # Repeating over different numbers of IVs chosen at once in the model...
            function(Number_in_Combo) {
                utils::combn(Indep_Vars, Number_in_Combo) # ...obtain all combinations choosing a considering a specific number of IVs chosen given the entire IV name vector
            } 
    )

Total_Models_to_Estimate <- 2**Total_Indep_Vars - 1 # total number of models to estimate

# Define function to call regression models ----

# function to call regression models for modeling
doModel_Fit <- function(Indep_Var_Combination, Dep_Var, 
                        reg, fitstat, all = NULL, consmodel = NULL, intercept, ...) {

    formula_to_use <- 
        # formula( # build formula to submit to modeling function by...
        #     paste0(deparse(Dep_Var), " ~ ", # ...combining the DV with...
        #            paste0(c(Indep_Var_Combination, all), collapse = " + " )) #...the set of IVs submitted 
        # )
      stats::reformulate(c(Indep_Var_Combination, all, consmodel), 
                  response = Dep_Var, intercept = intercept)

    Model_Result <- 
        list( # capture data from the called model as a list...
            do.call(reg, list(formula_to_use, ...) ) # ...`do.call` modeling function with formula and all other arguments
        ) 
    
    if (length(fitstat) > 2) # if there are additional arguments to pass to the fitstat function, indicated by having length of > 2 for this list...
        Model_Result <- 
            append(Model_Result, fitstat[3:length(fitstat)]) # ...append these additional arguments to `temp_result`
    
    Fit_Value <- do.call(fitstat[[1]], Model_Result) # use first entry of `fitstat` as fitstat function name, use `Model_Result` as results to submit to it
    
    return( 
        list( # `doModel_Fit` then returns (as list)...
            "names" = Indep_Var_Combination, # ... the combo of IVs used ...
            "value" = Fit_Value[[ fitstat[[2]] ]] # ... and uses second, necessarily named, argument of `fitstat` to select the result of `Fit_Value` to return
        )
    )

}

# Constant model adjustments ----

if (length(consmodel) > 0) { # if there are entries in consmodel...
  Cons_Result <- 
    doModel_Fit(NULL, consmodel = consmodel, Dep_Var, reg, fitstat, intercept = intercept, ...) # ...obtain their `fitstat` value...
  FitStat_Adjustment <- Cons_Result[["value"]] # ...and add the value as the adjustment to the fitstat
}

else {
  Cons_Result <- NULL # ...otherwise return a null
  FitStat_Adjustment <- 0
}

# All subsets adjustment ----

if (length(all) > 0) { # if there are entries in all...
  All_Result <- 
    doModel_Fit(NULL, all = all, consmodel = consmodel, Dep_Var, reg, fitstat, intercept = intercept, ...) # ...obtain their `fitstat` value as well...
  FitStat_Adjustment <- 
    All_Result[["value"]] # ...and log the value as the adjustment to the fitstat (replacing consmodel)
}

else All_Result <- NULL # ...otherwise return a null

# Obtain all subsets regression results ----

# 'Ensemble_of_Models' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific result from `do.call`

# low-level function to identify the specific set of IVs to submit to `doModel_Fit` - called by `doModel_Coordinator`
doModel_ListSelector <- function(Indep_Vars_Chosen, Number_of_Indep_Vars) { 
    
    doModel_Fit(
        Combination_List[[Number_of_Indep_Vars]][, Indep_Vars_Chosen], # From the list at a specific number of IVs in the model, choose one unique combination (which is associated with the columns of the matrices returned by `combn`)...
        Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...) # ...and submit all other pertinent information for model fitting - other names assumed pulled from parent env scope
    # ensemble_begin = ensemble_end # update where the ensemble tracker will begin for next round
    
}

# high-level function to coordinate listing of models within a specific number of IVs
doModel_Coordinator <- function(Number_of_Indep_Vars) { 
    
    lapply(1:ncol(Combination_List[[Number_of_Indep_Vars]]), # list apply `doModel_ListSelector` over all combinations of IVs at a single number of IVs in the model
           doModel_ListSelector, Number_of_Indep_Vars) 
    
}

# list apply `doModel_Coordinator` across all numbers of IVs in the model
Ensemble_of_Models <- # as a list ...
  lapply(1:Total_Indep_Vars, doModel_Coordinator) #... call `doModel_Coordinator` over all numbers of IVs in the model


# Process all subsets - find the increments ----

# low-level function to compute model fit increments related to IVs 
    #called by Ensemble_Fitstat_domIncrementor below
Identify_domIncrement <- function (IVs, IVs_previous) { 
    
    if (all(is.na(unlist(IVs_previous)))) # if there is are all NAs in the previous IVs (i.e., by design in models with 1 IV)... 
        
        value <- list(names_curr = IVs[["names"]], # include IV names at focal ...
                      names_prev = "", # ...IV names at one less (which are an empty string...
                      increment = IVs[["value"]] - FitStat_Adjustment) # ...and the increment to the fit metric
    
    else if ( length(intersect(IVs[["names"]], IVs_previous[["names"]])) == #... models in previous are all in current - valid increment ...
              length(IVs_previous[["names"]]) ) 
        
        value <- list(names_curr = IVs[["names"]], # include IV names at focal ...
                      names_prev = IVs_previous[["names"]], # ...IV names at one less...
                      increment = IVs[["value"]] - IVs_previous[["value"]]) # ...and the increment to the fit metric
    
    else value <- NULL # ... otherwise the models in previous are not all in current - invalid increment; return a NULL
    
    return(value)
    
}

# mid-level function to coordinate finding all valid fitstat increments 
    # called by Prepare_domList below
Ensemble_Fitstat_domIncrementor <- 
    function(List_of_Models, List_of_Models_Previous) {
    
    domIncrement_Lists <-
        mapply(Identify_domIncrement,  # call Identify_domIncrement ...
               List_of_Models, List_of_Models_Previous, #... submit candidate models for current IV and one less IV
               SIMPLIFY = FALSE) # do not simplify object to non-list/retain list type
    
    Null_Elements <- which(sapply(domIncrement_Lists, is.null)) # identify NULL list elements
    
    if (length(Null_Elements) > 0) # if there are NULL elements...
        domIncrement_Lists <- 
            domIncrement_Lists[ -Null_Elements ] # ... remove them before returning

    return( domIncrement_Lists )

}

# top-level function to prepare model lists for finding increments
Prepare_domList <- function(Number_of_Indep_Vars) {
    
    if (Number_of_Indep_Vars > 1) Previous_Models <- # if a list of multi_IV models ...
            Ensemble_of_Models[[Number_of_Indep_Vars-1]] #... obtain the list of models at one less IV
    else Previous_Models <- NA #... otherwise this is the 1 IV list - there are no models at one less.
    
    Current_Models <- Ensemble_of_Models[[Number_of_Indep_Vars]] # collect models at current numbers of IVs
    
    Current_Models_Length <- length(Current_Models) # record number of models at current IVs
    Previous_Models_Length <- length(Previous_Models) # record number of models at one less IVs
    
    Current_Models <- # "spread" current models at number of previous models to find combinations
        rep(Current_Models, each=Previous_Models_Length)
    
    Previous_Models <- # repeat one less IV models at number of current models to find combinations
        rep(Previous_Models, times=Current_Models_Length)
    
    return( Ensemble_Fitstat_domIncrementor( # submit all combinations to Ensemble_Fitstat_domIncrementor
        Current_Models, Previous_Models) )
}


Model_List <- lapply(1:length(Ensemble_of_Models), Prepare_domList) # for all numbers of IVs in model

# 'Model_List' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific increment's information (full_model, reduced_model, fit metric difference)


# Obtain conditional dominance statistics ----

Conditional_Dominance <- matrix(nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # conditional dominance container

Identify_domCondit <- function (focalIV, CandidateIncrement) { 
  
  if (is.element(focalIV, CandidateIncrement[["names_curr"]]) &
      !is.element(focalIV, CandidateIncrement[["names_prev"]]) )
    value <- CandidateIncrement[["increment"]]

  else value <- NA 
  
  return(value)
  
}

for (IV_Location in 1:Total_Indep_Vars) {
  
  IV_name <- Model_List[[1]][[IV_Location]][["names_curr"]]
  
  for (numIndepVars in 1:Total_Indep_Vars) {
    
    Conditional_Dominance[IV_Location, numIndepVars] <- 
      mean(sapply(Model_List[[numIndepVars]], function(Indep_Var) 
        Identify_domCondit(IV_name, Indep_Var)), na.rm=TRUE)
    
  }
       
}

# Obtain complete dominance statistics ----

Identify_domComplt <- function (Increment, focalIV, compIV) { 
  
  if ((is.element(focalIV, Increment[["names_curr"]])) & 
      (!is.element(focalIV, Increment[["names_prev"]])) & 
      ( (!is.element(compIV, c(Increment[["names_prev"]], 
                               Increment[["names_curr"]]) )) & 
        (length(Increment[["names_curr"]]) > 1) ))
    value <- Increment
  
  else if ( (is.element(focalIV, Increment[["names_curr"]])) & # marked not covered in tests but clearly executes
            (length(Increment[["names_curr"]]) == 1) )
    value <- Increment
  
  else value <- NA # marked not covered in tests but clearly executes
  
  return(value)
  
}

# mid
domComplt_Comparator <- function(focal_model, comp_model, focalIV, compIV) {
  
  if ( setequal(focal_model[["names_prev"]], comp_model[["names_prev"]]) && 
       (setdiff(focal_model[["names_curr"]], focal_model[["names_prev"]])==focalIV) &&
       (setdiff(comp_model[["names_curr"]], comp_model[["names_prev"]])==compIV) )
    value <- focal_model[["increment"]] > comp_model[["increment"]]
    
  else if (length(focal_model[["names_curr"]])==1) 
    value <- focal_model[["increment"]] > comp_model[["increment"]]
  
  else value <- NA
  
  if (!is.na(value) && focal_model[["increment"]] == comp_model[["increment"]]) # if fit metrics identical - they're NA
    value <- NA
  
  return(value)
  
}

# intention find IV1's increments - find IV2's increments - return logical vector of comparisons
Prepare_domComplt <- function(IncrementList, focalIV, compIV) {
  
  relevantIncs <- lapply(IncrementList, Identify_domComplt, focalIV=focalIV, compIV=compIV)
  
  relevantIncs2 <- lapply(IncrementList, Identify_domComplt, focalIV=compIV, compIV=focalIV)
  
  Null_Elements <- which(is.na(relevantIncs)) # identify NULL list elements
  
  if (length(Null_Elements) > 0) # if there are NULL elements...
    relevantIncs <- relevantIncs[ -Null_Elements ] # ... remove them before returning
  
  Null_Elements2 <- which(is.na(relevantIncs2)) # identify NULL list elements
  
  if (length(Null_Elements2) > 0) # if there are NULL elements...
    relevantIncs2 <- relevantIncs2[ -Null_Elements2 ] # ... remove them before returning
  
  Focal_Models_Length <- length(relevantIncs) # record number of models at current IVs
  Comp_Models_Length <- length(relevantIncs2) # record number of models at one less IVs
  
  Focal_Models <- # "spread" current models at number of previous models to find combinations
    rep(relevantIncs, each=Comp_Models_Length)
  
  Comp_Models <- # repeat one less IV models at number of current models to find combinations
    rep(relevantIncs2, times=Focal_Models_Length)
  
  Focal_Comp_logi <- mapply(domComplt_Comparator, 
                            focal_model=Focal_Models, comp_model=Comp_Models, 
                            focalIV=focalIV, compIV=compIV,
                            SIMPLIFY = TRUE)
  
  Null_Elements3 <- which(is.na(Focal_Comp_logi)) # identify NULL list elements
  
  if (length(Null_Elements3) > 0) # if there are NULL elements...
    Focal_Comp_logi <- Focal_Comp_logi[ -Null_Elements3 ] # ... remove them before returning
  
  #return(sum(Focal_Comp_logi))
  compile_compare <- ifelse(length(Focal_Comp_logi) == 0, 
                            NA, ifelse(all(Focal_Comp_logi),
                                       TRUE, ifelse(all(!Focal_Comp_logi), 
                                                    FALSE, NA)))  #ensure that mixes of F & T get correctly classified
  return(compile_compare)
  
}

if (complete) {
  
  Complete_Dominance <- 
    matrix(data=NA, nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # complete dominance container
  
  for (IV_Col in 1:(Total_Indep_Vars-1)) {

    Row_loc <- IV_Col + 1
    
    IV_Col_Name <- Indep_Vars[[IV_Col]]
    
    for (IV_Row in Row_loc:Total_Indep_Vars) {
      
      IV_Row_Name <- Indep_Vars[[IV_Row]]
      
      All_Complete_Comparisons <- 
        sapply(Model_List[-length(Model_List)], # don't use the last one - no valid comparisons
               Prepare_domComplt, 
               focalIV=IV_Row_Name, compIV=IV_Col_Name)
      
      Complete_Dominance[IV_Row, IV_Col] <- 
        ifelse(all(All_Complete_Comparisons), TRUE,
               ifelse(all(!All_Complete_Comparisons), FALSE, NA))
      
      Complete_Dominance[IV_Col, IV_Row] <- # ensure symmetry of complete dominance matrix
        !Complete_Dominance[IV_Row, IV_Col]
      
    }
    
  }
  
}

else Complete_Dominance <- NULL

if (reverse == TRUE) Complete_Dominance <- !Complete_Dominance # reverse all designations with `reverse`

# Obtain general dominance statistics ----

General_Dominance <- 
    apply(Conditional_Dominance, 1, mean) # average conditional dominance statistics to produce general dominance

# Obtain overall fit statistic and ranks ----

FitStat <- 
    sum(General_Dominance) + FitStat_Adjustment # adjust overall fit statistic by replacing all subsets component and constant model component

if (reverse == FALSE) General_Dominance_Ranks <- rank(-General_Dominance) # rank general dominance statistic if fitstat value increases (i.e., `reverse` == FALSE)
else General_Dominance_Ranks <- rank(General_Dominance) # rank general dominance statistic if fitstat value decreases (i.e., `reverse` == TRUE)

# Finalize returned values and attributes ----

if (length(sets) == 0 ) IV_Labels <- 
    attr(stats::terms(formula_overall), "term.labels")
else IV_Labels <- 
    c( attr(stats::terms(formula_overall), "term.labels"), 
      paste0("set", 1:length(sets)) ) # names for returned values

names(General_Dominance) <- IV_Labels
names(General_Dominance_Ranks) <- IV_Labels 
dimnames(Conditional_Dominance) <- list(IV_Labels, paste0("IVs_", 1:length(Indep_Vars)))
if (complete) 
  dimnames(Complete_Dominance) <- list(paste0("Dmnates_", IV_Labels),  paste0("Dmnated_", IV_Labels))

if (reverse == FALSE) # Standardized if fitstat increases...
       Standardized <- General_Dominance/(FitStat - ifelse(length(Cons_Result) > 0, Cons_Result[["value"]], 0)) # ...then use normal standardization...
       else Standardized <- -General_Dominance/-(FitStat - ifelse(length(Cons_Result) > 0, Cons_Result[["value"]], 0)) # ...otherwise reverse the general dominance stats to standardize

return_list <- list(
    "General_Dominance" = General_Dominance,
    "Standardized" = Standardized,
    "Ranks" = General_Dominance_Ranks,
    "Conditional_Dominance" = Conditional_Dominance,
    "Complete_Dominance" = Complete_Dominance,
    "Fit_Statistic_Overall" = FitStat,
    "Fit_Statistic_All_Subsets" = 
      All_Result[["value"]] - ifelse(is.null(Cons_Result[["value"]]), 0, Cons_Result[["value"]]),
    "Fit_Statistic_Constant_Model" = Cons_Result[["value"]],
    "Call" = match.call(),
    "Subset_Details" = list(
        "Full_Model" = stats::reformulate(c(Combination_List[[Total_Indep_Vars]], all, consmodel), response = Dep_Var, intercept = intercept),
        "Formula" = attr(stats::terms(formula_overall), "term.labels"), 
        "All" = all,
        "Sets" = sets,
        "Constant" = consmodel
    )
)

    class(return_list) <- c("domin", "list") # apply 'domin' type for S3 method dispatch - list is alternative
    
    return(return_list)

}

#' Print method for \code{domin}
#'
#' Reports formatted results from \code{domin} class object.
#' @param x an object of class "domin".
#' @param ... further arguments passed to or from other methods.
#' @return None. This method is called only for side-effect of printing to the console.
#' @details The print method for class \code{domin} objects reports out the following results:
#' \itemize{
#'  \item{Fit statistic for the full model as well as the fit statistic for the
#'  all subsets model if any entries in \code{all} as well as \code{consmodel}}
#'  \item{Matrix describing general dominance statistics, standardized 
#'  general dominance statistics, and the ranking of the general dominance 
#'  statistics}
#'  \item{Matrix describing the conditional dominance statistics.}
#'  \item{If \code{conditional} is \code{TRUE}, matrix describing the complete 
#'  dominance designations}
#'  \item{If there are entries in \code{sets} and/or \code{all} the terms included in each set as well as the terms in all subsets are reported}}
#'  The \code{domin} print method alters dimension names for readability and they do not display as stored in the \code{domin} object.
#' @export

print.domin <- function(x, ...) {

cat("Overall Fit Statistic:     ", x[["Fit_Statistic_Overall"]], "\n")
if (length(x[["Fit_Statistic_All_Subsets"]]) > 0) cat("All Subsets Fit Statistic: ", x[["Fit_Statistic_All_Subsets"]],"\n")
  if (length(x[["Fit_Statistic_Constant_Model"]]) > 0) cat("Constant Model Fit Statistic: ", x[["Fit_Statistic_Constant_Model"]],"\n")
cat("\n")
cat("General Dominance Statistics:\n")
Display_Std <- 
    t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
dimnames(Display_Std) <- 
    list(names(x[["Ranks"]]), c("General Dominance", "Standardized", "Ranks"))
print(Display_Std)
cat("\n")
cat("Conditional Dominance Statistics:\n")
colnames(x[["Conditional_Dominance"]]) <- 
  paste("IVs:", 1:ncol(x[["Conditional_Dominance"]]))
print(x[["Conditional_Dominance"]])
cat("\n")
if (length(x[["Complete_Dominance"]]>0)) {
    cat("Complete Dominance Designations:\n")
    colnames(x[["Complete_Dominance"]]) <- 
      gsub("^Dmnated_", "Dmnated?", colnames(x[["Complete_Dominance"]]))
    rownames(x[["Complete_Dominance"]]) <- 
      gsub("^Dmnates_", "Dmnates?", rownames(x[["Complete_Dominance"]]))
    print(x[["Complete_Dominance"]])
    cat("\n")
}
if (length(x[["Subset_Details"]][["Sets"]])>0) {
    cat("Components of sets:\n")
    for (set in 1:length(x[["Subset_Details"]][["Sets"]])) {
        cat(paste0("set", set),":", x[["Subset_Details"]][["Sets"]][[set]], "\n")
    }
    cat("\n")
}
if (length(x[["Subset_Details"]][["All"]])>0) {
    cat("All subsets variables:", x[["Subset_Details"]][["All"]])
}
}

