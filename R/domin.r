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
#' @param conditional Logical.  If \code{FALSE} then conditional dominance matrix is not computed.
#' 
#' If conditional dominance is not desired as an importance criterion, avoiding computing the conditional dominance matrix can save computation time.
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
           conditional = TRUE, complete = TRUE, consmodel = NULL, reverse = FALSE, ...) {
    
# Initial exit/warning conditions ---- 
    
if (!inherits(formula_overall, "formula")) 
  stop(paste(formula_overall, "is not a 'formula' class object."))
    
if (!is.list(fitstat)) 
  stop("fitstat is not a list.")
    
if (length(sets) > 0 & !is.list(sets)) 
  stop("sets is not a list.")
    
if (is.list(all)) 
  stop("all is a list.  Please submit it as a vector.")
  
if (!attr(stats::terms(formula_overall), "response")) 
    stop(paste(deparse(formula_overall), "missing a response."))
  
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

Combination_Matrix <- 
  expand.grid(
    lapply(1:Total_Indep_Vars, 
           function(x) c(FALSE, TRUE)), 
    KEEP.OUT.ATTRS = FALSE)[-1,] # Logical matrix of inclusion/exclusion for IVs; omit the first, empty row

Total_Models_to_Estimate <- 2**Total_Indep_Vars - 1 # total number of models to estimate

# Define function to call regression models ----

doModel_Fit <- function(Indep_Var_Combin_lgl, Indep_Vars, Dep_Var, # assumes receipt of a logical matrix indicating which IVs to use as well as IV list and DV
                        reg, fitstat, all = NULL, consmodel = NULL, intercept, ...) { # also regression function, fitstat function, with all and consmodel arguments along with an intercept argument for reformulate
  
  Indep_Var_Combination <- Indep_Vars[Indep_Var_Combin_lgl] # select vector of IVs
  
  formula_to_use <- 
    stats::reformulate(c(Indep_Var_Combination, all, consmodel), # 'reformulate' formula to submit to model
                       response = Dep_Var, intercept = intercept)
  
  Model_Result <- # capture the model object ...
    list( # ... as an element of a list, needed for next step calling fit statistic function ...
      do.call(reg, list(formula_to_use, ...) ) # ... from `do.call` invoking the modeling function with formula and all other arguments as the ellipsis
    ) 
  
  if (length(fitstat) > 2) # if there are additional arguments to pass to the fitstat function, indicated by having length of > 2 for this list...
    Model_Result <- 
    append(Model_Result, fitstat[3:length(fitstat)]) # ...append these additional arguments to 'Model_Result' for coming `do.call`
  
  Fit_Value <- do.call(fitstat[[1]], Model_Result) # use first entry of `fitstat` as fitstat function name, use `Model_Result` as results to submit to it
  
  return( Fit_Value[[ fitstat[[2]] ]] ) # ... use second, necessarily named, argument of `fitstat` to select the result of `Fit_Value` to return
  
}

# Constant model adjustments ----

if (length(consmodel) > 0) { # if there are entries in consmodel...

  FitStat_Adjustment <- Cons_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, consmodel = consmodel, intercept = intercept, ...) # ...obtain their fitstat' value using `doModel_Fit()`

}

else {
  Cons_Result <- NULL # ...otherwise return a null
  FitStat_Adjustment <- 0
}

# All subsets adjustment ----

if (length(all) > 0) { # if there are entries in all...
  
  FitStat_Adjustment <- All_Result <- doModel_Fit(NULL, Indep_Vars, Dep_Var, reg, fitstat, all = all, consmodel = consmodel, intercept = intercept, ...) # ...obtain their 'fitstat' value as well using `doModel_fit()`...
  
}

else All_Result <- NULL # ...otherwise return a null

# Obtain all subsets regression results ----

Ensemble_of_Models <- 
  sapply(1:nrow(Combination_Matrix), # for all individual models/rows in the logical combination matrix...
         function(x) { # ... submit the row number ...
           doModel_Fit(unlist(Combination_Matrix[x,]), # ... select a unique pattern of IVs as a logical vector from the combination matrix ...
                        Indep_Vars, Dep_Var, reg, fitstat, # ... and submit all model information
                        all = all, consmodel = consmodel, intercept = intercept, ...)
         },
         simplify = TRUE, USE.NAMES = FALSE)

# Obtain conditional dominance statistics ----

if (conditional) {

  Conditional_Dominance <- 
    matrix(nrow = Total_Indep_Vars, ncol = Total_Indep_Vars) # conditional dominance container
  
  Combination_Matrix_Anti <-!Combination_Matrix # complement of logical IV inclusion matrix
  
  IVs_per_Model <- rowSums(Combination_Matrix) # count IVs in each model using logical IV inclusion matrix
  
  Combins_at_Order <- 
    sapply(IVs_per_Model, # for each IV/column, ... 
           function(x) choose(Total_Indep_Vars, x), # ... compute # of combinations associated with each row given the total number of IVs
           simplify = TRUE, USE.NAMES = FALSE)
  
  Combins_at_Order_Prev <- 
    sapply(IVs_per_Model, # for each IV/column, ... 
           function(x) choose(Total_Indep_Vars - 1, x),# ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
           simplify = TRUE, USE.NAMES = FALSE)
  
  Weighted_Order_Ensemble <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model; these values are associated with models that have the focal IV (hence multiplied by logical matrix).  This matrix is inverted to make weights that can be used for weighted averaging.  Finally, these weights are multiplied by all the fit statistics
    ((Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev))**-1)*Ensemble_of_Models
  
  Weighted_Order_Ensemble <- # effectively missing values (Inf) are changed to 0 to remove from summing
    replace(Weighted_Order_Ensemble, 
            Weighted_Order_Ensemble==Inf, 0)
  
  Weighted_Order_Ensemble_Anti <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model; these values are associated with models that have do not have focal IV and would thus be used to subtract out of models that do (hence the reflection across 0). This matrix is inverted to make weights that can be used for weighted averaging.  Finally, these weights are multiplied by all the fit statistics
    ((Combination_Matrix_Anti*Combins_at_Order_Prev)**-1)*Ensemble_of_Models
  
  Weighted_Order_Ensemble_Anti <-# effectively missing values (Inf) are changed to 0 to remove from summing
    replace(Weighted_Order_Ensemble_Anti, 
            Weighted_Order_Ensemble_Anti==Inf, 0)
  
  for (order in 1:Total_Indep_Vars) {
    
    Conditional_Dominance[, order] <- # the column associated with an order (# of IVs) in the model is replaced by ...
      t( # ... transpose of ...
        colSums(Weighted_Order_Ensemble[IVs_per_Model==order,]) - # ... the sum of the weighted fit stats at a with a specified number of IVs in the model for all models including the focal IV (across all rows) ...
          colSums(Weighted_Order_Ensemble_Anti[IVs_per_Model==(order-1),]) # ... subtracting out the sum of the weighted fit stats with one less than a specified number of IVs in the model for all models that do not contain the focal IV (across all rows)
        )
    
  }
  
  Conditional_Dominance[,1] <- Conditional_Dominance[,1] - FitStat_Adjustment # adjust the first set of conditional dominance statistics for the constant and/or all fitstat adjustments
  
}

else Conditional_Dominance <- NULL

# Obtain complete dominance statistics ----

if (complete) {
  
  Complete_Dominance <- 
    matrix(data = NA, nrow = Total_Indep_Vars, ncol = Total_Indep_Vars) # complete dominance container; initialize with missing values
  
  Complete_Combinations <- 
    utils::combn(1:Total_Indep_Vars, 2) # produce all pairs of IVs (indicated by column numbers in logical matrix)
  
  for (pair in 1:ncol(Complete_Combinations)) { # looping across all pairs of IVs...
    
    Focal_Cols <- Complete_Combinations[, pair] # identify which columns are associated with the focal unique pair of IVs
    
    NonFocal_Cols <- setdiff(1:Total_Indep_Vars, Focal_Cols) # find the columns associated with the complement of IVs not selected in the unique pair
    
    Select_2IVs <- 
      cbind(Combination_Matrix, 1:nrow(Combination_Matrix))[ # combine logical IV inclusion matrix with vector of row indicators (used to select specific rows in the fitstat vector) ...
        rowSums( Combination_Matrix[,Focal_Cols] )==1, ] #... then take only the rows/models associated with having only one of either of the focal IVs (never both or neither IVs)
    
    Sorted_2IVs <- 
      Select_2IVs[
        do.call("order", # sort/order the logical and row indicator matrix...
                as.data.frame(Select_2IVs[,c(NonFocal_Cols, Focal_Cols)])), ] #... based on the non included IVs - this forces non-included IVs to "pair" sequentially; that is, models with focal IV 1 are at vector location 'n' and models with focal IV 2 are at vector location 'n + 1' and all the non-focal IVs are identical between those two models (which is required for a valid complete dominance comparison)
    
    Compare_2IVs <-
      cbind( # bind in a matrix as different columns...
        Ensemble_of_Models[ # ... fit statistics ...
          Sorted_2IVs[(1:nrow(Sorted_2IVs) %% 2)==0, ncol(Sorted_2IVs)]], # ... selected based on the row indicators (i.e., last row of the sorted matrix produced above), in all 'n' row positions (i.e., modulus of 0 with frequency 2)...
        Ensemble_of_Models[ # ... as well as the fit statistics ...
          Sorted_2IVs[(1:nrow(Sorted_2IVs) %% 2)==1, ncol(Sorted_2IVs)]] # ... selected based on the row indicators (again, last row of the sorted matrix produced above), in all 'n + 1' row positions (i.e., modulus of 1 with frequency 2)
        )
    
    Complete_Designation <- # compare the matched matrices across columns - and make complete dominance designation
      ifelse(all(Compare_2IVs[,1] > Compare_2IVs[,2]), FALSE,
                 ifelse(all(Compare_2IVs[,1] < Compare_2IVs[,2]), TRUE, NA))
    
    Complete_Dominance[ Focal_Cols[[2]], Focal_Cols[[1]] ] <- #assign designation to one instance of pair 
      Complete_Designation
    
    Complete_Dominance[ Focal_Cols[[1]], Focal_Cols[[2]] ] <- #assign designation to complementary instance of pair
      !Complete_Designation
    
  }
  
}

else Complete_Dominance <- NULL

if (reverse == TRUE) Complete_Dominance <- !Complete_Dominance # reverse all designations with `reverse`

# Obtain general dominance statistics ----

if (!conditional) { # if there was no conditional dominance computed, generate general dominance as below...
  
  Combination_Matrix_Anti <-!Combination_Matrix # complement of logical IV inclusion matrix
  
  IVs_per_Model <- rowSums(Combination_Matrix) # count IVs in each model using logical IV inclusion matrix
  
  Combins_at_Order <- 
    sapply(IVs_per_Model, # for each IV/column, ... 
           function(x) choose(Total_Indep_Vars, x), # ... compute # of combinations associated with each row given the total number of IVs
           simplify = TRUE, USE.NAMES = FALSE)
  
  Combins_at_Order_Prev <- 
    sapply(IVs_per_Model, # for each IV/column, ... 
           function(x) choose(Total_Indep_Vars - 1, x),# ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
           simplify = TRUE, USE.NAMES = FALSE)
  
  Indicator_Weight <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (surrogate for what conditional dominance statistics do); these values are associated with models that have the focal IV
    Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev)
  
  Indicator_Weight_Anti <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (again, surrogate for what conditional dominance statistics do); these values are associated with models that have do not have focal IV and would thus be used to subtract out of models that do (hence the reflection across 0)
    (Combination_Matrix_Anti*Combins_at_Order_Prev)*-1
  
  Weight_Matrix <- # combine combinations of focal and non-focal counts associated with what the conditional dominance statistics would do with the number of IV average associated with what the general dominance statistics do (arithmetic average of all conditional dominance statistics); these combinations are inverted to make them function as a set of weights that produce averages 
    ((Indicator_Weight + Indicator_Weight_Anti)*Total_Indep_Vars)^-1
  
  General_Dominance <- # general dominance is sum of the product of the weight matrix above and all the fit statistics
    colSums(Ensemble_of_Models*Weight_Matrix)
  
  General_Dominance <- General_Dominance - FitStat_Adjustment/Total_Indep_Vars # adjust the general dominance statistics for the constant and/or all fitstat adjustments in the first/order 1 stats as conditional dominance would do
  
}

else General_Dominance <- rowMeans(Conditional_Dominance) #... otherwise average conditional dominance by IV/row to produce general dominance statistics

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
if (conditional) 
  dimnames(Conditional_Dominance) <- list(IV_Labels, paste0("IVs_", 1:length(Indep_Vars)))
if (complete) 
  dimnames(Complete_Dominance) <- list(paste0("Dmnates_", IV_Labels),  paste0("Dmnated_", IV_Labels))

if (reverse == FALSE) # Standardized if fitstat increases...
       Standardized <- General_Dominance/(FitStat - ifelse(length(Cons_Result) > 0, Cons_Result, 0)) # ...then use normal standardization...
       else Standardized <- -General_Dominance/-(FitStat - ifelse(length(Cons_Result) > 0, Cons_Result, 0)) # ...otherwise reverse the general dominance stats to standardize

return_list <- list(
    "General_Dominance" = General_Dominance,
    "Standardized" = Standardized,
    "Ranks" = General_Dominance_Ranks,
    "Conditional_Dominance" = Conditional_Dominance,
    "Complete_Dominance" = Complete_Dominance,
    "Fit_Statistic_Overall" = FitStat,
    "Fit_Statistic_All_Subsets" = All_Result - ifelse(is.null(Cons_Result), 0, Cons_Result),
    "Fit_Statistic_Constant_Model" = Cons_Result,
    "Call" = match.call(),
    "Subset_Details" = list(
        "Full_Model" = stats::reformulate(c(Indep_Vars, all, consmodel), response = Dep_Var, intercept = intercept),
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
#' @param ... further arguments passed to or from other methods. Not used currently.
#' @return The "domin" object with altered column and row names for conditional and complete dominance results as displayed in the console.
#' @details The print method for class \code{domin} objects reports out the following results:
#' \itemize{
#'  \item{Fit statistic for the full model.  The fit statistic for the all subsets model is reported here if there are any entries in \code{all}.  The fit statistic for the constant model is reported here if there are any entries in \code{consmodel}.}
#'  \item{Matrix describing general dominance statistics, standardized general dominance statistics, and the ranking of the general dominance statistics}
#'  \item{If \code{conditional} is \code{TRUE}, matrix describing the conditional dominance designations}
#'  \item{If \code{complete} is \code{TRUE}, matrix describing the complete dominance designations}
#'  \item{If following \code{summary.domin}, matrix describing the strongest dominance designations between all independent variables}
#'  \item{If there are entries in \code{sets} and/or \code{all} the terms included in each set as well as the terms in all subsets are reported}}
#'  The \code{domin} print method alters dimension names for readability and they do not display as stored in the original \code{domin} object.
#' @export

print.domin <- function(x, ...) {
  
  cat("Overall Fit Statistic:     ", x[["Fit_Statistic_Overall"]], "\n")
  
  if (length(x[["Fit_Statistic_All_Subsets"]]) > 0) 
    cat("All Subsets Fit Statistic: ", x[["Fit_Statistic_All_Subsets"]], "\n")
  
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
  
  if (length(x[["Subset_Details"]][["Sets"]]) > 0) {
    
    cat("Components of sets:\n")
    
    for (set in 1:length(x[["Subset_Details"]][["Sets"]])) {
      
      cat(paste0("set", set),":", x[["Subset_Details"]][["Sets"]][[set]], "\n")
      
    }
    
    cat("\n")
    
  }
  
  if (length(x[["Subset_Details"]][["All"]]) > 0) {
    
    cat("All subsets variables:", x[["Subset_Details"]][["All"]])
    
  }
  
  invisible(x)
  
}


#' Summary method for \code{domin}
#'
#' Reports dominance designation results from the \code{domin} class object.
#' @param object an object of class "domin".
#' @param ... further arguments passed to or from other methods. Not used currently.
#' @return The originally submitted "domin" object with an additional \code{Strongest_Dominance} element added.
#' \describe{
#'  \item{\code{Strongest_Dominance}}{Matrix comparing the independent variable in the first row to the independent variable in the third row.  The second row denotes the strongest designation between the two independent variables.}
#' }
#' @details The summary method for class \code{domin} is used for obtaining the strongest dominance designations (i.e., general, conditional, or complete) among the independent variables.
#' @export

summary.domin <- function(object, ...) {
  
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
                     "completely dominates",
                     "is completely dominated by")
            
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
