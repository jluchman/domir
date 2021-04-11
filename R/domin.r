#' Dominance analysis supporting \code{formula}-based functions
#'
#' Computes dominance statistics for predictive modeling functions that accept a "standard" \code{formula}.
#' @param formula_overall An object of class \code{\link{formula}} or that can be coerced to class \code{formula} for use in the function in \code{reg}. 
#' The \code{formula} object must have the form \code{response ~ terms} with the \code{terms} separated by \code{+}.
#' @param reg The function implementing the predictive (or "reg"ression) model called. Uses \code{\link{do.call}} and accepts any function call 
#' \code{do.call} would accept.
#' @param fitstat List of specifications to call a fit statistic extracting function (see details). Like \code{reg}, uses \code{do.call} and 
#' accepts any function call \code{do.call} would accept.
#' @param sets An optional list containing vectors of variable or factor names.  Each \link{vector} in the list is used as a set and always 
#' included together in the \code{formula}.
#' @param all An optional vector of variable or factor names to be built into the \code{formula} and included in all model subsets.
#' @param complete Logical.  If \code{FALSE} then complete dominance matrix is not computed.
#' @param ... Additional arguments passed to the function in \code{reg}.
#'
#' @return Returns an object of \code{\link{class}} "domin".
#' An object of class "domin" is a list composed of the following elements:
#' \describe{
#'  \item{\code{General_Dominance}}{Vector of general dominance statistics.}
#'  \item{\code{Standardized}}{Vector of general dominance statistics normalized to be out of 100.}
#'  \item{\code{Ranks}}{Vector of ranks applied to the general dominance statistics.}
#'  \item{\code{Conditional_Dominance}}{Matrix of conditional dominance statistics.}
#'  \item{\code{Complete_Dominance}}{Matrix of complete dominance designations.}
#'  \item{\code{Fit_Statistic_Overall}}{Value of fit statistic across all IVs.}
#'  \item{\code{Fit_Statistic_All_Subsets}}{Value of fit statistic associated with IVs in \code{all}.}
#'  \item{\code{Call}}{The matched call.}
#'  \item{\code{Subset_Details}}{List containing full model and descriptions of IVs in model by source.}
#' }
#'
#' @details Dominance analysis focuses on computing the contribution of independent variables/IVs or \code{terms} to a predictive model's fit to the data.
#' \code{domin} automates the process whereby combinations of IVs are created and concatenated in a formula to be submitted to the model in \code{reg}.
#' \code{domin} creates all the concatenated IVs from the entries on the right hand side of \code{formula_overall} and the entries in \code{sets}.  
#'
#' Each entry in the right of \code{formula_overall} is processed and each individual entry is included as a separate IV.  
#' \code{formula_overall} must contain the dependent variable/DV or \code{response} on the left hand side and any individual IVs separated by \code{+}.  
#' \code{domin} applies only the formula processing that is available in the \code{stats} package.
#'
#' The elements of the list entries in \code{sets} are each considered a separate IV and must be submitted as a list.  
#' Each entry in \code{sets} must be a vector of IVs.  Individual vector elements within a single set are concatenated using \code{+} automatically.
#' It is possible to use a \code{domin} with only sets (i.e., no IVs in \code{formula_overall}; see examples below). 
#'
#' The IV's in \code{all} must also be submitted as a vector, are concatenated with \code{+} automaically, and are also included in the model.
#' These "all subsets" IVs are removed from the fit statistic and all subsequent dominance statistics.
#' 
#' The entry to \code{fitstat} must be list and follow a specific structure: (\code{fit_function}, \code{element_name}, \code{...})
#' \describe{
#'  \item{\code{fit_function}}{First element and function to be applied to \code{reg}}
#'  \item{\code{element_name}}{Second element and name of the element from the object returned by \code{fit_function}}
#'  \item{\code{...}}{Subsequnt elements and are additional arguments passed to \code{fit_function}}
#' }
#' 
#' @keywords multivariate utilities
#' @export
#' @examples
#' ## Basic linear model with r-square
#' domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), data=mtcars)
#' 
#' ## Including sets
#' domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), 
#'  data=mtcars, sets=list(c("carb", "gear"), c("disp", "wt")))
#'
#' ## Multivariate linear model with custom multivariate r-square function and all subsets variable
#' Rxy <- function(obj, names, data) {
#'    return(list("r2" = cancor(predict(obj), 
#'        as.data.frame(mget(names,as.environment(data))))[["cor"]][1]^2)) }
#' domin(cbind(wt, mpg) ~ vs + cyl + am, "lm", list("Rxy", "r2", c("mpg", "wt"), mtcars), 
#'  data = mtcars, all=c("carb"))
#'
#' ## Sets only
#' domin(mpg ~ 1, "lm", list("summary", "r.squared"), 
#'  data=mtcars, sets=list(c("am", "vs"), c("cyl", "disp"), c("qsec", "carb")))

domin <- function(formula_overall, reg, fitstat, sets=NULL, 
    all=NULL, complete=TRUE, ...) {
    
# Initial exit conditions ---- 
    
if (!methods::is(formula_overall, "formula")) 
    stop(paste(formula_overall, "is not a formula object.  Coerce it to formula before use in domin."))
    
if (!is.list(fitstat)) 
    stop("fitstat is not a list.  Please submit it as a list object.")
    
if (length(sets)>0 & !is.list(sets)) 
    stop("sets is not a list.  Please submit it as a list object.")

# Process variable lists ----
    
Indep_Vars <- 
    attr(stats::terms(formula_overall), "term.labels") # obtain IV name vector from `formula_overall`

if (length(sets) > 0) { # if there are sets...
    
    set_aggregated <- 
        sapply(sets, paste0, collapse=" + ") # ...paste together IV names from each set in `formula` format as a vector...
    
    Indep_Vars <- 
        append(Indep_Vars, set_aggregated) # ...append sets vector to end of IV name vector
    
}

Dep_Var <- 
    attr(stats::terms(formula_overall),"variables")[[2]] # pull out DV name from `formula_overall`

Total_Indep_Vars <- length(Indep_Vars) # count number of IVs and sets in model

    ## IV-based exit conditions ----
if (Total_Indep_Vars < 3) 
    stop(paste("Total of", Total_Indep_Vars, "independent variables or sets. At least 3 needed for useful dominance analysis."))

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
doModel_Fit <- function(Indep_Var_Combination, Dep_Var, reg, fitstat, all=NULL, ...) {

    formula_to_use <- 
        stats::formula( # build formula to submit to modeling function by...
            paste0(deparse(Dep_Var), " ~ ", # ...combining the DV with...
                   paste0(c(Indep_Var_Combination, all), collapse = " + " )) #...the set of IVs submitted 
        )

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

# All subsets adjustment ----

if (length(all) > 0) { # if there are entries in all...
    All_Result <- 
        doModel_Fit(all, Dep_Var, reg, fitstat, ...) # ...obtain their `fitstat` value as well...
    FitStat_Adjustment <- 
        All_Result[["value"]] # ...and log the value as the adjustment to the fitstat
}

else {
    All_Result <- NULL # ...otherwise return a null
    FitStat_Adjustment <- 0 # ...and a 0 for fitstat adjustment
}

# Obtain all subsets regression results ----

# 'Ensemble_of_Models' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific result from `do.call`

# ensemble_begin = 0 # note where the ensemble of models has begun for this set of IVs (relevant for tracking progress only)
# ensemble_end = 0 # note where the ensemble of models has ended for this set of IVs (relevant for tracking progress only)
# 
# if Total_Indep_Vars > 4: # if at least 20 models, note which models will report a '.' when estimating
#     flag_list = [int(twentieth/20*Total_Models_to_Estimate) for twentieth in range(1,21)]
# else: flag_list = [] # if not at least 20 models, do not track  progress

# low-level function to identify the specific set of IVs to submit to `doModel_Fit` - called by `doModel_Coordinator`
doModel_ListSelector <- function(Indep_Vars_Chosen, Number_of_Indep_Vars) { 
    
    doModel_Fit(
        Combination_List[[Number_of_Indep_Vars]][, Indep_Vars_Chosen], # From the list at a specific number of IVs in the model, choose one unique combination (which is associated with the columns of the matrices returned by `combn`)...
        Dep_Var, reg, fitstat, all=all, ...) # ...and submit all other pertinent information for model fitting - other names assumed pulled from parent env scope
    # ensemble_begin = ensemble_end # update where the ensemble tracker will begin for next round
    
}

# high-level function to coordinate listing of models within a specific number of IVs
doModel_Coordinator <- function(Number_of_Indep_Vars) { 
    
    lapply(1:ncol(Combination_List[[Number_of_Indep_Vars]]), # list apply `doModel_ListSelector` over all combinations of IVs at a single number of IVs in the model
           doModel_ListSelector, Number_of_Indep_Vars) 
    
}

# list apply `doModel_Coordinator` across all numbers of IVs in the model
utils::capture.output( suppressWarnings( # ensure that "verbose" models are quieted - no fitting information or warnings
    
    Ensemble_of_Models <- # as a list ...
        lapply(1:Total_Indep_Vars, doModel_Coordinator) #... call `doModel_Coordinator` over all numbers of IVs in the model
    
) )

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
    
    Null_Elements <- which(sapply(domIncrement_Lists, is.null)) # identfy NULL list elements
    
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

str(Model_List)

# 'Model_List' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific increment's information (full_model, reduced_model, fit metric difference)


# Obtain complete and conditional dominance statistics ----

Conditional_Dominance <- matrix(nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # conditional dominance container

if (complete) Complete_Dominance <- 
    matrix(data=0, nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # complete dominance container

else Complete_Dominance <- NULL

  ## Single IV model results ----

for (Indep_Var in 1:Total_Indep_Vars) { # for each IV in the model...

    Conditional_Dominance[Indep_Var, 1] <- 
        Model_List[[1]][[Indep_Var]][["increment"]] # for single IV models, copy fit statistic as conditional dominance entry in first column...

    Indep_Varname <- 
        Model_List[[1]][[Indep_Var]][["names_curr"]] #... and record name of focal IV for use below

    if (complete) 
        Complete_atIndep_Var <- # produces logical vector indicating a comparison that represents ...
        ( Model_List[[1]][[Indep_Var]][["increment"]] > #... is focal IV's increment bigger than ...
             sapply(Model_List[[1]], # ... other models at one IV ... 
                    function(specific_fit_stat) specific_fit_stat[["increment"]] ) ) # ... when compared to their increments (note)
    
    # Multiple IV model results ----
    for (number_of_Indep_Vars in 2:Total_Indep_Vars) { # for all numbers of IVs greater than 1...

        Relevant_Increments <- 
            vector(mode="numeric", length=choose(Total_Indep_Vars-1, number_of_Indep_Vars-1)) # initialize/reset container for collecting specific/relevant conditional dominance increments
        
        place = 1

        for (model in 1:length(Model_List[[number_of_Indep_Vars]])) { # for each individual model within a specific number of IVs...

            proceed_to_record <- any(intersect(Indep_Varname, Model_List[[number_of_Indep_Vars]][[model]][[1]])==Indep_Varname) &  # flag this entry for recording if the focal IV name is in the IV set...
               !any(intersect(Indep_Varname, Model_List[[number_of_Indep_Vars]][[model]][[2]])==Indep_Varname) # ...but is _not_ in the IV set less one - thus, the fit statistic here is a valid "increment" for the focal IV

            if (proceed_to_record) {
                Relevant_Increments[[place]] <- Model_List[[number_of_Indep_Vars]][[model]][[3]] # always collect the fit statistic for conditional dominance computations
                place <- place +1 
            }

            if (complete) {
                for (other_model in 1:length(Model_List[number_of_Indep_Vars])) { # also proceed to collect complete dominance data using this loop comparing to all other models within this number of IVs to find relevant comparisons

                       relevant_complete <- ( # a relevant complete dominance comparision is found when ...
                            setequal(Model_List[[number_of_Indep_Vars]][[model]][[2]], Model_List[[number_of_Indep_Vars]][[other_model]][[2]]) & # ...the focal full model and the full other model have the same IV set (the only way they can be a 'subset' here) ...
                                (length(setdiff(Model_List[[number_of_Indep_Vars]][[model]][[1]], Model_List[[number_of_Indep_Vars]][[other_model]][[1]])) == 1) ) #... but their reduced IV set differs by one IV (this ensures it is not trying to compare the subset to itself)


                    if (relevant_complete) {
                        MatrixLocation_Complete <- (1:Total_Indep_Vars)[ Indep_Vars %in% 
                            setdiff(Model_List[[number_of_Indep_Vars]][[other_model]][[1]], Model_List[[number_of_Indep_Vars]][[model]][[1]]) ] #... the different element in the reduced model (to place it in the correct "row" for the dominance matrix/list)
                        
                        Complete_atIndep_Var[MatrixLocation_Complete] <- as.integer( #at the correct location in the complete dominance matrix, append...
                            all(Model_List[[number_of_Indep_Vars]][[model]][["increment"]] > 
                                  Model_List[[number_of_Indep_Vars]][[other_model]][["increment"]],  
                                as.logical(Complete_atIndep_Var[MatrixLocation_Complete]))) # ...whether the other model's increment is bigger than the focal
                    }
                
                }

            }

        }
        
        Conditional_Dominance[Indep_Var, number_of_Indep_Vars] <- 
            mean(Relevant_Increments) # compute conditional dominance at number of IVs for specific IV and append
    
    }
    
    if (complete) Complete_Dominance[Indep_Var,] <- 
        as.integer(Complete_atIndep_Var) # append full row of IV's complete dominance logicals/designations

}

if (complete) Complete_Dominance <- Complete_Dominance + t(-Complete_Dominance) # ensure symmetry of complete dominance matrix

#     ~~ Compute general dominance and fit statistic  ~~ ##

General_Dominance <- 
    apply(Conditional_Dominance, 1, mean) # average conditional dominance statistics to produce general dominance

FitStat <- 
    sum(General_Dominance) + FitStat_Adjustment # adjust overall fit statistic by replacing all subsets component and constant model component

General_Dominance_Ranks <- rank(-General_Dominance) # rank general dominance statistic

if (length(sets) == 0 ) IV_Labels <- 
    attr(stats::terms(formula_overall), "term.labels")
else IV_Labels <- 
    c( attr(stats::terms(formula_overall), "term.labels"), 
      paste0("set", 1:length(sets)) ) # names for returned values

names(General_Dominance) <- IV_Labels
names(General_Dominance_Ranks) <- IV_Labels 
dimnames(Conditional_Dominance) <- list(IV_Labels, paste("IVs:", 1:length(Indep_Vars)))
dimnames(Complete_Dominance) <- list(paste0("Dmate?", IV_Labels),  paste0("Dmned?", IV_Labels))

return_list <- list(
    "General_Dominance" = General_Dominance,
    "Standardized" = General_Dominance/FitStat,
    "Ranks" = General_Dominance_Ranks,
    "Conditional_Dominance" = Conditional_Dominance,
    "Complete_Dominance" = Complete_Dominance,
    "Fit_Statistic_Overall" = FitStat,
    "Fit_Statistic_All_Subsets" = All_Result[["value"]],
    "Call" = match.call(),
    "Subset_Details" = list(
        "Full_Model" = paste0(deparse(Dep_Var), " ~ ", (paste0(Combination_List[[Total_Indep_Vars]], collapse=" + "))),
        "Formula" = attr(stats::terms(formula_overall), "term.labels"), 
        "All" = all,
        "Sets" = sets
    )
)
    
    class(return_list) <- c("domin", "list") # apply 'domin' type for S3 method dispatch - list is alternative
    
    return(return_list)

}

#' Print method for \code{domin}
#'
#' Reports basic results from \code{domin} class object.
#' @param x an object of class "domin".
#' @param ... further arguments passed to or from other methods.
#' @return No returned value.  This method is called for compact display of 
#' results in the console.
#' @details The print method for class \code{domin} objects reports out the 
#' following results:
#' \itemize{
#'  \item{Fit statistic for the full model as well as the fit statistic for the
#'  all subsets model if any entries in \code{all}.}
#'  \item{Matrix describing general dominance statistics, standardized 
#'  general dominance statisics, and the ranking of the general dominance 
#'  statistics.}
#'  \item{Matrix describing the conditional dominance statistics.}
#'  \item{If \code{conditional} is \code{TRUE}, matrix describing the complete 
#'  dominance statistics.}
#' }
#' @keywords print
#' @export

print.domin <- function(x, ...) {

cat("Overall Fit Statistic:     ", x[["Fit_Statistic_Overall"]], "\n")
if (length(x[["Fit_Statistic_All_Subsets"]]) > 0) cat("All Subsets Fit Statistic: ", x[["Fit_Statistic_All_Subsets"]],"\n")
cat("\n")
cat("General Dominance Statistics:\n")
Display_Std <- 
    t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
dimnames(Display_Std) <- 
    list(names(x[["Ranks"]]), c("General_Dominance", "Standardized", "Ranks"))
print(Display_Std)
cat("\n")
cat("Conditional Dominance Statistics:\n")
print(x[["Conditional_Dominance"]])
cat("\n")
if (length(x[["Complete_Dominance"]]>0)) {
    cat("Complete Dominance Statistics:\n")
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

