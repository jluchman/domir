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
#' The IV's in \code{all} must also be submitted as a vector, are concatenated with \code{+} automatically, and are also included in the model.
#' These "all subsets" IVs are removed from the fit statistic and all subsequent dominance statistics.
#' 
#' The entry to \code{fitstat} must be list and follow a specific structure: (\code{fit_function}, \code{element_name}, \code{...})
#' \describe{
#'  \item{\code{fit_function}}{First element and function to be applied to \code{reg}}
#'  \item{\code{element_name}}{Second element and name of the element from the object returned by \code{fit_function}}
#'  \item{\code{...}}{Subsequent elements and are additional arguments passed to \code{fit_function}}
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
    
    # ~~ Exit conditions ~~ #
    
if (!methods::is(formula_overall, "formula")) stop(paste(formula_overall, "is not a formula object.  Coerce it to formula before use in domin."))
if (!is.list(fitstat)) stop("fitstat is not a list.  Please submit it as a list object.")
if (length(sets)>0 & !is.list(sets)) stop("sets is not a list.  Please submit it as a list object.")

    # ~~ Create independent variable list ~~ #
    
Indep_Var_List <- attr(stats::terms(formula_overall), "term.labels") # obtain IV list

if (length(sets) > 0) { # if there are sets...
    
    set_aggregated <- sapply(sets, paste0, collapse=" + ") # ...paste together elements of set...
    
    Indep_Var_List <- append(Indep_Var_List, set_aggregated) # ... append to IV list
    
}

Dep_Var <- attr(stats::terms(formula_overall),"variables")[[2]] # pull out DV

Total_Indep_Vars <- length(Indep_Var_List) # number of IVs in model

if (Total_Indep_Vars < 3) stop(paste("Total of", Total_Indep_Vars,"independent variables or sets. At least 3 needed for useful dominance analysis."))

    # ~~ Create independent variable combination list ~~ #
    
Combination_List <- lapply( (1:length(Indep_Var_List)), # use lapply() function to apply each distinct number of combination to ...
							function(Comb_Num) {utils::combn(Indep_Var_List, Comb_Num)} ) # ... combn() function using the the IV list to obtain all combinations

Total_Models_to_Estimate <- 2**Total_Indep_Vars - 1 # total number of models to estimate

#     ~~ Define function to call regression models ~~ #

Ensemble_Coordinator <- function(Indep_Var_combination, Dep_Var, reg, fitstat, all=NULL, ...) {

    formula_to_use <- stats::formula(paste0(deparse(Dep_Var), " ~ ", paste0(c(Indep_Var_combination, all), collapse = " + " )))

    temp_result <- list(do.call(reg, list(formula_to_use, ...)) )  # build function that processes list then calls regression
    
    if (length(fitstat) > 2) temp_result <- append(temp_result, fitstat[3:length(fitstat)]) # include additional arguments to fitstat
    
    fit_value <- do.call(fitstat[[1]], temp_result) # apply fitstat function
    
    return( list( # return fitstat value as associated with IV combination
        Indep_Var_combination,
        fit_value[[ fitstat[[2]] ]]
    ))

}

if (length(all) > 0) All_Result <- Ensemble_Coordinator(all, Dep_Var, reg, fitstat, ...)
else All_Result <- NULL

#     ~~ Obtain all subsets regression results ~~ #

Ensemble_of_Models <- list() # initialize ensemble list container

# 'Ensemble_of_Models' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific result from 'st_model_call'

# ensemble_begin = 0 # note where the ensemble of models has begun for this set of IVs (relevant for tracking progress only)
# 
# ensemble_end = 0 # note where the ensemble of models has ended for this set of IVs (relevant for tracking progress only)
# 
# if Total_Indep_Vars > 4: # if at least 20 models, note which models will report a '.' when estimating
#     flag_list = [int(twentieth/20*Total_Models_to_Estimate) for twentieth in range(1,21)]
#     
# else: flag_list = [] # if not at least 20 models, do not track  progress

for (number_of_Indep_Vars in 1:Total_Indep_Vars) { # applying the modeling function across all IV combinations at a distinct number of IVs

    utils::capture.output(
        Models_at_Indep_Var_number <- 
            lapply((1:ncol(Combination_List[[number_of_Indep_Vars]])), # for all columns of `Combination_List`...
                function (indep_vars) { 
                    Ensemble_Coordinator(Combination_List[[number_of_Indep_Vars]][, indep_vars], # ... submit column as independent variables to `Ensemble_Coordinator`
                                         Dep_Var, reg, fitstat, all=all, ...) 
                }
            ) 
    )

    Ensemble_of_Models <- append(Ensemble_of_Models, list(Models_at_Indep_Var_number) )
 
#     ensemble_begin = ensemble_end # update where the ensemble tracker will begin for next round

}

    # ~~ Process all subsets - find the increments  ~~ #

Model_List <- list(Ensemble_of_Models[[1]])  # evaluate the lapply-ed models and record them - start with the single IV models...

if (length(all) > 0) FitStat_Adjustment <- All_Result[[2]]
else FitStat_Adjustment <- 0

for (model in 1:length(Model_List[[1]])) { # ...for the single IV models...
     
     Model_List[[1]][[model]][[2]] <- Model_List[[1]][[model]][[2]] - FitStat_Adjustment #... have to remove constant model results as well as all subets results
     
}

for (number_of_Indep_Vars in 2:length(Ensemble_of_Models)) { # when >1 IV in the model, processing needed...

    Model_Incremented <- list()  # initialize/reset container for finding subset
    Location_in_Model_Incremented <- 1 # ... useful for R...

    Indep_Var_Set_at_1lessIndep_Var <- 
        lapply(Ensemble_of_Models[[number_of_Indep_Vars-1]], 
            function(Candidate_Indep_Var_Set) { Candidate_Indep_Var_Set[[1]] }) # collect all sets IVs (coerced to be a set object), specifically all sets at one less IV in the model than the current number of IVs
    
    for (model in 1:length(Ensemble_of_Models[[number_of_Indep_Vars]])) { # loop through all models at a specific number of IVs in the model...
#
        Indep_Var_Set <- Ensemble_of_Models[[number_of_Indep_Vars]][[model]][[1]] # IV set for a focal model; coerced to be set object

        for (at1less_model in 1:length(Indep_Var_Set_at_1lessIndep_Var)) { # loop through all models at one less than the specific number of IVs in the model...
# 
            if (length(intersect(Indep_Var_Set_at_1lessIndep_Var[[at1less_model]], Indep_Var_Set)) == length(Indep_Var_Set_at_1lessIndep_Var[[at1less_model]])) { # if IV set at one less is a subset of the predictors in the focal model...

                Model_Incremented[[Location_in_Model_Incremented]] <- 
                    list(Ensemble_of_Models[[number_of_Indep_Vars]][[model]][[1]], # append IV names at focal ...
                        Ensemble_of_Models[[number_of_Indep_Vars-1]][[at1less_model]][[1]], # ...IV names at one less...
                        Ensemble_of_Models[[number_of_Indep_Vars]][[model]][[2]] - Ensemble_of_Models[[number_of_Indep_Vars-1]][[at1less_model]][[2]] ) # ...and the increment to the fit metric
                 
                Location_in_Model_Incremented <- Location_in_Model_Incremented + 1
                 
            }
            
        }
                
    }
    
    Model_List <- append(Model_List, list(Model_Incremented))
    
}


# 'Model_List' is structured such that:
# 1. Top level is results by number of IVs in the model
# 2. Middle level is model within a number of IVs
# 3. Bottom level is a specific increment's information (full_model, reduced_model, fit metric difference)


#     ~~ Obtain complete and conditional dominance statistics  ~~ #

Conditional_Dominance <- matrix(nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # conditional dominance container

if (complete) Complete_Dominance <- matrix(data=0, nrow=Total_Indep_Vars, ncol=Total_Indep_Vars) # complete dominance container
else Complete_Dominance <- NULL

for (Indep_Var in 1:Total_Indep_Vars) { # for each IV in the model...

    Conditional_atIndep_Var <- list() # initialize/reset container for conditional dominance

    Conditional_Dominance[Indep_Var, 1] <- Model_List[[1]][[Indep_Var]][[2]] # for IV alone - copy fit statistic

    Indep_Varname <- Model_List[[1]][[Indep_Var]][[1]] # record name of focal IV

    if (complete) 
        Complete_atIndep_Var <- (Model_List[[1]][[Indep_Var]][[2]] > sapply(Model_List[[1]], function(specific_fit_stat) {specific_fit_stat[[2]]} ))   # ~ ... redo documentation ... ~ # the idea is to compare all vars at 1 IV

    for (number_of_Indep_Vars in 2:Total_Indep_Vars) { # for all numbers of IVs greater than 1...

        Relevant_Increments <- c() # initialize/reset container for collecting specific/relevant conditional dominance increments

        for (model in 1:length(Model_List[[number_of_Indep_Vars]])) { # for each individual model within a specific number of IVs...

            proceed_to_record <- any(intersect(Indep_Varname, Model_List[[number_of_Indep_Vars]][[model]][[1]])==Indep_Varname) &  # flag this entry for recording if the focal IV name is in the IV set...
               !any(intersect(Indep_Varname, Model_List[[number_of_Indep_Vars]][[model]][[2]])==Indep_Varname) # ...but is _not_ in the IV set less one - thus, the fit statistic here is a valid "increment" for the focal IV

            if (proceed_to_record)
                Relevant_Increments <- append(Relevant_Increments, Model_List[[number_of_Indep_Vars]][[model]][[3]]) # always collect the fit statistic for conditional dominance computations

            if (complete) {
                for (other_model in 1:length(Model_List[number_of_Indep_Vars])) { # also proceed to collect complete dominance data using this loop comparing to all other models within this number of IVs to find relevant comparisons

                       relevant_complete <- ( # a relevant complete dominance comparsion is found when ...
                            setequal(Model_List[[number_of_Indep_Vars]][[model]][[2]], Model_List[[number_of_Indep_Vars]][[other_model]][[2]]) & # ...the focal full model and the full other model have the same IV set (the only way they can be a 'subset' here) ...
                                (length(setdiff(Model_List[[number_of_Indep_Vars]][[model]][[1]], Model_List[[number_of_Indep_Vars]][[other_model]][[1]])) == 1) ) #... but their reduced IV set differs by one IV (this ensures it is not trying to compare the subset to itself)


                    if (relevant_complete) {
                        MatrixLocation_Complete <- (1:Total_Indep_Vars)[ Indep_Var_List %in% 
                            setdiff(Model_List[[number_of_Indep_Vars]][[other_model]][[1]], Model_List[[number_of_Indep_Vars]][[model]][[1]]) ] #... the different element in the reduced model (to place it in the correct "row" for the dominance matrix/list)
                        
                        Complete_atIndep_Var[MatrixLocation_Complete] <- as.integer( #at the correct location in the complete dominance matrix, append...
                            all(Model_List[[number_of_Indep_Vars]][[model]][[3]] > Model_List[[number_of_Indep_Vars]][[other_model]][[3]],  
                                as.logical(Complete_atIndep_Var[MatrixLocation_Complete]))) # ...whether the other model's increment is bigger than the focal
                    }
                
                }

            }

        }
        
        Conditional_Dominance[Indep_Var, number_of_Indep_Vars] <- mean(Relevant_Increments) # compute conditional dominance at number of IVs for specific IV and append
    
    }
    
    if (complete) Complete_Dominance[Indep_Var,] <- as.integer(Complete_atIndep_Var) # append full row of IV's complete dominance logicals/designations

}

if (complete) Complete_Dominance <- Complete_Dominance + t(-Complete_Dominance) # ensure symmetry of complete dominance matrix

#     ~~ Compute general dominance and fit statistic  ~~ ##

General_Dominance <- apply(Conditional_Dominance, 1, mean) # average conditional dominance statistics to produce general dominance

FitStat <- sum(General_Dominance) + FitStat_Adjustment # adjust overall fit statistic by replacing all subsets component and constant model component

General_Dominance_Ranks <- rank(-General_Dominance) # rank general dominance statistic

if (length(sets) == 0 ) IV_Labels <- attr(stats::terms(formula_overall), "term.labels")
else IV_Labels <- c(attr(stats::terms(formula_overall), "term.labels"), paste0("set", 1:length(sets))) # names for returned values

names(General_Dominance) <- IV_Labels
names(General_Dominance_Ranks) <- IV_Labels 
dimnames(Conditional_Dominance) <- list(IV_Labels, paste("IVs:", 1:length(Indep_Var_List)))
dimnames(Complete_Dominance) <- list(paste0("Dmate?", IV_Labels),  paste0("Dmned?", IV_Labels))

return_list <- list(
    "General_Dominance" = General_Dominance,
    "Standardized" = General_Dominance/FitStat,
    "Ranks" = General_Dominance_Ranks,
    "Conditional_Dominance" = Conditional_Dominance,
    "Complete_Dominance" = Complete_Dominance,
    "Fit_Statistic_Overall" = FitStat,
    "Fit_Statistic_All_Subsets" = All_Result[[2]],
    "Call" = match.call(),
    "Subset_Details" = list(
        "Full_Model" = paste0(deparse(Dep_Var), " ~ ", (paste0(Combination_List[[Total_Indep_Vars]], collapse=" + "))),
        "Formula" = attr(stats::terms(formula_overall), "term.labels"), 
        "All" = all,
        "Sets" = sets
    )
)
    
    class(return_list) <- c("domin","list")
    
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
#'  general dominance statistics, and the ranking of the general dominance 
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
Display_Std <- t(rbind(x[["General_Dominance"]], x[["Standardized"]], x[["Ranks"]]))
dimnames(Display_Std) <- list(names(x[["Ranks"]]), c("General_Dominance", "Standardized", "Ranks"))
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

