#' User-definable \code{formula} class object-based dominance analysis
#'
#' Uses \code{formula} class objects to build all model subsets
#' @param formula_overall an object of class \link{formula} for use in the functon in \code{reg}.
#' @param reg The function implementing the regression model called.
#' @param fitstat List indicating function implemeting fit statistic and list element used for dominance analysis (see details).
#' @param sets an optional \link{list} containing individual vectors of variable or factor names.  Each \link{vector} in the list is used as a set and always included together in the \code{formula}.
#' @param all an optional vector of variable or factor names to be built into the \code{formula} and included in all model subsets.
#' @param complete logical.  If \code{FALSE} then complete dominance matrix is not computed.
#' @param ... additional arguments passed to \code{reg} function.
#' @keywords relative importance dominance analysis shapley value
#' @export
#' @examples
#' domin(mpg ~ am + vs + cyl, "lm", list("summary", "r.squared"), data=mtcars)

domin <- function(formula_overall, reg, fitstat, sets=NULL, 
    all=NULL, complete=TRUE, ...) {
    
    # ~~ Exit conditions ~~ #
    
if (!methods::is(formula_overall, "formula")) stop(paste(formula_overall, "is not a formula object.  Coerce it to formula before use in domin."))

    # ~~ Create independent variable list ~~ #
    
Indep_Var_List <- attr(stats::terms(formula_overall), "term.labels") # obtain IV list

if (length(sets) > 0) { # if there are sets...
    
    set_aggregated <- sapply(sets, paste0, collapse=" + ") # ...paste together elements of set...
    
    Indep_Var_List <- append(Indep_Var_List, set_aggregated) # ... append to IV list
    
}

Dep_Var <- rownames(attr(stats::terms(formula_overall),"factors"))[[1]] # pull out DV

Total_Indep_Vars <- length(Indep_Var_List) # number of IVs in model

if (Total_Indep_Vars < 3) stop(paste("Total of", Total_Indep_Vars,"independent variables or sets. At least 3 needed for useful dominance analysis."))

    # ~~ Create independent variable combination list ~~ #
    
Combination_List <- lapply( (1:length(Indep_Var_List)), # use lapply() function to apply each distinct number of combination to ...
							function(Comb_Num) {utils::combn(Indep_Var_List, Comb_Num)} ) # ... combn() function using the the IV list to obtain all combinations

Total_Models_to_Estimate <- 2**Total_Indep_Vars - 1 # total number of models to estimate

#     ~~ Define function to call regression models ~~ #

Ensemble_Coordinator <- function(Indep_Var_combination, Dep_Var, reg, fitstat, all=NULL, ...) {

    formula_to_use <- stats::formula(paste0(Dep_Var, " ~ ", paste0(c(Indep_Var_combination, all), collapse = " + " )))

    temp_result <- list(do.call(reg, list(formula_to_use, ...)) )  # build function that processes list then calls regression
    
    if (length(fitstat) > 2) temp_result <- append(temp_result, fitstat[3:length(fitstat)]) # include additional arguments to fitstat
    
    fit_value <- do.call(fitstat[[1]], temp_result) # apply fitstat function
    
    return( list( # return fistat value as associated with IV combination
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
        Models_at_Indep_Var_number <- lapply(as.data.frame(Combination_List[[number_of_Indep_Vars]]), Ensemble_Coordinator, Dep_Var, reg, fitstat, all=all, ...) 
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
    "All" = all,
    "Sets" = sets
)
    
    class(return_list) <- c("domin","list")
    
    return(return_list)

}

#' Print method for \code{domin}
#'
#' Reports basic results from \code{domin} run
#' x an object of class "domin".
#' ... further arguments passed to or from other methods.
#' @keywords relative importance
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
if (length(x[["Sets"]])>0) {
    cat("Components of sets:\n")
    for (set in 1:length(x[["Sets"]])) {
        cat(paste0("set", set),":", x[["Sets"]][[set]], "\n")
    }
    cat("\n")
}
if (length(x[["All"]])>0) {
    cat("All subsets variables:", x[["All"]])
}
}

