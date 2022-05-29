#' @exportS3Method 

domin.old_method <- 
  function(formula_overall, reg, fitstat, sets = NULL, all = NULL, 
           conditional = TRUE, complete = TRUE, consmodel = NULL, 
           reverse = FALSE, ...) {
    
    print("old method!") ##
    
    warning("'formula_overall', 'reg', and 'fitstat' arguments are ", 
            "depreciated as of domir 1.0.\nPlease use 'object2domin', ", 
            "'model', and 'metric' arguments.", 
            call. = FALSE)
    
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