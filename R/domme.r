#' @title Internal dominance analysis computation function
#'
#' @keywords internal
#'
#' @export
domme <- function(component_list, conditional, complete, reverse) {
  
  # Create independent variable/set combination list ----
  
  Combination_Matrix <- 
    expand.grid(
      lapply(1:component_list$Total_Combination_N, 
             function(x) c(FALSE, TRUE)), 
      KEEP.OUT.ATTRS = FALSE)[-1,] # Logical matrix of inclusion/exclusion for IVs; omit the first, empty row
  
  Total_Models_to_Estimate <- 2**component_list$Total_Combination_N - 1 # total number of models to estimate
  
  # Constant model adjustments ----
  
  if (length(component_list$args_list$consmodel) > 0) { # if there are entries in consmodel...
    
    FitStat_Adjustment <- Cons_Result <- 
      do.call(component_list$fitting_fun, append(x = list(Indep_Var_Combin_lgl  = NULL), values = component_list$cons_args))# ...obtain their fitstat' value using `doModel_Fit()`
    
  }
  
  else {
    Cons_Result <- NULL # ...otherwise return a null
    FitStat_Adjustment <- 0
  }
  
  # All subsets adjustment ----
  
  if (length(component_list$args_list$all) > 0) { # if there are entries in all...
    
    FitStat_Adjustment <- All_Result <- do.call(component_list$fitting_fun, append(x = list(Indep_Var_Combin_lgl  = NULL), values = component_list$args_list)) # ...obtain their 'fitstat' value as well using `doModel_fit()`...
    
  }
  
  else All_Result <- NULL # ...otherwise return a null
  
  # Obtain all subsets regression results ----
  #print(append(x = list(Indep_Var_Combin_lgl = unlist(Combination_Matrix[1,])), values = component_list$args_list))  # clear
  
  Ensemble_of_Models <- 
    sapply(1:nrow(Combination_Matrix), # for all individual models/rows in the logical combination matrix...
           function(x) { # ... submit the row number ...
             do.call(component_list$fitting_fun, 
                     append(x = list(Indep_Var_Combin_lgl = unlist(Combination_Matrix[x,])), # ... select a unique pattern of IVs as a logical vector from the combination matrix ...
                         values = component_list$args_list))
           },
           simplify = TRUE, USE.NAMES = FALSE)
  
  # Obtain conditional dominance statistics ----
  
  if (conditional) {
    
    Conditional_Dominance <- 
      matrix(nrow = component_list$Total_Combination_N, ncol = component_list$Total_Combination_N) # conditional dominance container
    
    Combination_Matrix_Anti <-!Combination_Matrix # complement of logical IV inclusion matrix
    
    IVs_per_Model <- rowSums(Combination_Matrix) # count IVs in each model using logical IV inclusion matrix
    
    Combins_at_Order <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(component_list$Total_Combination_N, x), # ... compute # of combinations associated with each row given the total number of IVs
             simplify = TRUE, USE.NAMES = FALSE)
    
    Combins_at_Order_Prev <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(component_list$Total_Combination_N - 1, x),# ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
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
    
    for (order in 1:component_list$Total_Combination_N) {
      
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
      matrix(data = NA, nrow = component_list$Total_Combination_N, ncol = component_list$Total_Combination_N) # complete dominance container; initialize with missing values
    
    Complete_Combinations <- 
      utils::combn(1:component_list$Total_Combination_N, 2) # produce all pairs of IVs (indicated by column numbers in logical matrix)
    
    for (pair in 1:ncol(Complete_Combinations)) { # looping across all pairs of IVs...
      
      Focal_Cols <- Complete_Combinations[, pair] # identify which columns are associated with the focal unique pair of IVs
      
      NonFocal_Cols <- setdiff(1:component_list$Total_Combination_N, Focal_Cols) # find the columns associated with the complement of IVs not selected in the unique pair
      
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
             function(x) choose(component_list$Total_Combination_N, x), # ... compute # of combinations associated with each row given the total number of IVs
             simplify = TRUE, USE.NAMES = FALSE)
    
    Combins_at_Order_Prev <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(component_list$Total_Combination_N - 1, x),# ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
             simplify = TRUE, USE.NAMES = FALSE)
    
    Indicator_Weight <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (surrogate for what conditional dominance statistics do); these values are associated with models that have the focal IV
      Combination_Matrix*(Combins_at_Order - Combins_at_Order_Prev)
    
    Indicator_Weight_Anti <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (again, surrogate for what conditional dominance statistics do); these values are associated with models that have do not have focal IV and would thus be used to subtract out of models that do (hence the reflection across 0)
      (Combination_Matrix_Anti*Combins_at_Order_Prev)*-1
    
    Weight_Matrix <- # combine combinations of focal and non-focal counts associated with what the conditional dominance statistics would do with the number of IV average associated with what the general dominance statistics do (arithmetic average of all conditional dominance statistics); these combinations are inverted to make them function as a set of weights that produce averages 
      ((Indicator_Weight + Indicator_Weight_Anti)*component_list$Total_Combination_N)^-1
    
    General_Dominance <- # general dominance is sum of the product of the weight matrix above and all the fit statistics
      colSums(Ensemble_of_Models*Weight_Matrix)
    
    General_Dominance <- General_Dominance - FitStat_Adjustment/component_list$Total_Combination_N # adjust the general dominance statistics for the constant and/or all fitstat adjustments in the first/order 1 stats as conditional dominance would do
    
  }
  
  else General_Dominance <- rowMeans(Conditional_Dominance) #... otherwise average conditional dominance by IV/row to produce general dominance statistics
  
  # Obtain overall fit statistic and ranks ----
  
  FitStat <- 
    sum(General_Dominance) + FitStat_Adjustment # adjust overall fit statistic by replacing all subsets component and constant model component
  
  if (reverse == FALSE) General_Dominance_Ranks <- rank(-General_Dominance) # rank general dominance statistic if fitstat value increases (i.e., `reverse` == FALSE)
  else General_Dominance_Ranks <- rank(General_Dominance) # rank general dominance statistic if fitstat value decreases (i.e., `reverse` == TRUE)
  
  # Finalize returned values and attributes ----
  
  return_list <- list(General_Dominance = General_Dominance, General_Dominance_Ranks = General_Dominance_Ranks, Conditional_Dominance = Conditional_Dominance, Complete_Dominance = Complete_Dominance, All_Result = All_Result, Cons_Result = Cons_Result, FitStat = FitStat)
  
  return(return_list)
  
}