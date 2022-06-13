#' @title Internal dominance analysis computation function assuming scalar 
#' output
#'
#' @keywords internal
#'
#' @name domir_scalar
#' 
#' @rdname domir_scalar
#' 
#' @export
domir_scalar <- 
  # may have to rename if using this for a visible function that returns 
  # a scalar
  function(fitting_fun, args_list, cons_args, full_fit, 
           conditional, complete, reverse) {
    
  # Subset processing ----
  
  # total number of models to be run (assuming a .adj model)
  subsets_total <- 2^(length(args_list$RHS))
  
  # number of subset producing names
  name_count <-length(args_list$RHS)
    
  # Create subset selection matrix ----
  
  # generate logical matrix indicating selection of subset producing name;
  # rows are separate subsets; columns are subset names;
  # empty subset is removed - conceptually is .adj model
  Subset_matrix <- 
    expand.grid(
      lapply(1:name_count, 
             function(name) c(FALSE, TRUE)), 
      KEEP.OUT.ATTRS = FALSE)[-1,]
  
  # Constant model adjustments ----
  
  if (length(args_list$.adj) > 0) {
    
    result_adjustment <- 
      Adj_result <- 
      do.call(fitting_fun, 
              append(list(Selector_lgl = NULL), 
                     cons_args))
    
  }
  
  else {
    
    Adj_result <- NULL
    
    result_adjustment <- 0
    
  }
  
  # All subsets adjustment ----
  
  if (length(args_list$.all) > 0) {
    
    result_adjustment <- 
      All_result <- 
      do.call(fitting_fun, 
              append(list(Selector_lgl = NULL), 
                     args_list))
    
  }
  
  else All_result <- NULL 
  
  # Obtain all subsets regression results ----
  
  # `sapply` constructs vector of results from '.fct' from `domir`; proceeds 
  # selecting row from 'Subset_matrix' to submit to `fitting_fun`, with 
  # required arguments called in `do.call`; all subsets save the subset 
  # with all names (which was estimated as a check in `domir` and passed)
  Result_vector <- 
    sapply(1:(nrow(Subset_matrix) - 1),
           function(subset) { 
             do.call(fitting_fun, 
                     append(list(Selector_lgl = unlist(Subset_matrix[subset,])),
                            args_list))
           },
           simplify = TRUE, USE.NAMES = FALSE
    )
  
  # append full model's result
  Result_vector <- 
    append(Result_vector, full_fit)
  
  # ~~review ended here ~~ ----
  
  # Obtain conditional dominance statistics ----
  
  if (conditional) {
    
    Conditional_Dominance <- 
      matrix(nrow = name_count, 
             ncol = name_count) # conditional dominance container
    
    Subset_matrix_Anti <-!Subset_matrix # complement of logical IV inclusion matrix
    
    IVs_per_Model <- rowSums(Subset_matrix) # count IVs in each model using logical IV inclusion matrix
    
    Combins_at_Order <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(name_count, x), # ... compute # of combinations associated with each row given the total number of IVs
             simplify = TRUE, USE.NAMES = FALSE)
    
    Combins_at_Order_Prev <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(name_count - 1, x), # ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
             simplify = TRUE, USE.NAMES = FALSE)
    
    Weighted_Order_Ensemble <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model; these values are associated with models that have the focal IV (hence multiplied by logical matrix).  This matrix is inverted to make weights that can be used for weighted averaging.  Finally, these weights are multiplied by all the fit statistics
      ((Subset_matrix*(Combins_at_Order - Combins_at_Order_Prev))**-1)*Result_vector
    
    Weighted_Order_Ensemble <- # effectively missing values (Inf) are changed to 0 to remove from summing
      replace(Weighted_Order_Ensemble, 
              Weighted_Order_Ensemble==Inf, 0)
    
    Weighted_Order_Ensemble_Anti <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model; these values are associated with models that have do not have focal IV and would thus be used to subtract out of models that do (hence the reflection across 0). This matrix is inverted to make weights that can be used for weighted averaging.  Finally, these weights are multiplied by all the fit statistics
      ((Subset_matrix_Anti*Combins_at_Order_Prev)**-1)*Result_vector
    
    Weighted_Order_Ensemble_Anti <-# effectively missing values (Inf) are changed to 0 to remove from summing
      replace(Weighted_Order_Ensemble_Anti, 
              Weighted_Order_Ensemble_Anti==Inf, 0)
    
    for (order in 1:name_count) {
      
      Conditional_Dominance[, order] <- # the column associated with an order (# of IVs) in the model is replaced by ...
        t( # ... transpose of ...
          colSums(Weighted_Order_Ensemble[IVs_per_Model==order,]) - # ... the sum of the weighted fit stats at a with a specified number of IVs in the model for all models including the focal IV (across all rows) ...
            colSums(Weighted_Order_Ensemble_Anti[IVs_per_Model==(order-1),]) # ... subtracting out the sum of the weighted fit stats with one less than a specified number of IVs in the model for all models that do not contain the focal IV (across all rows)
        )
      
    }
    
    Conditional_Dominance[,1] <- Conditional_Dominance[,1] - result_adjustment # adjust the first set of conditional dominance statistics for the constant and/or all fitstat adjustments
    
  }
  
  else Conditional_Dominance <- NULL
  
  # Obtain complete dominance statistics ----
  
  if (complete) {
    
    Complete_Dominance <- 
      matrix(data = NA, nrow = name_count, ncol = name_count) # complete dominance container; initialize with missing values
    
    Complete_Combinations <- 
      utils::combn(1:name_count, 2) # produce all pairs of IVs (indicated by column numbers in logical matrix)
    
    for (pair in 1:ncol(Complete_Combinations)) { # looping across all pairs of IVs...
      
      Focal_Cols <- Complete_Combinations[, pair] # identify which columns are associated with the focal unique pair of IVs
      
      NonFocal_Cols <- setdiff(1:name_count, Focal_Cols) # find the columns associated with the complement of IVs not selected in the unique pair
      
      Select_2IVs <- 
        cbind(Subset_matrix, 1:nrow(Subset_matrix))[ # combine logical IV inclusion matrix with vector of row indicators (used to select specific rows in the fitstat vector) ...
          rowSums( Subset_matrix[,Focal_Cols] )==1, ] #... then take only the rows/models associated with having only one of either of the focal IVs (never both or neither IVs)
      
      Sorted_2IVs <- 
        Select_2IVs[
          do.call("order", # sort/order the logical and row indicator matrix...
                  as.data.frame(Select_2IVs[,c(NonFocal_Cols, Focal_Cols)])), ] #... based on the non included IVs - this forces non-included IVs to "pair" sequentially; that is, models with focal IV 1 are at vector location 'n' and models with focal IV 2 are at vector location 'n + 1' and all the non-focal IVs are identical between those two models (which is required for a valid complete dominance comparison)
      
      Compare_2IVs <-
        cbind( # bind in a matrix as different columns...
          Result_vector[ # ... fit statistics ...
            Sorted_2IVs[(1:nrow(Sorted_2IVs) %% 2)==0, ncol(Sorted_2IVs)]], # ... selected based on the row indicators (i.e., last row of the sorted matrix produced above), in all 'n' row positions (i.e., modulus of 0 with frequency 2)...
          Result_vector[ # ... as well as the fit statistics ...
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
    
    Subset_matrix_Anti <-!Subset_matrix # complement of logical IV inclusion matrix
    
    IVs_per_Model <- rowSums(Subset_matrix) # count IVs in each model using logical IV inclusion matrix
    
    Combins_at_Order <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(name_count, x), # ... compute # of combinations associated with each row given the total number of IVs
             simplify = TRUE, USE.NAMES = FALSE)
    
    Combins_at_Order_Prev <- 
      sapply(IVs_per_Model, # for each IV/column, ... 
             function(x) choose(name_count - 1, x),# ... compute # of combinations associated with each row given the total number of IVs (i.e., # of combinations _not_ including focal IV)
             simplify = TRUE, USE.NAMES = FALSE)
    
    Indicator_Weight <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (surrogate for what conditional dominance statistics do); these values are associated with models that have the focal IV
      Subset_matrix*(Combins_at_Order - Combins_at_Order_Prev)
    
    Indicator_Weight_Anti <- # create a matrix indicating how many unique combinations are associated with each number of IVs in the model (again, surrogate for what conditional dominance statistics do); these values are associated with models that have do not have focal IV and would thus be used to subtract out of models that do (hence the reflection across 0)
      (Subset_matrix_Anti*Combins_at_Order_Prev)*-1
    
    Weight_Matrix <- # combine combinations of focal and non-focal counts associated with what the conditional dominance statistics would do with the number of IV average associated with what the general dominance statistics do (arithmetic average of all conditional dominance statistics); these combinations are inverted to make them function as a set of weights that produce averages 
      ((Indicator_Weight + Indicator_Weight_Anti)*name_count)^-1
    
    General_Dominance <- # general dominance is sum of the product of the weight matrix above and all the fit statistics
      colSums(Result_vector*Weight_Matrix)
    
    General_Dominance <- General_Dominance - result_adjustment/name_count # adjust the general dominance statistics for the constant and/or all fitstat adjustments in the first/order 1 stats as conditional dominance would do
    
  }
  
  else General_Dominance <- rowMeans(Conditional_Dominance) #... otherwise average conditional dominance by IV/row to produce general dominance statistics
  
  # Obtain overall fit statistic and ranks ----
  
  FitStat <- 
    sum(General_Dominance) + result_adjustment # adjust overall fit statistic by replacing all subsets component and constant model component
  
  if (reverse == FALSE) General_Dominance_Ranks <- rank(-General_Dominance) # rank general dominance statistic if fitstat value increases (i.e., `reverse` == FALSE)
  else General_Dominance_Ranks <- rank(General_Dominance) # rank general dominance statistic if fitstat value decreases (i.e., `reverse` == TRUE)
  
  # Finalize returned values and attributes ----
  
  return_list <- 
    list(General_Dominance = General_Dominance, 
         General_Dominance_Ranks = General_Dominance_Ranks, 
         Conditional_Dominance = Conditional_Dominance, 
         Complete_Dominance = Complete_Dominance, 
         All_result = All_result, 
         Adj_result = Adj_result, 
         FitStat = FitStat)
  
  return(return_list)
  
}