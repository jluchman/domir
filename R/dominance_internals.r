#' @title Internal dominance analysis computation function assuming scalar 
#' output
#'
#' @keywords internal
#'
#' @name dominance_scalar
#' 
#' @rdname dominance_scalar
#' 
#' @export
dominance_scalar <- 
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
  
  # Obtain conditional dominance statistics ----
  
  if (conditional) {
    
    # allocate conditional dominance matrix for all names
    Conditional_Dominance <- 
      matrix(nrow = name_count, 
             ncol = name_count) 
    
    # compliment subsets matrix (used for computing increments below)
    Subset_matrix_complement <-!Subset_matrix 
    
    # vector of number of names in a subset (used to arrange values)
    # assumes coercion of logicals to integers
    Names_per_subset <- rowSums(Subset_matrix)
    
    # vector of number of combinations total associated with each subset
    # each element in 'result_vector' receives a value associated with the 
    # number of combinations possible given the number of names in that 
    # specific subset - note that this is used in averaging later
    How_many_combins_at_name_count <- 
      sapply(Names_per_subset, 
             function(number_of_names_in_subset) 
               choose(name_count, number_of_names_in_subset),
             simplify = TRUE, USE.NAMES = FALSE)
    
    # Identical to 'How_many_combins_at_name_count' but associated with the 
    # number of combinations possible given the number of names in that 
    # from a subset that would be subtracted from/is an increment beyond that 
    # element - hence, is the number of combinations at a 'decrement' to name 
    # count - note that this is also used in averaging later
    How_many_combins_at_name_count_decrement <- 
      sapply(Names_per_subset, 
             function(number_of_names_in_subset) 
               choose(name_count - 1, number_of_names_in_subset),
             simplify = TRUE, USE.NAMES = FALSE)
    
    # creates a matrix for each 'TRUE' in 'Subset_matrix' the value of 
    # 'Result_vector' is plugged in.  This value from 'Result_vector' is 
    # divided by (i.e., **-1) the difference between the number of combinations 
    # at the number of names in the current model and the number of combinations 
    # at the number of names in the 'decrement' number of names model--which 
    # corresponds with the number of models that need to be averaged for that 
    # name with that number of names in the model
    # Can be thought of as a set of weighted results associated with each 
    # name in the 'Subset_matrix'
    # Note that these results are used in summing below
    Weighted_result_matrix <- 
      (
        (Subset_matrix*(
          How_many_combins_at_name_count - 
            How_many_combins_at_name_count_decrement))**-1
        )*Result_vector
    
    # 'Inf' values created above are replaced as 0
    Weighted_result_matrix <-
      replace(Weighted_result_matrix, 
              Weighted_result_matrix==Inf, 0)
    
    # Identical in intention to 'Weighted_result_matrix' but associated with 
    # 'Results_vector' for values of results not included in 
    # the number of combinations at the number of names in the 'decrement' 
    # number of names model
    # Can also be thought of as a set of weighted results associated with each 
    # name in the 'Subset_matrix'
    # Note that these results are also used in summing below
    Weighted_result_matrix_decrement <- 
      (
        (Subset_matrix_complement*How_many_combins_at_name_count_decrement)**-1
      )*Result_vector
    
    # 'Inf' values created above are replaced as 0
    Weighted_result_matrix_decrement <-
      replace(Weighted_result_matrix_decrement, 
              Weighted_result_matrix_decrement==Inf, 0)
    
    # across names (represented in rows); sum weighted results by number of 
    # names in models
    # the 'Weighted_result_matrix' sum represents the contribution the focal 
    # name has to the value
    # the 'Weighted_result_matrix_decrement' adjusts the 
    # 'Weighted_result_matrix' sum for value associated with the other names
    # Result is transposed to ensure rows correspond with names (cols 
    # correspond with names in 'Weighted_result...' matrices)
    for (names_in_model in 1:name_count) {
      
      Conditional_Dominance[, names_in_model] <- 
        t( 
          colSums(
            Weighted_result_matrix[Names_per_subset==names_in_model,]
            ) - 
            colSums(
              Weighted_result_matrix_decrement[Names_per_subset==(
                names_in_model-1
                ),]
              ) 
        )
      
    }
    
    # adjust one name in model results for .adj and .all models
    Conditional_Dominance[,1] <- 
      Conditional_Dominance[,1] - result_adjustment
    
  }
  
  else Conditional_Dominance <- NULL
  
  # Obtain complete dominance statistics ----
  
  ## ~~ todo: redesign cpt dom to capture %of models as statistic ----
  
  if (complete) {
    
    # allocate complete domianance container matrix
    Complete_Dominance <- 
      matrix(nrow = name_count, 
             ncol = name_count)
    
    # `combn` produces all possible pairs of names (implemented here as 
    # column numbers for selection from 'Subset_matrix')
    All_name_pairs <- 
      utils::combn(1:name_count, 2) 
    
    for (name_pair in 1:ncol(All_name_pairs)) {
      
      # which columns/names are chosen?
      Which_name_pair <- All_name_pairs[, name_pair] 
      
      # which columns/names are not?
      Which_not_name_pair <- 
        setdiff(1:name_count, Which_name_pair) 
      
      # create an additional row associated with 'Subset_matrix' that flags 
      # models where one, and only one (never both, ever none), of the two 
      # chosen names is present 
      Subset_matrix_flag_singleton <- 
        cbind(Subset_matrix, 
              1:nrow(Subset_matrix))[ 
                rowSums( Subset_matrix[,Which_name_pair] )==1, ] 
      
      # sort/`order` the updated 'Subset_matrix' by names not chosen 
      # such that names that the names that are chosen are always one row away 
      # from one another in the matrix (a convenience used below for 
      # comparisons)
      # `do.call` necessary here to get the matrices created to read as 
      # input for sorting
      Subset_matrix_sort_singleton <- 
        Subset_matrix_flag_singleton[
          do.call("order",
                  as.data.frame(
                    Subset_matrix_flag_singleton[,c(Which_not_name_pair, 
                                                    Which_name_pair)])), ] 
      
      # selects 'Result_vector' entries that meet criteria for inclusion 
      # in complete dominance
      # binds together columns of results selected from 'Result_vector' by 
      # using 'Subset_matrix_sort_singleton'-s last row (which flags where 
      # there is only a single relevant name) where one column includes 
      # results at row 'n' and the other includes results at row 'n + 1' 
      # (i.e., '%%' and the use of sorting described above); hence, the 
      # results at 'n' and 'n + 1' are now aligned on the same row
      Sorted_results_pair <-
        cbind( 
          Result_vector[ 
            Subset_matrix_sort_singleton[
              (1:nrow(Subset_matrix_sort_singleton) %% 2)==0, 
              ncol(Subset_matrix_sort_singleton)]], 
          Result_vector[ 
            Subset_matrix_sort_singleton[
              (1:nrow(Subset_matrix_sort_singleton) %% 2)==1, 
              ncol(Subset_matrix_sort_singleton)]] 
        )
      
      # compute complete dominance designation
      # if all paired results in column 1 are greater; indicate 'FALSE'
      # if all paired results in column 2 are greater; indicate 'TRUE'
      # otherwise, it's 'NA'
      Complete_Designation <- 
        ifelse(
          all(Sorted_results_pair[,1] > Sorted_results_pair[,2]), 
          FALSE,
          ifelse(
            all(Sorted_results_pair[,1] < Sorted_results_pair[,2]), 
            TRUE, 
            NA)
          )
      
      # fill in designation
      Complete_Dominance[ Which_name_pair[[2]], Which_name_pair[[1]] ] <-
        Complete_Designation
      
      # fill complementary designation
      Complete_Dominance[ Which_name_pair[[1]], Which_name_pair[[2]] ] <- 
        !Complete_Designation
      
    }
    
  }
  
  else Complete_Dominance <- NULL
  
  # reverse the complete dominance indication if `reverse`-d
  if (reverse == TRUE) Complete_Dominance <- 
    !Complete_Dominance 
  
  # Obtain general dominance statistics ----
  
  # results in this section repeat some of the resuls in the conditional 
  # dominance section, specifically 'Subset_matrix_complement', 
  # 'Names_per_subset', 'How_many_combins_at_name_count', and 
  # 'How_many_combins_at_name_count_decrement'
  # Each of these are described in that section above
  if (!conditional) {
    
    Subset_matrix_complement <-!Subset_matrix
    
    Names_per_subset <- rowSums(Subset_matrix) 
    
    How_many_combins_at_name_count <- 
      sapply(Names_per_subset, 
             function(number_of_names_in_subset) 
               choose(name_count, number_of_names_in_subset),
             simplify = TRUE, USE.NAMES = FALSE)
    
    How_many_combins_at_name_count_decrement <- 
      sapply(Names_per_subset, 
             function(number_of_names_in_subset) 
               choose(name_count - 1, number_of_names_in_subset),
             simplify = TRUE, USE.NAMES = FALSE)
    
    # creates a matrix for each 'TRUE' in 'Subset_matrix' the value of 
    # 'Result_vector' is plugged in.  This value is the difference between 
    # the number of combinations at the number of names in the current model 
    # and the number of combinations at the number of names in the 'decrement' 
    # number of names model--which corresponds with the number of models that 
    # need to be averaged for that name with that number of names in the model
    # Can be thought of as a set of weighted results associated with each 
    # name in the 'Subset_matrix'
    # Note that these results are used in summing below
    Unique_combins <- 
      Subset_matrix*(How_many_combins_at_name_count - 
                       How_many_combins_at_name_count_decrement)
    
    # identical in intention to 'Unique_combins' but associated with 
    # names not in the current subset - also made negative as it 
    # this value is associated with the names in subset/models being 
    # adjusted for 
    Unique_combins_decrement <- 
      (Subset_matrix_complement*How_many_combins_at_name_count_decrement)*-1
    
    # create matrix of positive weights associated with 'Unique_combins' and 
    # negative values associated with 'Unique_combins_decrement' - these 
    # effective reproduce conditional dominance statistics
    # conditional dominance is averaged to get general; hence multiplication 
    # by number of names
    # all values are inverted to serve as weights in the sum below (to make it 
    # an average)
    Weight_Matrix <- 
      ((Unique_combins + Unique_combins_decrement)*name_count)^-1
    
    # general dominance is weighted sum of all results and weight matrix 
    # by column (colums associated with different names)
    General_Dominance <- 
      colSums(Result_vector*Weight_Matrix)
    
    # adjust general dominance for .adj and .all models - only needs fixing 
    # in model where there is a single name in a subset; hence divided by 
    # the number of names total (in all other models will have been 
    # adjusted for in the averaging process with the '_decrement' matrix; there 
    # is no value for models with 0 names to adjust the 1 name subsets)
    General_Dominance <- 
      General_Dominance - result_adjustment/name_count 
    
  }
  
  # general dominance is also the average of all the conditonal dominance 
  # statisics when available
  else General_Dominance <- 
    rowMeans(Conditional_Dominance)
  
  # Obtain overall fit statistic and ranks ----
  
  Value <- 
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
         Value = Value)
  
  return(return_list)
  
}