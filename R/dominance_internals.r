#' @title Dominance analysis meta-function that returns scalar
#' @description Internal dominance analysis computation function assuming scalar
#' or vector of length 1 returned value.
#'
#' Not intended to be called by the user.
#'
#' @keywords internal
#' @name dominance_scalar
#' @rdname dominance_scalar
#' @export
dominance_scalar <-
  function(function2call, args_list,
           value_w_all_names,
           do_cdl, do_cpt, reverse,
           cluster, progress) {
    # generate subsets ----
    name_count <- length(args_list$RHS)
    # TRUE/FALSE list for in-/excluding name
    in_out_namelist <- lapply(1:name_count, function(name) c(FALSE, TRUE))
    # matrix of combinations that in-/exclude name
    # rows are different subsets; columns are subset names
    # empty subset removed; it is .adj and/or .all value
    subset_matrix <-
      expand.grid(in_out_namelist, KEEP.OUT.ATTRS = FALSE)
    subset_filter <- apply(subset_matrix, 1, any)
    subset_matrix <- subset_matrix[subset_filter, ]
    # adjust values with '.adj' ----
    adj_value <- args_list$.adj
    result_adjustment <- ifelse(is.null(adj_value), 0, adj_value)
    # adjust values with '.all' ----
    all_value <- args_list$.all
    result_adjustment <-
      ifelse(is.null(all_value), result_adjustment, all_value)
    # obtain values from all subsets ----
    # set progress bars when requested
    if (progress) {
      pg_bar <-
        utils::txtProgressBar(min = 0, max = nrow(subset_matrix) - 1,
                              style = 3)
    } else {
      pg_bar <- NULL
    }
    # function takes integer value and selects row from 'subset_matrix'
    # 'subset_matrix' row is coerced to logical vector
    # logical vector is appended to other arguments to `function2call`
    # all arguments passed to function2call` where value is returned
    obtain_value <-
      function(subset, pg_bar) {
        if (!is.null(pg_bar)) utils::setTxtProgressBar(pg_bar, subset)
        lgl_select_vector <- unlist(subset_matrix[subset, ])
        value_fct_args <- list(Selector_lgl = lgl_select_vector)
        value_fct_args <- append(value_fct_args, args_list)
        do.call(function2call, value_fct_args)
      }
    # vector of values from '.fct'; excludes subset of all names selected
    if (!is.null(cluster)) {
      value_vector <-
        parallel::parSapply(
          cl = cluster,
          1:(nrow(subset_matrix) - 1),
          function(subset) obtain_value(subset, pg_bar),
          simplify = TRUE, USE.NAMES = FALSE
        )
    } else {
      value_vector <-
        sapply(1:(nrow(subset_matrix) - 1),
               function(subset) obtain_value(subset, pg_bar),
               simplify = TRUE, USE.NAMES = FALSE)
    }
    # append value for subset of all names selected
    value_vector <- append(value_vector, value_w_all_names)
    # compute conditional dominance statistics ----
    if (do_cdl) {
      # allocate conditional dominance matrix for all names
      conditional_dominance <- matrix(nrow = name_count, ncol = name_count)
      # compliment subsets matrix; used for computing increments
      subset_matrix_complement <- !subset_matrix
      # count number of included names in each subset
      name_count_by_subset <- rowSums(subset_matrix)
      # tally up possible combinations of included names
      # for each subset in 'subset_matrix'
      combo_count_by_subset <- choose(name_count, name_count_by_subset)
      # tally up possible combinations of included names at one less name
      # for each subset in 'subset_matrix'
      combo_1ls_count_by_subset <- choose(name_count - 1, name_count_by_subset)
      # by subset, compute number of combinations that include each name
      wgtd_result_matrix <-
        combo_count_by_subset - combo_1ls_count_by_subset
      # associate these combinations with each selected name;
      # fills in for all TRUE entries
      wgtd_result_matrix <- subset_matrix * wgtd_result_matrix
      # turn this count into a weight; weight is inverse of count
      wgtd_result_matrix <- wgtd_result_matrix**-1
      # distribute values to all nonmissing values
      wgtd_result_matrix <- wgtd_result_matrix * value_vector
      # 'Inf' values created above are replaced as 0
      wgtd_result_matrix <-
        replace(wgtd_result_matrix,
                abs(wgtd_result_matrix) == Inf, 0)
      # repeat above process with combinations at one less name
      wgtd_1ls_result_matrix <-
        subset_matrix_complement * combo_1ls_count_by_subset
      wgtd_1ls_result_matrix <- wgtd_1ls_result_matrix**-1
      wgtd_1ls_result_matrix <- wgtd_1ls_result_matrix * value_vector
      wgtd_1ls_result_matrix <-
        replace(wgtd_1ls_result_matrix,
                abs(wgtd_1ls_result_matrix) == Inf, 0)
      # for each 'order'/number of names contributing to value
      for (contrib_count in 1:name_count) {
        # subset values to include just those with focal number of names
        values_subset <-
          wgtd_result_matrix[name_count_by_subset == contrib_count, ]
        # subset values with one less names
        values_subset_1ls <-
          wgtd_1ls_result_matrix[name_count_by_subset == contrib_count - 1, ]
        # conditional dominance is difference between weighted sums of
        # values at focal number of names and one less;
        # this effectively creates the average of the increments
        # associated with each name at the focal number of names contributing to
        # the value
        conditional_dominance[, contrib_count] <-
          t(colSums(values_subset) - colSums(values_subset_1ls))
      }
      # adjust values at one name in model results for '.adj' and '.all'
      conditional_dominance[, 1] <-
        conditional_dominance[, 1] - result_adjustment
      # if '.cdl' was FALSE
    } else {
      conditional_dominance <- NULL
    }
    # obtain complete dominance statistics ----
    if (do_cpt) {
      # allocate complete dominance container matrix
      complete_dominance <- matrix(nrow = name_count, ncol = name_count)
      # generate all combinations of two names
      # names are locations in matrix
      all_name_pairs <- utils::combn(1:name_count, 2)
      for (name_pair in seq_len(ncol(all_name_pairs))) {
        # select two names by location
        selected_name_pair <- all_name_pairs[, name_pair]
        # indicate which names are not selected
        unselected_names <- setdiff(1:name_count, selected_name_pair)
        # generate version of 'subset_matrix' with row id
        selected_names_matrix <-
          cbind(subset_matrix, seq_len(nrow(subset_matrix)))
        # generate vector flagging locations in 'subset_matrix' where
        # one name of the two selected names is present
        subsets_one_name <- rowSums(subset_matrix[, selected_name_pair]) == 1
        # filter 'selected_names_matrix' to obtain all rows where
        # one, never both or neither, names are a value generator
        selected_names_matrix <- selected_names_matrix[subsets_one_name, ]
        # generate matrix that places un-selected names earlier in
        # sorting order and selected names last to ensure they are
        # contiguous vertically in matrix
        sorting_matrix <-
          selected_names_matrix[, c(unselected_names, selected_name_pair)]
        # coerce 'sorting_matrix' to `data.frame` for use in `order()`
        sorting_df <- as.data.frame(sorting_matrix)
        # sort rows of 'selected_names_matrix' by forced evaluation of
        # 'sorting_df' by `do.call` with `order`
        selected_names_sorted <-
          selected_names_matrix[do.call("order", sorting_df), ]
        # generate indicator for location of first name in
        # 'selected_names_sorted'; always even number index
        first_name_locs <- (seq_len(nrow(selected_names_sorted)) %% 2) == 0
        # generate mapping of 'selected_names_sorted' locations to
        # locations in 'value_vector'
        first_name_index <-
          selected_names_sorted[first_name_locs, ncol(selected_names_sorted)]
        # generate vector selecting all values associated with first name
        first_name_values <- value_vector[first_name_index]
        # generate indicator for location of second name in
        # 'selected_names_sorted'; always odd number index
        second_name_locs <- (seq_len(nrow(selected_names_sorted)) %% 2) == 1
        # apply same process as in first name to second name
        second_name_index <-
          selected_names_sorted[second_name_locs, ncol(selected_names_sorted)]
        second_name_values <- value_vector[second_name_index]
        # bind first and second names' values in matrix
        sorted_results_pair <- cbind(first_name_values, second_name_values)
        # comparing first name's values to second
        first_vs_second <- sorted_results_pair[, 1] > sorted_results_pair[, 2]
        # comparing second name's values to first
        second_vs_first <- sorted_results_pair[, 1] < sorted_results_pair[, 2]
        # record designation in container matrix
        complete_dominance[selected_name_pair[[2]], selected_name_pair[[1]]] <-
          mean(second_vs_first)
        # record designation of complementary comparison in container matrix
        complete_dominance[selected_name_pair[[1]], selected_name_pair[[2]]] <-
          mean(first_vs_second)
      }
      # if '.cpt' was FALSE
    } else {
      complete_dominance <- NULL
    }
    # reverse the complete dominance indication if '.rev'
    if (reverse) complete_dominance <- 1 - complete_dominance
    # obtain general dominance statistics ----
    # if '.cdl' is false, implement general dominance statistic
    # computational method
    if (!do_cdl) {
      # implement some otherwise conditional dominance processes
      subset_matrix_complement <- !subset_matrix
      name_count_by_subset <- rowSums(subset_matrix)
      combo_count_by_subset <- choose(name_count, name_count_by_subset)
      combo_1ls_count_by_subset <- choose(name_count - 1, name_count_by_subset)
      # generates number of 'unique' combinations the focal name has at
      # specific number of value generating names
      uniq_cmb_count <-
        subset_matrix * (combo_count_by_subset - combo_1ls_count_by_subset)
      # if there is an '.adj' and/or '.all' model, adjust models with 1 name
      # for that value; other models adjust automatically given increment
      if (result_adjustment > 0)
        value_vector <-
          replace(
            value_vector,
            combo_count_by_subset == 1,
            value_vector[combo_count_by_subset == 1] - result_adjustment
          )
      # generates number of 'unique' combinations the focal name has at
      # specific number of value generating names not considering self; note
      # use of complement matrix and reflection over 0 to get increments
      uniq_cmb_1ls_count <-
        (subset_matrix_complement * combo_1ls_count_by_subset) * -1
      # combine unique combination matrices and invert; values are now weights
      # to be used in a weighted average
      wgt_mat <- ((uniq_cmb_count + uniq_cmb_1ls_count) * name_count)^-1
      # implement sum by column to get weighted average of value increments
      # by name
      general_dominance <- colSums(value_vector * wgt_mat)
      # if '.cdl' is TRUE; general dominance is average of cdl dominance
    } else {
      general_dominance <- rowMeans(conditional_dominance)
    }
    # obtain overall fit statistic and ranks ----
    # replace result adjustment for overall value
    value <- sum(general_dominance) + result_adjustment
    # compute ranks; reverse if '.rev'
    if (reverse == FALSE) {
      gnrl_ranks <- rank(-general_dominance)
    } else {
      gnrl_ranks <- rank(general_dominance)
    }
    # finalize returned values and attributes ----
    list(
      General_Dominance = general_dominance,
      General_Dominance_Ranks = gnrl_ranks,
      Conditional_Dominance = conditional_dominance,
      Complete_Dominance = complete_dominance,
      All_result = all_value,
      Adj_result = adj_value,
      Value = value
    )
  }
