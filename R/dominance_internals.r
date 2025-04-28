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
# TODO: restructure 'args_list' into directly submitted args to 'dominance _scalar()' ----
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
    # expand '.wst's ----
    if (!is.null(args_list$.wst)) {
      # obtain cols and 'selector_locations' of 'subset_matrix" 
      # associated with '.wst'
      wst_matrix <- as.matrix(subset_matrix[,args_list$.wst])
      wst_locs <- args_list$RHS[args_list$.wst]
      # list and locations of non-'.wst' names
      nonwst_loc <- 1:(ncol(subset_matrix)-length(wst_locs))
      if (nonwst_loc[length(nonwst_loc)] == 0) nonwst_loc <- 0 # when there are no other names, makes a reversed 1 0 vec
      nonwst_names <- names(subset_matrix)[nonwst_loc]
      # construct new names for '.wst' variables for `rbind`-ing
      wst_names <- 
        lapply(
          1:length(wst_locs),
          function(wst) {
            paste("wst_", wst, "_", 1:length(wst_locs[[wst]]), sep = "")
          }
        )
      names(wst_names) <- paste("wst", 1:length(wst_names), sep = "_")
      # create all combinations of names within a '.wst' element as separate 
      # lists. remove empty/all FALSE and full/all TRUE combinations within 
      # each '.wst' element
      in_out_namelist_wst <- 
        lapply(
          wst_locs,
          function(elem) {
            in_out_nmlst <- 
              lapply(1:length(elem), function(name) c(FALSE, TRUE))
            sub_mat <- 
              expand.grid(in_out_nmlst, KEEP.OUT.ATTRS = FALSE)[-1,]
          }
        )
      # create a data.frame that melds together the combinations created in 
      # 'in_out_namelist_wst' with other within-set name sets. This involves 
      # identifying whether there are other within-set name sets ('oth_wsts), 
      # how many names are in each of those other within-set name sets 
      # ('oth_wst_lens'), and the names in those other within-set name sets 
      # ('oth_wst_names'). These three factors are then combined to expand 
      # other '.wst' elements such, for each within-set name in each 
      # '.wst' element, that they all fill in TRUE or FALSE for all names 
      # corresponding with whether they are included or given 'wst_matrix'.
      # The names and namesets are then joined with the within-set names and 
      # bound together by row. Note that only TRUE entries in 'wst_matrix' 
      # are expanded.
      fmt_in_out_namelist_wst <- 
        lapply(
          1:ncol(wst_matrix),
          function(col) {
            wst_expanded_combs <-
              lapply(
                1:length(wst_matrix[,col]),
                function(row) {
                  if (wst_matrix[row, col] == TRUE) {
                    oth_wsts <- wst_matrix[row,-col]
                    oth_wst_lens <- sapply(wst_locs[-col], length)
                    oth_wst_names <- wst_names[-col]
                    if (length(oth_wsts) == 0) {
                      oth_wst_combs <- NULL
                    } else {
                      oth_wst_combs <- 
                        lapply(
                          1:length(oth_wsts),
                          function(elem) 
                            matrix(
                              rep(oth_wsts[elem], times = oth_wst_lens[elem]),
                              ncol = oth_wst_lens[elem],
                              dimnames = list(NULL, oth_wst_names[[elem]])
                            )
                        )
                    }
                    focal_wst <- in_out_namelist_wst[[col]]
                    colnames(focal_wst) <- wst_names[[col]]
                    if (is.null(oth_wst_combs)) {
                      focal_wst_w_other_names <- 
                        data.frame(subset_matrix[row, nonwst_loc], focal_wst)
                    } else {
                      focal_wst_w_other_names <- 
                        data.frame(
                          subset_matrix[row, nonwst_loc], 
                          focal_wst, 
                          oth_wst_combs
                        )
                    }
                    names(focal_wst_w_other_names)[nonwst_loc] <- nonwst_names
                    all_names <- c(nonwst_names, unlist(wst_names))
                    focal_wst_w_other_names <- 
                      focal_wst_w_other_names[,all_names]
                  }
                }
              )
            do.call("rbind", wst_expanded_combs)
          }
        )
      # 'subset_matrix' entries where all '.wst' elements are FALSE,
      # this expands the names of the columns associated with the '.wst' 
      # elements so that they may be row bound with those from 
      # 'fmt_in_out_namelist_wst'.
      subset_noninc_wst <-
        as.matrix(subset_matrix[apply(wst_matrix, 1, sum)==0, -args_list$.wst])
      if (nrow(subset_noninc_wst) == 0) {
        subset_wst_matrix <- do.call("rbind", fmt_in_out_namelist_wst)
      } else {
        subset_noninc_wst <-
          data.frame(
            subset_noninc_wst,
            matrix(
              rep(FALSE, times = length(unlist(wst_locs))),
              ncol = length(unlist(wst_locs))
            )
          )
        names(subset_noninc_wst) <- append(nonwst_names, unlist(wst_names))
        subset_inc_wst <- do.call("rbind", fmt_in_out_namelist_wst)
        subset_wst_matrix <- rbind(subset_noninc_wst, subset_inc_wst)
      }
      subset_wst_matrix <- 
        subset_wst_matrix[!duplicated(subset_wst_matrix, fromLast = TRUE),] #somewhat inelegant but removes needless duplicated row(s) introduced by the 'in_out_namelist_wst' -  might consider removing 1st and last entries in that process and re-adding in all wst in and all wst out from 'subset_matrix' instead
      # 'selector_locations' is inaccurate and will not map to 
      # 'subset_wst_matrix'. below 'selector_locations' is updated 
      # so all within-set names will map to columns.
      if (length(wst_locs) == length(args_list$RHS)) { # only wsts
        RHS_wst <- unlist(args_list$RHS)
      } else { # mixed wst and others
        RHS_wst <- 
          append(args_list$RHS[nonwst_loc], unlist(args_list$RHS[-nonwst_loc]))
      }
      subset_matrix <- subset_wst_matrix; print(subset_matrix) # ~~
      args_list$RHS <- RHS_wst
      name_count <- length(args_list$RHS)
    }
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
    value_vector <- append(value_vector, value_w_all_names); print(value_vector) # ~~
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
      if (is.null(args_list$.wst)) {
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
      } else {
        wst_counts <- 
          sum(grepl("^Var", colnames(subset_matrix))) + length(wst_names)
        conditional_dominance <- 
          matrix(nrow = name_count, ncol = wst_counts)
        for (contrib_count in 1:name_count) {
          relevant_subsets <- subset_matrix
          relevant_values <- value_vector
          name_count_by_subset <- rowSums(relevant_subsets)
          cdl_wst_filter <- rep(TRUE, times = nrow(subset_matrix))
          is_wst <- !grepl("^Var[0-9]+", names(subset_matrix)[contrib_count])
          if (is_wst) {
            if (length(wst_names) > 1) {
              which_wst <- 
                which(
                  sapply(
                    wst_names, 
                    function(names) 
                      any(grepl(names(subset_matrix[contrib_count]), names)))
                )
              for (wst in wst_names[-which_wst]) {
                cdl_wst_filter <-
                  apply(
                    subset_matrix[,wst], 1, 
                    function(row) (all(row) | !any(row))
                  ) & cdl_wst_filter
              }
            }
          } else {
            for (wst in wst_names) {
              cdl_wst_filter <-
                apply(
                  subset_matrix[,wst], 1, 
                  function(row) (all(row) | !any(row))
                ) & cdl_wst_filter
            }
          }
          relevant_subsets <- relevant_subsets[cdl_wst_filter,]
          relevant_values <- relevant_values[cdl_wst_filter]
          #relevant_name_count <- rowSums(relevant_subsets)
          non_wst_locs <- which(grepl("^Var", colnames(relevant_subsets)))
          if (length(non_wst_locs) > 0) {
            relevant_name_count <- 
              rowSums(as.matrix(relevant_subsets[, non_wst_locs]))
          } else {
            relevant_name_count <- relevant_values*0
          }
          for (wst in names(wst_names)) {
            wst_loc <- which(grepl(wst, colnames(relevant_subsets)))
            relevant_name_count <- 
              relevant_name_count + apply(relevant_subsets[, wst_loc], 1, any)
          }
          relevant_perms <-
            perm_computer(relevant_subsets, wst_names, names(subset_matrix)[contrib_count])
          for (inc_order in 1:wst_counts) {
            var_at_order <- # lgl to select
              relevant_name_count == inc_order & 
              relevant_subsets[, contrib_count]
            inc_at_n_subsets <- relevant_subsets[var_at_order, ] # models selected
            inc_at_n_values <- relevant_values[var_at_order]
            inc_at_n_ls_subsets <- inc_at_n_subsets
            inc_at_n_ls_subsets[, contrib_count] <- FALSE
            select_n_ls_values <- 
              unlist(
                apply(
                  inc_at_n_ls_subsets,
                  1, 
                  function(req_row) {
                    which(
                      apply(
                        relevant_subsets, 
                        1, 
                        function(try_row) {
                          all(req_row == try_row)
                        }
                      )
                    )
                  }
                )
              )
            # if (inc_order > 1) {
            #   inc_at_n_ls_values <- relevant_values[select_n_ls_values]*-1
            # } else {
            #   inc_at_n_ls_values <- result_adjustment*-1
            # }
            inc_at_n_ls_values <- relevant_values[select_n_ls_values]
            # #inc_at_n_wgts <- exp(relevant_perms[var_at_order])
            inc_at_n_wgts <- relevant_perms[var_at_order]
            # if (inc_order > 1) {
            #   inc_at_n_ls_wgts <- exp(relevant_perms[select_n_ls_values])
            # } else {
            #   inc_at_n_ls_wgts <- inc_at_n_wgts
            # }
            inc_at_n_ls_wgts <- relevant_perms[select_n_ls_values]
            # conditional_dominance[contrib_count, inc_order] <- 
            #   sum(inc_at_n_wgts*inc_at_n_values) + 
            #   sum(inc_at_n_ls_wgts*inc_at_n_ls_values)
            # conditional_dominance[contrib_count, inc_order] <-
            #   (sum(inc_at_n_values) - sum(inc_at_n_ls_values)
            #   )/length(inc_at_n_values)
            conditional_dominance[contrib_count, inc_order] <-
              exp(log(sum(exp((inc_at_n_wgts + log(inc_at_n_values)))) -
                  sum(exp(inc_at_n_ls_wgts + log(inc_at_n_ls_values)))) - 
              log(sum(exp(inc_at_n_wgts))))
          }
        }
      }
      # adjust values at one name in model results for '.adj' and '.all'
      #if (is.null(args_list$.wst))
      conditional_dominance[, 1] <- 
        conditional_dominance[, 1] - result_adjustment
    # if '.cdl' was FALSE
    } else {
      conditional_dominance <- NULL
    }; print(conditional_dominance) # ~~
    # obtain complete dominance statistics ----
    # !! cpt dom not done !! ----
    if (do_cpt) {
      # allocate complete dominance container matrix
      complete_dominance <- matrix(nrow = name_count, ncol = name_count)
      # generate all combinations of two names
      # names are locations in matrix
      all_name_pairs <- utils::combn(1:name_count, 2)
      # !! start new ----
      subset_0_matrix <- 
        rbind(
          matrix(
            rep(FALSE, times = ncol(subset_matrix)),
            nrow = 1,
            dimnames = list(NULL, colnames(subset_matrix))
          ),
          subset_matrix
        )
      value_0_vector <- c(result_adjustment, value_vector)
      # !! end new ----
      for (name_pair in seq_len(ncol(all_name_pairs))) {
        # select two names by location
        selected_name_pair <- all_name_pairs[, name_pair]; print(selected_name_pair) # ~~
        # indicate which names are not selected
        unselected_names <- setdiff(1:name_count, selected_name_pair)
        if (is.null(args_list$.wst)) {
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
        } else {
          is_wst_1 <- 
            !grepl("^Var[0-9]+", colnames(subset_matrix)[selected_name_pair[[1]]])
          is_wst_2 <- 
            !grepl("^Var[0-9]+", colnames(subset_matrix)[selected_name_pair[[2]]])
          if (is_wst_1 && is_wst_2) {
            is_same_wst <- 
              gsub("_[0-9]+$", "", colnames(subset_matrix)[selected_name_pair[[1]]]) == 
              gsub("_[0-9]+$", "", colnames(subset_matrix)[selected_name_pair[[2]]])
          } else {
            is_same_wst <- FALSE
          }
          if (is_same_wst) {
            which_wst <- gsub("_[0-9]+$", "", colnames(subset_matrix)[selected_name_pair[[1]]])
            cpt_count <- 
              length(grep("^Var[0-9]+", colnames(subset_matrix))) + 
              length(wst_names) - 1 + length(wst_names[[which_wst]]) - 2
            other_comb_names <- 
              c(grep("^Var[0-9]+", colnames(subset_matrix), value = TRUE), 
                names(wst_names)[which(names(wst_names) != which_wst)],
                setdiff(
                  wst_names[[which_wst]], 
                  c(names(subset_matrix)[[selected_name_pair[[1]]]],
                    names(subset_matrix)[[selected_name_pair[[2]]]])
                )
              )
          } else {
            cpt_count <- 
              length(grep("^Var[0-9]+", colnames(subset_matrix))) + 
              length(wst_names) - 2; print(cpt_count) # ~~
            other_comb_names <- 
              c(grep("^Var[0-9]+", colnames(subset_matrix), value = TRUE), 
                names(wst_names)
              )
            other_comb_names <- 
              setdiff(
                other_comb_names,
                ifelse(
                  is_wst_1, 
                  gsub("_[0-9]+$", "", colnames(subset_matrix)[selected_name_pair[[1]]]),
                  colnames(subset_matrix)[[selected_name_pair[[1]]]]
                )
              )
            other_comb_names <- 
              setdiff(
                other_comb_names,
                ifelse(
                  is_wst_2, 
                  gsub("_[0-9]+$", "", colnames(subset_matrix)[selected_name_pair[[2]]]),
                  colnames(subset_matrix)[[selected_name_pair[[2]]]]
                )
              )
          }
          cpt_compares <- matrix(nrow = 2^(cpt_count), ncol = 2)
          num_cpt_reps <- lapply(1:cpt_count, function(num) c(TRUE, FALSE))
          other_combs <- do.call("expand.grid", num_cpt_reps)
          names(other_combs) <- other_comb_names
          if (cpt_count > 0) {
            other_combs <- 
              lapply(
                names(other_combs), 
                function(nm) {
                  if (grepl("wst_[0-9]+$", nm)) {
                    pass <- sapply(
                      other_combs[[nm]],
                      \(lgl) {
                        rep(lgl, times = length(wst_names[[nm]]))
                      }
                    )
                    pass <- as.data.frame(t(pass))
                    names(pass) <- wst_names[[nm]]
                    pass
                  } else {
                    pass <- data.frame(other_combs[[nm]])
                    names(pass) <- nm
                    pass
                  }
                }
              )
          }
          other_combs <- as.data.frame(other_combs); print(other_combs) # ~~
          go <- all(selected_name_pair == 1:2); print(ifelse(go, "go!", "")) # ~~
          for (cpr in 1:nrow(cpt_compares)) {
            select_vec_1 <- subset_0_matrix[[selected_name_pair[[1]]]]
            select_vec_2 <- subset_0_matrix[[selected_name_pair[[2]]]]
            if (cpt_count > 0) {
              select_others <- subset_0_matrix[, names(other_combs)]
              select_others <- 
                apply(
                  as.matrix(select_others), 1, 
                  function(row) all(row == other_combs[cpr,])
                )
            } else {
              select_others <- TRUE
            }
            relevant_subsets <- 
              subset_0_matrix[select_vec_1 & !select_vec_2 & select_others, ]
            if (go) print(relevant_subsets) # ~~
            relevant_values <- 
              value_0_vector[select_vec_1 & !select_vec_2 & select_others]
            if (go) print(relevant_values) # ~~
            if (length(relevant_values) > 1) {
              relevant_perms <- 
                perm_computer_cpt(
                  relevant_subsets, wst_names, 
                  colnames(subset_matrix)[[selected_name_pair[[1]]]])
              if (go) print(relevant_perms) # ~~
              subset_ls_1 <- 
                subset_0_matrix[!select_vec_1 & !select_vec_2 & select_others, ]
              values_ls_1 <- 
                value_0_vector[!select_vec_1 & !select_vec_2 & select_others]
              subset_ls_1[,selected_name_pair[[1]]] <- TRUE
              orig_sort <- 
                do.call("order", as.data.frame(relevant_subsets))
              ls_1_sort <- 
                do.call("order", as.data.frame(subset_ls_1))
              wgts <- exp(relevant_perms[orig_sort] - log(sum(exp(relevant_perms))))
              cpt_compares[cpr, 1] <-
                sum((relevant_values[orig_sort] - values_ls_1[ls_1_sort])*wgts)
            } else {
              cpt_compares[cpr,1] <- 
                relevant_values - 
                value_0_vector[!select_vec_1 & !select_vec_2 & select_others]
            }
            relevant_subsets <- 
              subset_0_matrix[!select_vec_1 & select_vec_2 & select_others, ]
            relevant_values <- 
              value_0_vector[!select_vec_1 & select_vec_2 & select_others]
            if (length(relevant_values) > 1) {
              relevant_perms <- 
                perm_computer_cpt(
                  relevant_subsets, wst_names, 
                  colnames(subset_matrix)[[selected_name_pair[[2]]]])
              subset_ls_1 <- 
                subset_0_matrix[!select_vec_1 & !select_vec_2 & select_others, ]
              values_ls_1 <- 
                value_0_vector[!select_vec_1 & !select_vec_2 & select_others]
              subset_ls_1[,selected_name_pair[[2]]] <- TRUE
              orig_sort <- 
                do.call("order", as.data.frame(relevant_subsets))
              ls_1_sort <- 
                do.call("order", as.data.frame(subset_ls_1))
              wgts <- exp(relevant_perms[orig_sort] - log(sum(exp(relevant_perms))))
              cpt_compares[cpr, 2] <-
                sum((relevant_values[orig_sort] - values_ls_1[ls_1_sort])*wgts)
            } else {
              cpt_compares[cpr,2] <- 
                relevant_values - 
                value_0_vector[!select_vec_1 & !select_vec_2 & select_others]
            }
            # !! here !! ----
            # now two IVs or sets are selected and all 'other_combs' or combos 
            # of other IVs/wsts have been enumerated.
            # will need to select row or within-set rows associated with the 
            # 'other_combs' IVs/wsts as well as the focal one and it's comparator
            # Within-sets must be selected as group and
            # with the weighted averaging conducted within
            # Only in the case that both are in the same wst will the comparison
            # process be the same, or at least similar, as with no wsts. 
            # In the same wst, full enumeration (with all other wsts full in 
            # or out, which should be reflected already in the 'subset_matrix')
            
          }
          print(cpt_compares) # ~~
          first_vs_second <- cpt_compares[, 1] > cpt_compares[, 2]
          second_vs_first <- cpt_compares[, 1] < cpt_compares[, 2]
          complete_dominance[selected_name_pair[[2]], selected_name_pair[[1]]] <-
            mean(second_vs_first)
          complete_dominance[selected_name_pair[[1]], selected_name_pair[[2]]] <-
            mean(first_vs_second)
        }
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
      # !! needs updating with within-sets !! ----
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
# function to compute permutations for inclusion precedence ----
perm_computer <- function(subset_matrix, wst_names, current_name) {
  perm_vec <- vector(mode = "numeric", length = nrow(subset_matrix))
  col <- which(names(subset_matrix) == current_name)
  non_wst <-
    as.matrix(subset_matrix[, which(grepl("^Var[0-9]+", names(subset_matrix)))])
  if (length(non_wst) > 0) {
    colnames(non_wst) <-
      grep("^Var[0-9]+", names(subset_matrix), value = TRUE)
    nonwst_names <-
      as.list(grep("^Var[0-9]+", names(subset_matrix), value = TRUE))
    names(nonwst_names) <-
      grep("^Var[0-9]+", names(subset_matrix), value = TRUE)
    all_wst_names <- append(nonwst_names, wst_names)
  } else {
    all_wst_names <- wst_names
  }
  wst <-
    lapply(
      wst_names,
      function(wst) {
        apply(subset_matrix[, wst], 1, \(row) any(row))
      }
    )
  if (length(wst) > 0) {
    wst <- as.matrix(as.data.frame(wst))
    colnames(wst) <- paste("wst", 1:ncol(wst), sep = "_")
  }
  groups <- cbind(non_wst, wst)
  current_group <-
    ifelse(
      grepl("^wst", current_name),
      gsub("_[0-9]+$", "",  current_name),
      current_name
    )
  current_group_i <- which(colnames(groups) == current_group)
  for (row in 1:nrow(subset_matrix)) {
    if (subset_matrix[row, col]) {
      groups_before <- sum(groups[row, -current_group_i])
      groups_after <- sum(!groups[row, -current_group_i])
      wgrps_before <-
        sapply(
          all_wst_names[groups[row, ]], function(elem) length(elem))
      wgrps_before <-
        wgrps_before[-which(names(wgrps_before) == current_group)]
      wgrps_after <-
        sapply(all_wst_names[!groups[row, ]], function(elem) length(elem))
      wgrp_names <-
        all_wst_names[[which(names(all_wst_names) == current_group)]]
      wgrp_index <- subset_matrix[row, wgrp_names]
      names_before <- sum(wgrp_index) - 1
      names_after <- sum(!wgrp_index)
      perm_vec[[row]] <-
        lfactorial(groups_before) + lfactorial(groups_after) +
        ifelse(
          length(wgrps_before) == 0,
          0,
          sum(sapply(wgrps_before, function(size) lfactorial(size)))
        ) +
        ifelse(
          length(wgrps_after) == 0,
          0,
          sum(sapply(wgrps_after, function(size) lfactorial(size)))
        ) +
        lfactorial(names_before) + lfactorial(names_after)
    }
  }
  for (row in 1:nrow(subset_matrix)) {
    if (!subset_matrix[row, col]) {
      patt2match <- subset_matrix[row, ]
      patt2match[, col] <- TRUE
      loc_of_match <-
        which(apply(subset_matrix, 1, function(patt) all(patt == patt2match)))
      perm_vec[[row]] <- perm_vec[[loc_of_match]]
    }
  }
  # perm_vec <-
  #   perm_vec -
  #   (lfactorial(length(all_wst_names)) +
  #      sum(sapply(all_wst_names, function(elem) lfactorial(length(elem)))))
  perm_vec
}
perm_computer_cpt <- function(subset_matrix, wst_names, current_name) {
  perm_vec <- vector(mode = "numeric", length = nrow(subset_matrix))
  nonwst_names <- grep("^Var[0-9]+", colnames(subset_matrix[]), value = TRUE)
  nonwst_names <- setdiff(nonwst_names, current_name)
  wst_names <- lapply(wst_names, function(wst) setdiff(wst, current_name))
  if (length(nonwst_names) > 0) {
    names(nonwst_names) <- nonwst_names
    all_wst_names <- append(nonwst_names, wst_names)
  } else {
    all_wst_names <- wst_names
  }
  if (grepl("^wst", current_name)) {
    wst_2_remove <- gsub("_[0-9]+$", "", current_name)
    use_wsts <- which(names(all_wst_names) != wst_2_remove)
  } else {
    use_wsts <- 1:length(all_wst_names)
  }
  for (row in 1:nrow(subset_matrix)) {
    grp_in <-
      sapply(
        all_wst_names[use_wsts],
        \(x) {
          all(subset_matrix[row, x])
        }
      )
    grp_out <- 
      sapply(
        all_wst_names[use_wsts],
        \(x) {
          all(!subset_matrix[row, x])
        }
      )
    wst_combs <- 
      sapply(
        all_wst_names,
        \(x) {
          lfactorial(sum(subset_matrix[row,x])) + lfactorial(sum(!subset_matrix[row,x]))
        }
      )
    perm_vec[row] <- lfactorial(sum(grp_in)) + lfactorial(sum(grp_out)) + sum(wst_combs)
    # groups_before <- sum(groups[row, -current_group_i])
    # groups_after <- sum(!groups[row, -current_group_i])
    # wgrps_before <-
    #   sapply(
    #     all_wst_names[groups[row, ]], function(elem) length(elem))
    # wgrps_before <-
    #   wgrps_before[-which(names(wgrps_before) == current_group)]
    # wgrps_after <-
    #   sapply(all_wst_names[!groups[row, ]], function(elem) length(elem))
    # wgrp_names <-
    #   all_wst_names[[which(names(all_wst_names) == current_group)]]
    # wgrp_index <- subset_matrix[row, wgrp_names]
    # names_before <- sum(wgrp_index) - 1
    # names_after <- sum(!wgrp_index)
    # perm_vec[[row]] <-
    #   lfactorial(groups_before) + lfactorial(groups_after) +
    #   ifelse(
    #     length(wgrps_before) == 0,
    #     0,
    #     sum(sapply(wgrps_before, function(size) lfactorial(size)))
    #   ) +
    #   ifelse(
    #     length(wgrps_after) == 0,
    #     0,
    #     sum(sapply(wgrps_after, function(size) lfactorial(size)))
    #   ) +
    #   lfactorial(names_before) + lfactorial(names_after)
  }
  # for (row in 1:nrow(subset_matrix)) {
  #   if (!subset_matrix[row, col]) {
  #     patt2match <- subset_matrix[row, ]
  #     patt2match[, col] <- TRUE
  #     loc_of_match <-
  #       which(apply(subset_matrix, 1, function(patt) all(patt == patt2match)))
  #     perm_vec[[row]] <- perm_vec[[loc_of_match]]
  #   }
  # }
  # perm_vec <-
  #   perm_vec -
  #   (lfactorial(length(all_wst_names)) +
  #      sum(sapply(all_wst_names, function(elem) lfactorial(length(elem)))))
  perm_vec
}

#' TBD
dominance_scalar2 <- 
  function(.obj, .fct, .nms, .val, .adj, .all, 
           .cdl, .cpt, .rev, .cls, .prg, .arg) {
    wsts <- grepl("^wst", names(.nms))
    wsts <- .nms[wsts]
    sets <- grepl("^set", names(.nms))
    sets <- .nms[sets]
    subset_matrix <- subset_matrix_constructor(.nms, sets, wsts)
    subset_matrix <- subset_matrix[-c(1, nrow(subset_matrix)),]
    subsets <- apply(subset_matrix, 1, function(row) names(subset_matrix)[row])
    value_vector <- sapply(subsets, obtain_value2, .obj, .fct, .arg, .prg)
    print(value_vector) # ~~
    print(.nms) # ~~
    
    # ---- ended here 4/27 ----
    conditional_dominance <- 
      compute_conditional_dominance(value_vector, subset_matrix, .nms, .cdl)
    # ---- ended here 4/27 ----
    
  }

#' TBD
subset_matrix_constructor <- function(.nms, .set, .wst) {
  in_out_constructor <- function(name) {c(FALSE, TRUE)}
  in_out_namelist <- 
    lapply(seq_len(length(.nms)), in_out_constructor)
  subset_matrix <- 
    expand.grid(in_out_namelist, KEEP.OUT.ATTRS = FALSE)
  names(subset_matrix) <- names(.nms)
  # ---- sets ----
  if (length(.set) > 0) {
    for (matrix in seq_len(length(.set))) {
      merge_name <- names(.set)[matrix]
      merge_matrix <- 
        data.frame(
          rbind(
            rep(TRUE, times = length(.set[[matrix]]) + 1),
            rep(FALSE, times = length(.set[[matrix]]) + 1)
          )
        )
      names(merge_matrix) <- 
        c(
          merge_name, 
          paste(merge_name, ".Var", seq_len(length(.set[[matrix]])), sep = "")
        )
      subset_matrix <- 
        merge(subset_matrix, merge_matrix, by = merge_name)
      print(subset_matrix) # ~~
    }
    subset_matrix <- 
      subset_matrix[-which(names(subset_matrix) %in% names(.set))]
  }
  # ---- within-sets ----
  if (length(.wst) > 0) {
    in_out_namelist_wst <- 
      lapply(
        seq_len(length(.wst)),
        function(elem) {
          lapply(seq_len(length(.wst[[elem]])), in_out_constructor)
        }
      )
    subset_matrices_wst <- 
      lapply(
        in_out_namelist_wst,
        function(elem) {
          expand.grid(elem, KEEP.OUT.ATTRS = FALSE)
        }
      )
    names(subset_matrices_wst) <- names(.wst)
    for (matrix in seq_len(length(subset_matrices_wst))) {
      merge_name <- names(.wst)[matrix]
      merge_matrix <- 
        data.frame(name = TRUE, subset_matrices_wst[matrix])
      names(merge_matrix)[1] <- merge_name
      subset_matrix <- 
        merge(subset_matrix, merge_matrix, by = merge_name)
    }
    subset_matrix <- 
      subset_matrix[-which(names(subset_matrix) %in% names(.wst))]
  }
  # ---- update names ----
  name_locator <- grep("^var", names(subset_matrix))
  set_locator <- grep("^set", names(subset_matrix))
  wst_locator <- grep("^wst", names(subset_matrix))
  names(subset_matrix)[name_locator] <-
    .nms[grep("^var", names(.nms))]
  names(subset_matrix)[set_locator] <- unlist(.set)
  names(subset_matrix)[wst_locator] <- unlist(.wst)
  # ---- return subsets ----
  subset_matrix
}

#' TBD
obtain_value2 <-
  function(subset, .obj, .fct, .arg, .prg) {
    #if (!is.null(.prg)) utils::setTxtProgressBar(.prg, subset)
    names_in_model_lgl <- (.obj$rhs_names %in% subset)
    .obj$select_lgl[names_in_model_lgl] <- TRUE
    fml <-
      stats::reformulate(
        c(.obj$rhs_names[.obj$select_lgl], .obj$offset),
        response = .obj$lhs_names,
        intercept = .obj$intercept_lgl
      )
    do.call(.fct, append(list(fml), .arg))
  }

#' TBD
compute_conditional_dominance <- 
  function(value_vector, subset_matrix, .nms, .cdl) {
  if (.cdl) {
    num_names <- 
      length(unlist(.nms[grep("^wst", names(.nms))])) + 
      length(.nms[grep("^set|^var", names(.nms))])
    conditional_dominance <- matrix(NA, num_names, num_names)
    print(conditional_dominance) # ~~
    # !! loop over names, sets, and wsets
    # !! determine their weights in 'subset_matrix'
    # !! map 'subset_matrix' to 'value_vector'
    # !! average then plug into dom matrix
    m_vector <- m_determiner(subset_matrix, .nms) # m value from paper
    print(m_vector) # ~~
    for (name in .nms) {
      in_wst <- any(name %in% unlist(.nms[grep("^wst", names(.nms))]))
      if (in_wst) {
        wst_names <- which(sapply(.nms, function(elem) all(name %in% elem)))
        wst_names <- .nms[wst_names]
        # k_vector <- ...
      } else {
        k_vector <- rep(1, times = length(m_mector)) # k value from paper
      }
      print(wst_names) # ~~
    }
  } else {
    return(NULL)
  }
}

#' TBD
m_determiner <- function(subset_matrix, .nms) {
  m_vec <- 
    apply(
      subset_matrix, 1, 
      function(row) {
        val <- 
        sapply(
          .nms, 
          function(elem) any(elem %in% names(subset_matrix)[row])
        )
        sum(val)
      }
    )
}

#' TBD
compute_complete_dominance <- function() {
  
}
