#' @title Dominance analysis meta-function that returns scalar
#' @description Internal dominance analysis computation function assuming scalar
#' or vector of length 1 returned value.
#' @param .obj `formula` or `formula_list` processed with `formula_parse()`.
#' @param .fct A `function` or string function name.
#' @param .nms A named list. Groups the results in `.set` and `.wst` together 
#' and is used to determine valid name combinations.
#' @param .val Numeric vector of length 1/scalar. Value associated with all 
#' names included.
#' @param .adj Numeric vector of length 1/scalar. Value associated with no 
#' names included.
#' @param .all Numeric vector of length 1/scalar. Value associated with names 
#' in `.all` included.
#' @param .rev Logical. Reverses interpretation for values that are better 
#' when low.
#' @param .cst Object of class c("SOCKcluster", "cluster") from
#' [`parallel-package`].
#' @param .prg Logical. Shows progress bar.
#' @param .arg List. Additional arguments from `...` to submit to `.fct`.
#' @param .is_fmllst Logical. Changes from `formula` to `formula_list` methods.
#' @return A list of dominance analysis results. To be renamed and used as 
#' `domir`'s list of returned results.
#' @noRd
dominance_scalar <- 
  function(.obj, .fct, .nms, .val, .adj, .all, .rev, .cst, .prg, 
           .arg, .is_fmllst) {
    #print("dominance object") # ~~
    #print(.obj) # ~~
    selector_locations <- 
      as.data.frame(
        matrix(
          Reduce(
            rbind, 
            unlist(format_selector(.obj, .is_fmllst))
          ), ncol = 3, byrow = TRUE, 
          dimnames = list(NULL, c("eq", "elem", "name"))
        )
      )
    selector_locations$eq <- as.numeric(selector_locations$eq)
    selector_locations$elem <- as.numeric(selector_locations$elem)
    #print("selector locations") # ~~
    #print(selector_locations) # ~~
    #print("names that receive value; formatted as list") # ~~
    #print(.nms) # ~~
    wsts <- grepl("^wst", names(.nms))
    wsts <- .nms[wsts]
    sets <- grepl("^set", names(.nms))
    sets <- .nms[sets]
    subset_matrix <- 
      subset_matrix_constructor(.nms, sets, wsts, selector_locations)
    #print("overall subset matrix") # ~~
    #print(subset_matrix) # ~~
    if (.prg) {
      pg_bar <-
        utils::txtProgressBar(min = 0, max = nrow(subset_matrix) - 1, style = 3)
    } else {
      pg_bar <- NULL
    }
    if (.is_fmllst) {
      obtain_value <- obtain_value_fmllst
    } else {
      obtain_value <- obtain_value_fml
    }
    if (is.null(.cst)) {
      value_vector <- 
        sapply(seq_len(nrow(subset_matrix)), obtain_value,
               .obj, .fct, .arg, pg_bar, .val, .adj, .all, selector_locations, 
               subset_matrix)
    } else {
      value_vector <- 
        parallel::parSapply(
          cl = .cst,
          seq_len(nrow(subset_matrix)),
          obtain_value,
          .obj, .fct, .arg, pg_bar, .val, .adj, .all, selector_locations, 
          subset_matrix)
    }
    names(value_vector) <- rownames(subset_matrix)
    #print("R2s") # ~~
    #print(value_vector) # ~~
    conditional_dominance <-
      compute_conditional_dominance(value_vector, subset_matrix, .nms)
    # print("conditional dominance") # ~~
    # print(conditional_dominance) # ~~
    general_dominance <- rowMeans(conditional_dominance)
    # print("general dominance") # ~~
    # print(general_dominance) # ~~
    # print("complete dominance") # ~~
    if (length(wsts) == 0) {
      complete_dominance <-
        compute_complete_dominance(value_vector, subset_matrix, .nms)
    } else {
      complete_dominance <- NULL
    }
    #print(complete_dominance) # ~~
    if (.rev) {
      if (!is.null(complete_dominance)) {
        complete_dominance <- (1 - complete_dominance)
      }
      ranks <- rank(general_dominance, ties.method = "min")
      standard <- 1 - (general_dominance/sum(general_dominance))
    } else {
      ranks <- rank(-general_dominance, ties.method = "min")
      standard <- general_dominance/sum(general_dominance)
    }
    list(
      general = general_dominance,
      conditional = conditional_dominance,
      complete = complete_dominance,
      ranks = ranks,
      standard = standard
    )
  }
#' @title Construct matrix of all valid names
#' @description Internal function to construct a logical matrix where columns 
#' represent names and each row is a different valid combination.
#' @param .nms A named list. Groups the results in `.set` and `.wst` together 
#' and is used to determine valid name combinations.
#' @param .set Subset of `.nms` associated with sets.
#' @param .wst Subset of `.nms` associated with within-group sets.
#' @param .loc The result of `format_selector` applied to all names.
#' @return A logical matrix with column names associated with each name in 
#' `.nms`. 
#' @noRd
subset_matrix_constructor <- function(.nms, .set, .wst, .loc) {
  in_out_constructor <- function(name) {c(FALSE, TRUE)}
  in_out_namelist <- lapply(seq_len(length(.nms)), in_out_constructor)
  subset_matrix <- expand.grid(in_out_namelist, KEEP.OUT.ATTRS = FALSE)
  names(subset_matrix) <- names(.nms)
  # print("initial subset matrix") # ~~
  # print(subset_matrix) # ~~
  if (length(.set) > 0) {
    for (matrix in seq_len(length(.set))) {
      merge_name <- names(.set)[matrix]
      num_names <- sum(sapply(.set[[matrix]], function(elem) length(elem)))
      merge_matrix <- 
        data.frame(
          rbind(
            rep(TRUE, times = num_names + 1),
            rep(FALSE, times = num_names + 1)
          )
        )
      names(merge_matrix) <- 
        c(merge_name, paste(merge_name, ".Var", seq_len(num_names), sep = ""))
      #print(merge_matrix) # ~~
      subset_matrix <- 
        merge(subset_matrix, merge_matrix, by = merge_name)
    }
    subset_matrix <- 
      subset_matrix[-which(names(subset_matrix) %in% names(.set))]
  }
  if (length(.wst) > 0) {
    in_out_namelist_wst <- 
      lapply(
        seq_len(length(.wst)),
        function(elem) lapply(seq_len(length(.wst[[elem]])), in_out_constructor)
      )
    subset_matrices_wst <- 
      lapply(
        in_out_namelist_wst,
        function(elem) expand.grid(elem, KEEP.OUT.ATTRS = FALSE)
      )
    for (wst in seq_len(length(subset_matrices_wst))) {
      names(subset_matrices_wst[[wst]]) <- .wst[[wst]]
    }
    names(subset_matrices_wst) <- names(.wst)
    between_wst_subsets <- lapply(seq_len(length(.wst)), in_out_constructor)
    between_wst_subsets <- 
      expand.grid(between_wst_subsets, KEEP.OUT.ATTRS = FALSE)
    names(between_wst_subsets) <- names(.wst)
    add_subsets <- 
      lapply(
        seq_len(length(subset_matrices_wst)),
        function(matrix) {
          merge_name <- names(.wst)[[matrix]]
          subset_combs <- 
            subset_matrices_wst[[matrix]][
              -c(1,nrow(subset_matrices_wst[[matrix]])),
            ]
          merge_matrix <- 
            data.frame(name = TRUE, subset_combs, check.names = FALSE)
          names(merge_matrix)[1] <- merge_name
          merge_matrix <- 
            merge(between_wst_subsets, merge_matrix, by = merge_name)
          other_wsts <- subset_matrices_wst[-matrix]
          merge_others <- 
            lapply(
              names(other_wsts),
              function(other_wst) {
                other_names <- names(other_wsts[[other_wst]])
                other_df <- as.data.frame(matrix(ncol = length(other_names)))
                names(other_df) <- other_names
                merge_matrix <- 
                  data.frame(merge_matrix, other_df)
                merge_matrix[, other_names] <- merge_matrix[[other_wst]]
                merge_matrix[, other_names]
              }
            )
          if (length(merge_others) > 0) 
            merge_matrix <- cbind(merge_matrix, merge_others)
          merge_matrix
        })
    add_subsets <- do.call("rbind", add_subsets)
    subset_matrix_wst <- merge(subset_matrix, add_subsets)
    wst_subset_merge <- 
      lapply(
        names(subset_matrices_wst),
        function(matrix) {
          cols <- ncol(subset_matrices_wst[[matrix]]) + 1
          temp_mat <- 
            data.frame(matrix(rep(c(FALSE, TRUE), times = cols), ncol = cols))
          names(temp_mat) <- 
            c(matrix, names(subset_matrices_wst[[matrix]]))
          temp_mat
        }
      )
    for (matrix in wst_subset_merge) {
      subset_matrix <- merge(subset_matrix, matrix)
    }
    subset_matrix <- rbind(subset_matrix, subset_matrix_wst)
    wst_locator <- grep("^wst", names(subset_matrix))
    subset_matrix <- subset_matrix[,-wst_locator]
  }
  name_locator <- grep("^var", names(subset_matrix))
  set_locator <- grep("^set", names(subset_matrix))
  names(subset_matrix)[name_locator] <- .nms[grep("^var", names(.nms))]
  names(subset_matrix)[set_locator] <- unlist(.set)
  subset_matrix <- subset_matrix[, .loc$name]
  subset_matrix
}
#' @title Reconstruct formula and submit
#' @description Internal function to reconstruct a `formula` from a 
#' combination of names, submit it to the value generating function, and 
#' collect the returned value.
#' @param subset An integer vector of length 1/scalar.
#' @param .obj `formula` processed with `formula_parse()`.
#' @param .fct A `function` or string function name.
#' @param .arg List. Additional arguments from `...` to submit to `.fct`.
#' @param .prg Object of class "txtProgressBar" from [`utils-package`].
#' @param .val Numeric vector of length 1/scalar. Value associated with all 
#' names included.
#' @param .adj Numeric vector of length 1/scalar. Value associated with no 
#' names included.
#' @param .all Numeric vector of length 1/scalar. Value associated with names 
#' in `.all` included.
#' @param .loc A result of `format_selector` applied to all names.
#' @param .mat A result of `subset_matrix_contructor` applied to all names.
#' @return Numeric vector of length 1/scalar.
#' @noRd
obtain_value_fml <-
  function(subset, .obj, .fct, .arg, .prg, .val, .adj, .all, .loc, .mat) {
    if (!is.null(.prg)) utils::setTxtProgressBar(.prg, subset)
    lgl_vec <- unlist(.mat[subset, ])
    which_change <- which(lgl_vec) 
    select_lgl_change <- .loc$elem[which_change]
    .obj$select_lgl[select_lgl_change] <- TRUE
    if (all(lgl_vec)) {
      return(.val)
    } else if (!any(lgl_vec)) {
      return(.adj + .all)
    } else {
      fml <-
        stats::reformulate(
          c(.obj$rhs_names[.obj$select_lgl], .obj$offset),
          response = .obj$lhs_names,
          intercept = .obj$intercept_lgl
        )
      return(do.call(.fct, append(list(fml), .arg)))
    }
  }
#' @title Reconstruct formula_list and submit
#' @description Internal function to reconstruct a `formula_list` from a 
#' combination of names, submit it to the value generating function, and 
#' collect the returned value.
#' @param subset An integer vector of length 1/scalar.
#' @param .obj `formula_list` processed with `formula_parse()`.
#' @param .fct A `function` or string function name.
#' @param .arg List. Additional arguments from `...` to submit to `.fct`.
#' @param .prg Object of class "txtProgressBar" from [`utils-package`].
#' @param .val Numeric vector of length 1/scalar. Value associated with all 
#' names included.
#' @param .adj Numeric vector of length 1/scalar. Value associated with no 
#' names included.
#' @param .all Numeric vector of length 1/scalar. Value associated with names 
#' in `.all` included.
#' @param .loc A result of `format_selector` applied to all names.
#' @param .mat A result of `subset_matrix_contructor` applied to all names.
#' A logical matrix with column names.
#' @return Numeric vector of length 1/scalar.
#' @noRd
obtain_value_fmllst <-
  function(subset, .obj, .fct, .arg, .prg, .val, .adj, .all, .loc, .mat) {
    if (!is.null(.prg)) utils::setTxtProgressBar(.prg, subset)
    lgl_vec <- unlist(.mat[subset, , drop = TRUE])
    which_change <- which(lgl_vec)
    chosen_selectors <- .loc[which_change, ]
    for (chg in seq_len(nrow(chosen_selectors))) {
      .obj[[chosen_selectors$eq[[chg]]]]$select_lgl[[
        chosen_selectors$elem[[chg]]
      ]] <- TRUE
    }
    if (all(lgl_vec)) {
      return(.val)
    } else if (!any(lgl_vec)) {
      return(.adj + .all)
    } else {
      fmllst <-
        lapply(
          .obj,
          function(elem) {
            varlist <- elem$rhs_names[elem$select_lgl]
            if (length(varlist) == 0) varlist <- "1"
            stats::reformulate(
              c(varlist, elem$offset),
              response = elem$lhs_names,
              intercept = elem$intercept_lgl
            )
          }
        )
      fmllst <- do.call("formula_list", fmllst)
      return(do.call(.fct, append(list(fmllst), .arg)))
    }
  }
#' @title Conditional dominance computation
#' @description Internal function to compute conditional dominance values for 
#' each name at each valid  inclusion precedence position.
#' @param value_vector A numeric vector.
#' @param subset_matrix A result of `subset_matrix_contructor` applied to 
#' all names. A logical matrix with column names.
#' @param .nms A named list. Groups the results in `.set` and `.wst` together 
#' and is used to determine valid name combinations.
#' @return A numeric matrix. Names are associated with rows. Inclusion 
#' precedence positions are associated with columns.
#' @noRd
compute_conditional_dominance <- 
  function(value_vector, subset_matrix, .nms) {
    num_names <- 
      length(unlist(.nms[grep("^wst", names(.nms))])) + 
      length(.nms[grep("^set|^var", names(.nms))])
    conditional_dominance <- matrix(NA, nrow = num_names, ncol = length(.nms))
    m_vector_inclusive <- name_counter(subset_matrix, .nms, inclusive = TRUE)
    m_vector_exclusive <- name_counter(subset_matrix, .nms, inclusive = FALSE)
    name_loc <- 1
    for (name in .nms) {
      in_wst <- all(name %in% unlist(.nms[grep("^wst", names(.nms))]))
      namelist_locs <- which(sapply(.nms, function(elem) all(name %in% elem)))
      namelist <- .nms[namelist_locs]
      if (in_wst) namelist <- unlist(namelist)
      k_vectors <- 
        lapply(
          namelist,
          function(elem) {
            return_vec <- 
              name_counter(subset_matrix, namelist, inclusive = TRUE)
            condit_vec <- 
              (m_vector_inclusive == m_vector_exclusive) | 
              (m_vector_inclusive > 0 & return_vec > 0 & 
                 m_vector_inclusive != m_vector_exclusive) &
                 (return_vec != length(namelist))
            select_vec <- apply(subset_matrix[elem], 1, all)
            return_vec <-return_vec*condit_vec*select_vec
            return_vec
          }
        )
      processed <- 
        lapply(
          k_vectors,
          function(elem) {
            return_list <- 
              list(
                subset_matrix_for_name = subset_matrix[elem > 0, ],
                value_vector_for_name = value_vector[elem > 0],
                k_vector_for_name = elem[elem > 0]
              )
            if (in_wst) {
              add_list <- list(m_vector_for_name = m_vector_inclusive[elem > 0])
            } else {
              add_list <- list(m_vector_for_name = m_vector_exclusive[elem > 0])
            }
            append(return_list, add_list)
          }
        )
      for (var in seq_len(length(processed))) { # by row
        for (inc_seq in seq_len(length(.nms))) { # by column
          select_at_inc <- processed[[var]][["m_vector_for_name"]] == inc_seq
          k_vector_for_name_at_inc_seq <- 
            processed[[var]][["k_vector_for_name"]]
          k_vector_for_name_at_inc_seq <- 
            k_vector_for_name_at_inc_seq[select_at_inc]
          value_vector_for_name_at_inc_seq <- 
            processed[[var]][["value_vector_for_name"]]
          value_vector_for_name_at_inc_seq <- 
            value_vector_for_name_at_inc_seq[select_at_inc]
          weight_vector <- 
            (lfactorial(inc_seq - 1) + lfactorial(length(.nms) - inc_seq) + 
            lfactorial(k_vector_for_name_at_inc_seq - 1) + 
            lfactorial(length(namelist) - k_vector_for_name_at_inc_seq)) - 
            (lfactorial(length(.nms) - 1) + lfactorial(length(namelist)))
          value_with_name <- value_vector_for_name_at_inc_seq*exp(weight_vector)
          subset_matrix_for_name_at_inc_seq <- 
            processed[[var]][["subset_matrix_for_name"]]
          subset_matrix_for_name_at_inc_seq <- 
            subset_matrix_for_name_at_inc_seq[select_at_inc, ]
          increment_rows <- 
            find_increments(
              subset_matrix, 
              subset_matrix_for_name_at_inc_seq, 
              namelist[[var]]
            )
          increment_with_name <- value_vector[increment_rows]*exp(weight_vector)
          if (var > 1 & inc_seq == 1) name_loc <- name_loc + 1
          conditional_dominance[name_loc, inc_seq] <-
            sum(value_with_name - increment_with_name)
        }
      }
      name_loc <- name_loc + 1
    }
    conditional_dominance
}
#' @title Counts valid combinations by inclusion precedence position
#' @description Internal function to count the number of combinations of names
#' that are inclusive of any focal names (i.e., inclusive) or inclusive of all 
#' focal names (i.e., exclusive) for conditional dominance computations.
#' @param subset_matrix A result of `subset_matrix_contructor` applied 
#' to all names. A logical matrix with column names.
#' @param .nms A named list. Groups the results in `.set` and `.wst` together 
#' and is used to determine valid name combinations.
#' @param inclusive Logical. Inclusive or exclusive result.
#' @return Integer vector. One value per row of `subset_matrix`.
#' @noRd
name_counter <- function(subset_matrix, .nms, inclusive) {
  apply(
    subset_matrix, 1, 
    function(row) {
      val <- 
        sapply(
          .nms, 
          if (inclusive) {
            function(elem) any(elem %in% names(subset_matrix)[row])
          } else {
            function(elem) all(elem %in% names(subset_matrix)[row])
          }
        )
      sum(val)
    }
  )
}
#' @title Computes increment to value given name inclusion
#' @description Internal function to find the location of subsets in 
#' `subset_matrix` that contain a combination of names that can be used to 
#' compute an increment beyond a combination of names with a focal namelist.
#' @param subset_matrix A result of `subset_matrix_contructor` applied 
#' to all names. A logical matrix with column names.
#' @param subset_matrix_for_name A logical matrix. Subset of `subset_matrix` 
#' that identifies combinations at a specific inclusion precedence position 
#' where the `namelist` is included.
#' @param namelist A character vector of names. Focal names to identify 
#' increments.
#' @return Integer matrix with two rows. Top row corresponds to `subset_matrix`
#' row index number of combination with `namelist` included. Bottowm row 
#' corresponds to `subset_matrix` index number of combination with `namelist` 
#' excluded. Each column is a distinct valid increment. 
#' @noRd
find_increments <- function(subset_matrix, subset_matrix_for_name, namelist) {
  subset_matrix_for_name_delta <- subset_matrix_for_name
  subset_matrix_for_name_delta[namelist] <- FALSE
  rows_delta <- 
    apply(
      subset_matrix_for_name_delta,
      1,
      function(row1) {
        which(apply(
          subset_matrix,
          1, 
          function(row2) {
            all(row1 == row2)
          }
        ))
      }
    )
  rows_delta
}
#' @title Constructs map of equation, name, and selector location
#' @description Internal function to associate equation or left hand side names 
#' with term or right hand side names and locations in the selector list.
#' @param .obj `formula` or `formula_list` processed with `formula_parse()`.
#' @param .is_fmllst Logical.
#' @return A list with the following elements:
#' \describe{
#' \item{eq}{List index number for the equation or left hand side 
#' of a `formula_list`.}
#' \item{elem}{{List index number for the term or right hand side 
#' of a `formula_list`.}
#' \item{nm}{Character valued name.}
#' }
#' @noRd
format_selector <- function(.obj, .is_fmllst) {
  if (!.is_fmllst) {
    list_loc <-
      lapply(
        which(!.obj$select_lgl),
        function(el) {
          nm <- .obj$rhs_names[[el]]
          list(eq = 1, elem = el, name = nm)
        }
      )
  } else {
    list_loc <-
      lapply(
        seq_len(length(.obj)),
        function(elem) {
          lapply(
            which(!.obj[[elem]]$select_lgl),
            function(el) {
              nm <- 
                paste(
                  .obj[[elem]]$lhs_names, "~", 
                  .obj[[elem]]$rhs_names[[el]], sep = "")
              list(eq = elem, elem = el, name = nm)
            }
          )
        }
      )
  }
  list_loc
}
#' @title Complete dominance computation
#' @description Internal function to compute complete dominance proportions for 
#' each name pair.
#' @param value_vector A numeric vector.
#' @param subset_matrix A result of `subset_matrix_contructor` applied to 
#' all names. A logical matrix with column names.
#' @param .nms A named list. Groups the results in `.set` and `.wst` together 
#' and is used to determine valid name combinations.
#' @return A square numeric matrix. One name is associated with each column 
#' (dominant name). One name is associaated with each row (non-dominant name).
#' @noRd
compute_complete_dominance <- 
  function(value_vector, subset_matrix, .nms) {
  name_count <- length(.nms)
  complete_dominance <- matrix(nrow = name_count, ncol = name_count)
  all_name_pairs <- utils::combn(1:name_count, 2)
  for (name_pair in seq_len(ncol(all_name_pairs))) {
    selected_name_pair_locs <- 
      all_name_pairs[, name_pair]
    selected_name_pair <- 
      intersect(unlist(.nms[selected_name_pair_locs]), names(subset_matrix))
    unselected_names <- setdiff(names(subset_matrix), selected_name_pair)
    selected_names_matrix <-
      cbind(subset_matrix, seq_len(nrow(subset_matrix)))
    one_names <- unlist(.nms[[selected_name_pair_locs[[1]]]])
    subsets_only_one <- 
      rowSums(subset_matrix[, one_names, drop = FALSE]) == length(one_names)
    two_names <- unlist(.nms[[selected_name_pair_locs[[2]]]])
    subsets_only_two <- 
      rowSums(subset_matrix[, two_names, drop = FALSE]) == length(two_names)
    selected_names_matrix <- 
      selected_names_matrix[xor(subsets_only_one, subsets_only_two), ]
    sorting_matrix <- 
      selected_names_matrix[, c(unselected_names, selected_name_pair)]
    sorting_df <- as.data.frame(sorting_matrix)
    selected_names_sorted <- 
      selected_names_matrix[do.call("order", sorting_df), ]
    first_name_locs <- (seq_len(nrow(selected_names_sorted)) %% 2) == 0
    first_name_index <-
      selected_names_sorted[first_name_locs, ncol(selected_names_sorted)]
    first_name_values <- value_vector[first_name_index]
    second_name_locs <- (seq_len(nrow(selected_names_sorted)) %% 2) == 1
    second_name_index <-
      selected_names_sorted[second_name_locs, ncol(selected_names_sorted)]
    second_name_values <- value_vector[second_name_index]
    sorted_results_pair <- cbind(first_name_values, second_name_values)
    first_vs_second <- sorted_results_pair[, 1] > sorted_results_pair[, 2]
    second_vs_first <- sorted_results_pair[, 1] < sorted_results_pair[, 2]
    complete_dominance[
      selected_name_pair_locs[[2]], selected_name_pair_locs[[1]]
      ] <- mean(second_vs_first)
    complete_dominance[
      selected_name_pair_locs[[1]], selected_name_pair_locs[[2]]
      ] <- mean(first_vs_second)
  }
  complete_dominance
}
