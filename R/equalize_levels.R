#' Equalize paired item response levels
#'
#' `equalize_levels()` prepares paired Time 1 and Time 2 PROM items for
#' longitudinal CFA by ensuring that each item has the same observed response
#' levels at both time-points. Sparse or mismatched response categories are
#' collapsed consistently within each item pair.
#'
#' Item pairs can be detected either by column position or by suffix patterns.
#'
#' @param data A data frame containing paired item variables. Do not include
#'   the transition rating variable.
#' @param pair_by Pairing method. `"position"` assumes the first half of columns
#'   are Time 1 items and the second half are Time 2 items. `"suffix"` detects
#'   item pairs using `t1_suffix` and `t2_suffix`.
#' @param t1_suffix Regex suffix identifying Time 1 items when
#'   `pair_by = "suffix"`. Use `""` when Time 1 items have no suffix.
#' @param t2_suffix Regex suffix identifying Time 2 items when
#'   `pair_by = "suffix"`.
#' @param min_resp Integer. Minimum number of responses required in each
#'   response category at both time-points. Categories with fewer responses
#'   are collapsed.
#' @param shift_to_one Logical. If `TRUE`, globally shifts item scores so that
#'   the minimum observed item response is 1 before collapsing.
#' @param shift_back Logical. If `TRUE`, reverses the global shift after
#'   collapsing. Only relevant when `shift_to_one = TRUE`.
#' @param verbose Logical. If `TRUE`, prints progress messages.
#' @param print_tables Logical. If `TRUE`, prints before/after frequency tables
#'   only for item pairs that required collapsing or equalization.
#'
#' @return A list with components:
#' \describe{
#'   \item{data}{The processed data frame, ordered as T1 items followed by T2 items.}
#'   \item{shift}{The global shift applied to item responses.}
#'   \item{pair_map}{A data frame describing matched T1/T2 item pairs.}
#'   \item{collapsed_items}{A data frame listing item pairs that required collapsing/equalization.}
#'   \item{before_tables}{Frequency tables before collapsing for affected item pairs.}
#'   \item{after_tables}{Frequency tables after collapsing for affected item pairs.}
#'   \item{mappings}{Category mappings for affected item pairs.}
#'   \item{summary}{A data frame summarizing all item pairs.}
#' }
#'
#' @examples
#' dat <- data.frame(
#'   item1 = c(1, 1, 2, 2, 3, 3, 3, 4),
#'   item2 = c(1, 2, 2, 3, 3, 3, 4, 4),
#'   item1.1 = c(1, 2, 2, 2, 3, 4, 4, 4),
#'   item2.1 = c(1, 1, 2, 2, 3, 4, 4, 4)
#' )
#'
#' out <- equalize_levels(
#'   data = dat,
#'   pair_by = "suffix",
#'   t1_suffix = "",
#'   t2_suffix = "\\.1",
#'   min_resp = 2,
#'   shift_to_one = FALSE,
#'   verbose = FALSE,
#'   print_tables = FALSE
#' )
#' @export
equalize_levels <- function(
    data,
    pair_by = c("position", "suffix"),
    t1_suffix = NULL,
    t2_suffix = NULL,
    min_resp = 5L,
    shift_to_one = TRUE,
    shift_back = FALSE,
    verbose = TRUE,
    print_tables = TRUE
) {

  pair_by <- match.arg(pair_by)

  # ---------------------------------------------------------------------------
  # Checks
  # ---------------------------------------------------------------------------

  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (ncol(data) == 0L) {
    stop("`data` has no columns.", call. = FALSE)
  }

  non_numeric <- names(data)[!vapply(data, is.numeric, logical(1))]

  if (length(non_numeric) > 0L) {
    stop(
      "All item columns must be numeric. Non-numeric columns: ",
      paste(non_numeric, collapse = ", "),
      call. = FALSE
    )
  }

  if (!is.numeric(min_resp) || length(min_resp) != 1L || is.na(min_resp)) {
    stop("`min_resp` must be a single non-missing numeric value.", call. = FALSE)
  }

  min_resp <- as.integer(min_resp)

  if (min_resp < 1L) {
    stop("`min_resp` must be >= 1.", call. = FALSE)
  }

  all_values_check <- unlist(data, use.names = FALSE)
  all_values_check <- all_values_check[!is.na(all_values_check)]

  if (length(all_values_check) == 0L) {
    stop("No non-missing item responses found.", call. = FALSE)
  }

  if (!all(is.finite(all_values_check))) {
    stop("Item responses must be finite numeric values or NA.", call. = FALSE)
  }

  # ---------------------------------------------------------------------------
  # Helper: convert supplied suffix to end-anchored regex
  # ---------------------------------------------------------------------------

  suffix_pattern <- function(x) {

    if (is.null(x)) {
      return(NULL)
    }

    if (identical(x, "")) {
      return("")
    }

    if (grepl("\\$$", x)) {
      x
    } else {
      paste0(x, "$")
    }
  }

  # ---------------------------------------------------------------------------
  # Identify item pairs
  # ---------------------------------------------------------------------------

  if (pair_by == "position") {

    if (ncol(data) %% 2L != 0L) {
      stop(
        "`data` must contain an even number of columns when `pair_by = 'position'`: ",
        "Time 1 items followed by Time 2 items.",
        call. = FALSE
      )
    }

    nitems <- ncol(data) / 2L

    pair_map <- data.frame(
      item_id = seq_len(nitems),
      base_name = names(data)[seq_len(nitems)],
      t1_item = names(data)[seq_len(nitems)],
      t2_item = names(data)[nitems + seq_len(nitems)],
      stringsAsFactors = FALSE
    )

  } else {

    if (is.null(t1_suffix) || is.null(t2_suffix)) {
      stop(
        "`t1_suffix` and `t2_suffix` must be supplied when `pair_by = 'suffix'`.",
        call. = FALSE
      )
    }

    t1_pat <- suffix_pattern(t1_suffix)
    t2_pat <- suffix_pattern(t2_suffix)

    nm <- names(data)

    if (identical(t1_pat, "")) {

      # Example:
      # Time 1: item1
      # Time 2: item1.1
      #
      # User call:
      # t1_suffix = ""
      # t2_suffix = "\\.1"

      t2_items <- nm[grepl(t2_pat, nm)]
      t2_base  <- sub(t2_pat, "", t2_items)

      if (length(t2_items) == 0L) {
        stop("No Time 2 items found using `t2_suffix`.", call. = FALSE)
      }

      # Time 1 candidates are all columns that are not identified as T2.
      t1_items <- setdiff(nm, t2_items)

      if (length(t1_items) == 0L) {
        stop("No Time 1 items found.", call. = FALSE)
      }

      # For every T1 item, a corresponding T2 item must exist.
      missing_t2 <- setdiff(t1_items, t2_base)

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      # For every T2 item, a corresponding T1 item must exist.
      missing_t1 <- setdiff(t2_base, t1_items)

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      # Preserve Time 1 column order
      t2_items_ordered <- t2_items[match(t1_items, t2_base)]

      pair_map <- data.frame(
        base_name = t1_items,
        t1_item = t1_items,
        t2_item = t2_items_ordered,
        stringsAsFactors = FALSE
      )

    } else {

      # Example:
      # Time 1: item1.t1
      # Time 2: item1.t2
      #
      # User call:
      # t1_suffix = "\\.t1"
      # t2_suffix = "\\.t2"

      t1_items <- nm[grepl(t1_pat, nm)]
      t2_items <- nm[grepl(t2_pat, nm)]

      if (length(t1_items) == 0L) {
        stop("No Time 1 items found using `t1_suffix`.", call. = FALSE)
      }

      if (length(t2_items) == 0L) {
        stop("No Time 2 items found using `t2_suffix`.", call. = FALSE)
      }

      t1_base <- sub(t1_pat, "", t1_items)
      t2_base <- sub(t2_pat, "", t2_items)

      if (anyDuplicated(t1_base)) {
        stop("Duplicate Time 1 base names detected.", call. = FALSE)
      }

      if (anyDuplicated(t2_base)) {
        stop("Duplicate Time 2 base names detected.", call. = FALSE)
      }

      # For every T1 item, a corresponding T2 item must exist.
      missing_t2 <- setdiff(t1_base, t2_base)

      if (length(missing_t2) > 0L) {
        stop(
          "Time 2 items missing for Time 1 base names: ",
          paste(missing_t2, collapse = ", "),
          call. = FALSE
        )
      }

      # For every T2 item, a corresponding T1 item must exist.
      missing_t1 <- setdiff(t2_base, t1_base)

      if (length(missing_t1) > 0L) {
        stop(
          "Time 1 items missing for Time 2 base names: ",
          paste(missing_t1, collapse = ", "),
          call. = FALSE
        )
      }

      # Preserve Time 1 column order
      t2_items_ordered <- t2_items[match(t1_base, t2_base)]

      pair_map <- data.frame(
        base_name = t1_base,
        t1_item = t1_items,
        t2_item = t2_items_ordered,
        stringsAsFactors = FALSE
      )
    }

    if (nrow(pair_map) == 0L) {
      stop("No matched item pairs found.", call. = FALSE)
    }

    pair_map$item_id <- seq_len(nrow(pair_map))
    pair_map <- pair_map[, c("item_id", "base_name", "t1_item", "t2_item")]
  }

  nitems <- nrow(pair_map)

  # Internal data always arranged as:
  # T1 items followed by corresponding T2 items
  out <- data[, c(pair_map$t1_item, pair_map$t2_item), drop = FALSE]

  # ---------------------------------------------------------------------------
  # Optional global shift so minimum observed item response = 1
  # ---------------------------------------------------------------------------

  global_shift <- 0

  if (isTRUE(shift_to_one)) {

    all_values <- unlist(out, use.names = FALSE)
    all_values <- all_values[!is.na(all_values)]

    global_min <- min(all_values)
    global_shift <- 1 - global_min

    if (global_shift != 0) {
      out[] <- lapply(out, function(x) x + global_shift)
    }
  }

  # ---------------------------------------------------------------------------
  # Helper functions
  # ---------------------------------------------------------------------------

  present <- function(v) {
    sort(unique(v[!is.na(v)]))
  }

  print_level_tables <- function(table_list, title) {

    cat("\n================ ", title, " ================\n", sep = "")

    for (i in seq_along(table_list)) {

      cat("\nItem ", i, "\n\n", sep = "")

      print(table_list[[i]]$t1)
      cat("\n")
      print(table_list[[i]]$t2)
      cat("\n")
    }
  }

  needs_collapse <- function(t1, t2) {

    p1 <- present(t1)
    p2 <- present(t2)
    union_cats <- sort(unique(c(p1, p2)))

    if (length(union_cats) < 2L) {
      return(TRUE)
    }

    count_t1 <- function(cat) sum(t1 == cat, na.rm = TRUE)
    count_t2 <- function(cat) sum(t2 == cat, na.rm = TRUE)

    sparse <- any(vapply(
      union_cats,
      function(cat) count_t1(cat) < min_resp || count_t2(cat) < min_resp,
      logical(1)
    ))

    mismatch <- !identical(p1, p2)

    sparse || mismatch
  }

  collapse_pair <- function(t1, t2) {

    count_t1 <- function(cat) {
      sum(t1 == cat, na.rm = TRUE)
    }

    count_t2 <- function(cat) {
      sum(t2 == cat, na.rm = TRUE)
    }

    either_sparse <- function(cat) {
      count_t1(cat) < min_resp || count_t2(cat) < min_resp
    }

    original_categories <- sort(unique(c(present(t1), present(t2))))

    if (length(original_categories) < 2L) {
      stop(
        "An item pair has fewer than two observed categories before collapsing.",
        call. = FALSE
      )
    }

    repeat {

      p1 <- present(t1)
      p2 <- present(t2)

      union_cats <- sort(unique(c(p1, p2)))

      if (length(union_cats) <= 1L) {
        break
      }

      mn <- min(union_cats)
      mx <- max(union_cats)
      center <- (mn + mx) / 2

      choose_target <- function(cat) {

        lower <- union_cats[union_cats < cat]
        upper <- union_cats[union_cats > cat]

        down <- if (length(lower)) max(lower) else NA_real_
        up   <- if (length(upper)) min(upper) else NA_real_

        # Lower response options are merged upward
        if (cat < center) {
          return(if (!is.na(up)) up else down)
        }

        # Higher response options are merged downward
        if (cat > center) {
          return(if (!is.na(down)) down else up)
        }

        # Middle category: merge with the larger neighboring category
        candidates <- c(down, up)
        candidates <- candidates[!is.na(candidates)]

        if (!length(candidates)) {
          return(NA_real_)
        }

        totals <- vapply(
          candidates,
          function(cc) count_t1(cc) + count_t2(cc),
          numeric(1)
        )

        candidates[which.max(totals)]
      }

      # ---- A. sparse lower tail ----
      if (either_sparse(mn)) {
        target <- choose_target(mn)

        if (!is.na(target)) {
          t1[t1 == mn] <- target
          t2[t2 == mn] <- target
          next
        }
      }

      # ---- B. sparse upper tail ----
      if (either_sparse(mx)) {
        target <- choose_target(mx)

        if (!is.na(target)) {
          t1[t1 == mx] <- target
          t2[t2 == mx] <- target
          next
        }
      }

      # ---- C. categories present at one time-point only ----
      only_t1 <- setdiff(p1, p2)
      only_t2 <- setdiff(p2, p1)

      mismatched <- sort(unique(c(only_t1, only_t2)))

      if (length(mismatched) > 0L) {

        cat_mis <- if (min(mismatched) < center) {
          min(mismatched)
        } else {
          max(mismatched)
        }

        target <- choose_target(cat_mis)

        if (!is.na(target)) {
          t1[t1 == cat_mis] <- target
          t2[t2 == cat_mis] <- target
          next
        }
      }

      # ---- D. internal sparsity ----
      sparse <- union_cats[vapply(union_cats, either_sparse, logical(1))]

      if (length(sparse) > 0L) {

        cat_sp <- if (min(sparse) < center) {
          min(sparse)
        } else {
          max(sparse)
        }

        target <- choose_target(cat_sp)

        if (!is.na(target)) {
          t1[t1 == cat_sp] <- target
          t2[t2 == cat_sp] <- target
          next
        }
      }

      break
    }

    collapsed_categories <- sort(unique(c(present(t1), present(t2))))

    if (length(collapsed_categories) < 2L) {
      stop(
        "An item pair collapsed to fewer than two categories. ",
        "Consider lowering `min_resp` or removing the item.",
        call. = FALSE
      )
    }

    mapping <- data.frame(
      collapsed_category = collapsed_categories,
      final_category = collapsed_categories
    )

    list(
      t1 = t1,
      t2 = t2,
      original_categories = original_categories,
      collapsed_categories = collapsed_categories,
      mapping = mapping
    )
  }

  # ---------------------------------------------------------------------------
  # Identify item pairs that require collapsing/equalization
  # ---------------------------------------------------------------------------

  require_collapse <- logical(nitems)

  for (i in seq_len(nitems)) {

    t1_col <- i
    t2_col <- nitems + i

    require_collapse[i] <- needs_collapse(
      t1 = out[[t1_col]],
      t2 = out[[t2_col]]
    )
  }

  collapsed_items <- pair_map[require_collapse, , drop = FALSE]

  if (nrow(collapsed_items) == 0L) {
    message("No item pairs required collapsing or level equalization.")
  }

  # ---------------------------------------------------------------------------
  # Collect before-collapse tables only for affected items
  # ---------------------------------------------------------------------------

  before_tables <- vector("list", sum(require_collapse))
  after_tables  <- vector("list", sum(require_collapse))
  mappings      <- vector("list", sum(require_collapse))

  if (sum(require_collapse) > 0L) {

    affected_idx <- which(require_collapse)

    names(before_tables) <- pair_map$base_name[affected_idx]
    names(after_tables)  <- pair_map$base_name[affected_idx]
    names(mappings)      <- pair_map$base_name[affected_idx]

    for (k in seq_along(affected_idx)) {

      i <- affected_idx[k]

      t1_col <- i
      t2_col <- nitems + i

      before_tables[[k]] <- list(
        t1 = table(out[[t1_col]], useNA = "ifany"),
        t2 = table(out[[t2_col]], useNA = "ifany")
      )
    }

    if (isTRUE(print_tables)) {
      print_level_tables(before_tables, "BEFORE COLLAPSE")
    }
  }

  # ---------------------------------------------------------------------------
  # Collapse / equalize affected item pairs only
  # ---------------------------------------------------------------------------

  summary_list <- vector("list", nitems)
  map_counter <- 0L

  for (i in seq_len(nitems)) {

    t1_col <- i
    t2_col <- nitems + i

    t1_name <- names(out)[t1_col]
    t2_name <- names(out)[t2_col]

    original_levels <- sort(unique(c(
      present(out[[t1_col]]),
      present(out[[t2_col]])
    )))

    if (isTRUE(require_collapse[i])) {

      if (isTRUE(verbose)) {
        message(
          "Equalizing item pair ",
          i,
          " / ",
          nitems,
          ": ",
          t1_name,
          " + ",
          t2_name
        )
      }

      res <- collapse_pair(
        t1 = out[[t1_col]],
        t2 = out[[t2_col]]
      )

      out[[t1_col]] <- res$t1
      out[[t2_col]] <- res$t2

      map_counter <- map_counter + 1L

      after_tables[[map_counter]] <- list(
        t1 = table(out[[t1_col]], useNA = "ifany"),
        t2 = table(out[[t2_col]], useNA = "ifany")
      )

      mappings[[map_counter]] <- res$mapping

      original_levels <- res$original_categories
    }

    final_t1_levels <- present(out[[t1_col]])
    final_t2_levels <- present(out[[t2_col]])

    if (!identical(final_t1_levels, final_t2_levels)) {
      stop(
        "Internal error: final levels are not equal for item pair `",
        t1_name,
        "` and `",
        t2_name,
        "`.",
        call. = FALSE
      )
    }

    summary_list[[i]] <- data.frame(
      item_pair = i,
      base_name = pair_map$base_name[i],
      t1_item = t1_name,
      t2_item = t2_name,
      required_collapse = require_collapse[i],
      n_original_levels = length(original_levels),
      n_final_levels = length(final_t1_levels),
      original_levels = paste(original_levels, collapse = ", "),
      final_levels = paste(final_t1_levels, collapse = ", "),
      stringsAsFactors = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Optional shift back
  # ---------------------------------------------------------------------------

  if (isTRUE(shift_back) && global_shift != 0) {
    out[] <- lapply(out, function(x) x - global_shift)
  }

  # If shift_back is used, refresh after-tables and summary final levels
  if (isTRUE(shift_back) && global_shift != 0) {

    if (sum(require_collapse) > 0L) {

      affected_idx <- which(require_collapse)

      for (k in seq_along(affected_idx)) {

        i <- affected_idx[k]

        t1_col <- i
        t2_col <- nitems + i

        after_tables[[k]] <- list(
          t1 = table(out[[t1_col]], useNA = "ifany"),
          t2 = table(out[[t2_col]], useNA = "ifany")
        )
      }
    }

    for (i in seq_len(nitems)) {

      t1_col <- i
      t2_col <- nitems + i

      final_t1_levels <- present(out[[t1_col]])

      summary_list[[i]]$final_levels <- paste(final_t1_levels, collapse = ", ")
    }
  }

  if (sum(require_collapse) > 0L && isTRUE(print_tables)) {
    print_level_tables(after_tables, "AFTER COLLAPSE")
  }

  summary_df <- do.call(rbind, summary_list)

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------------------

  list(
    data = out,
    shift = global_shift,
    pair_map = pair_map,
    collapsed_items = collapsed_items,
    before_tables = before_tables,
    after_tables = after_tables,
    mappings = mappings,
    summary = summary_df
  )
}

