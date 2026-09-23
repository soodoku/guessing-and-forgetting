project_file <- function(...) {
  file.path(rprojroot::find_root(rprojroot::has_file("DESCRIPTION")), ...)
}

poll_manifest <- tibble::tibble(
  file = c(
    "aus", "btp04", "btp04GE", "btp05", "btp07", "bul", "ca", "cpl",
    "dk", "eu2007", "eu2009", "ire", "mi", "nic1", "sm", "swp",
    "ukbge", "ukcrime", "ukeu", "ukhealth", "ukmon", "vt", "wtu"
  ),
  poll = c(
    "Australia Constitutional Referendum", "BTP 2004 Primaries",
    "BTP 2004 General Election", "BTP 2005", "BTP 2007", "Bulgaria",
    "California Referendum", "CPL", "Denmark", "EU 2007", "EU 2009",
    "Northern Ireland", "Michigan", "NIC", "San Mateo", "SWEPCO",
    "UK BGE", "UK Crime", "UK EU", "UK Health", "UK Monarchy",
    "Vermont", "WTU"
  )
)

lucky_probabilities <- list(
  aus = c(.25, .25, .25, .50, .25, .50, .20, .20, .333, .333),
  btp04 = c(.25, .25, .25, .25, .33, .25, .33),
  btp04GE = c(.33, .25, .50, .50, .50, .25, .25, .25, .25),
  btp05 = c(.25, .33, .33, .20, .25, .25),
  btp07 = rep(.25, 8),
  bul = rep(.50, 7),
  ca = c(.33, .33, .25, .25, .25),
  cpl = c(.167, .333, .333, .333, .25, .143, .20),
  dk = c(.333, .333, .25, .25, .25, .25, .50, .50, .50),
  eu2007 = c(.25, .25, .25, .25, .25, .20, .20, .20, .25, .40, .40),
  eu2009 = rep(.25, 6),
  ire = c(.25, .20, .25, .25, .25, .333, .25),
  mi = c(.50, .50, .25, .25, .25, .571, .571, .571, .571),
  nic1 = c(.50, .50, .50, .50, .25, .25, .429, .429),
  sm = c(.20, .20, .20, .20, .20, .20, .25, .20),
  swp = c(.167, .333, .333, .333, .143),
  ukbge = c(.50, .50, .50, rep(3 / 7, 12)),
  ukcrime = rep(.50, 7),
  ukeu = rep(.50, 5),
  ukhealth = rep(.50, 6),
  ukmon = rep(.50, 8),
  vt = c(.20, .20, .25, .25, .25, .25, .20, .25, .25),
  wtu = c(.167, .333, .333, .333, .142)
)

verify_sources <- function() {
  manifest <- readr::read_csv(
    project_file("data", "manifest.csv"),
    show_col_types = FALSE
  )
  hashes <- manifest |>
    dplyr::mutate(
      observed_md5 = unname(tools::md5sum(project_file(manifest$path)))
    )
  assertr::verify(
    hashes,
    all(hashes$md5 == hashes$observed_md5),
    error_fun = assertr::error_stop
  )
  invisible(TRUE)
}

read_poll <- function(file) {
  data <- readr::read_csv(
    project_file("data", paste0(file, ".csv")),
    show_col_types = FALSE
  )
  data |>
    assertr::verify(names(data)[[ncol(data)]] == "female") |>
    assertr::verify((ncol(data) - 1L) %% 2L == 0L)
  n_items <- (ncol(data) - 1L) / 2L
  item_names <- paste0("item", seq_len(n_items))
  pre <- data |>
    dplyr::select(dplyr::all_of(seq_len(n_items))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
  post <- data |>
    dplyr::select(dplyr::all_of(n_items + seq_len(n_items))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
  names(pre) <- names(post) <- item_names
  valid_binary <- function(value) is.na(value) | value %in% c(0L, 1L)
  pre |> assertr::assert(valid_binary, dplyr::everything())
  post |> assertr::assert(valid_binary, dplyr::everything())
  list(data = data, pre = pre, post = post, female = data$female)
}

fit_item_model <- function(pre, post) {
  starts <- list(
    NULL,
    c(gg = .2, gk = .2, gd = .1, kk = .1, dg = .1, dk = .1, dd = .2, gamma = .2),
    c(gg = .3, gk = .1, gd = .1, kk = .1, dg = .1, dk = .1, dd = .2, gamma = .25)
  )
  attempt <- function(start) {
    tryCatch(
      guess::fit_item_lca(pre, post, na_as = "dk", start = start),
      error = function(error) NULL
    )
  }
  fit <- starts |>
    purrr::map(attempt) |>
    purrr::compact() |>
    purrr::pluck(1, .default = NULL)
  if (is.null(fit)) {
    stop("The current guess estimator failed for every documented starting value.")
  }
  fit
}

score_people <- function(pre, post, guessing_probability) {
  adjusted <- guess::group_adj(
    pre,
    post,
    guessing_probability,
    knowledge_given_dont_know = 0,
    na_as = "dk"
  )$adjusted_responses
  raw_pre <- rowMeans(replace(pre, is.na(pre), 0))
  raw_post <- rowMeans(replace(post, is.na(post), 0))
  tibble::tibble(
    knowledge = raw_pre,
    learning = raw_post - raw_pre,
    adjusted_knowledge = rowMeans(adjusted$pre_test),
    adjusted_learning = rowMeans(adjusted$post_test - adjusted$pre_test)
  )
}

gender_gap <- function(score, female) {
  gender_data <- tibble::tibble(score = score, female = female)
  gender_data <- dplyr::filter(gender_data, !is.na(gender_data$female))
  stats::coef(stats::lm(score ~ female, data = gender_data))[[2L]]
}
