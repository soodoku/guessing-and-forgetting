modern_items <- readr::read_csv(
  file.path("output", "item_level.csv"),
  show_col_types = FALSE
)
modern_reliability <- readr::read_csv(
  file.path("output", "reliability.csv"),
  show_col_types = FALSE
)
modern_gaps <- readr::read_csv(
  file.path("output", "gender_gaps.csv"),
  show_col_types = FALSE
)
benchmarks <- readr::read_csv(
  file.path("evidence", "benchmarks.csv"),
  show_col_types = FALSE
)

modern_gender <- modern_gaps |>
  dplyr::group_by(estimator) |>
  dplyr::summarise(
    knowledge_gap = -mean(knowledge_gap),
    learning_gap = -mean(learning_gap),
    .groups = "drop"
  )

current <- c(
  mean(modern_reliability$alpha_t1),
  mean(modern_reliability$alpha_t2),
  sum(modern_reliability$alpha_t2 > modern_reliability$alpha_t1),
  mean(modern_items$raw),
  mean(modern_items$lca),
  mean(modern_items$standard),
  mean(modern_items$lca > modern_items$raw),
  mean(modern_items$standard > modern_items$raw),
  mean(modern_items$gof_p_value >= .05),
  dplyr::filter(modern_gender, estimator == "raw")$knowledge_gap,
  dplyr::filter(modern_gender, estimator == "lca")$knowledge_gap,
  dplyr::filter(modern_gender, estimator == "raw")$learning_gap,
  dplyr::filter(modern_gender, estimator == "lca")$learning_gap
)

comparison <- benchmarks |>
  dplyr::mutate(
    current_guess = current,
    deposit_difference = deposit - paper,
    current_difference = current_guess - paper
  ) |>
  assertr::verify(!is.na(deposit)) |>
  assertr::verify(!is.na(current_guess))

readr::write_csv(comparison, file.path("output", "paper_comparison.csv"))
