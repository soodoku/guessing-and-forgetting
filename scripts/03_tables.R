dir.create("tabs", showWarnings = FALSE)

format_table <- function(data, caption, filename, digits = 3) {
  data |>
    knitr::kable(
      format = "html",
      caption = caption,
      digits = digits,
      escape = TRUE
    ) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = FALSE,
      position = "left"
    ) |>
    kableExtra::save_kable(file.path("tabs", filename), self_contained = TRUE)
}

reliability <- readr::read_csv(
  file.path("output", "reliability.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(Poll = poll, `T1 alpha` = alpha_t1, `T2 alpha` = alpha_t2)
poll_estimates <- readr::read_csv(
  file.path("output", "poll_level.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(
    Poll = poll,
    Respondents = respondents,
    Items = items,
    Raw = raw,
    LCA = lca,
    Standard = standard,
    `Proportion fitting` = proportion_fit
  )
gender_gaps <- readr::read_csv(
  file.path("output", "gender_gaps.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(
    Poll = poll,
    Estimator = estimator,
    `Knowledge gap` = knowledge_gap,
    `Learning gap` = learning_gap
  )
paper_comparison <- readr::read_csv(
  file.path("output", "paper_comparison.csv"),
  show_col_types = FALSE
) |>
  dplyr::rename(
    Metric = metric,
    Paper = paper,
    Deposit = deposit,
    `Current guess` = current_guess,
    `Paper page` = paper_page
  ) |>
  dplyr::select(Metric, Paper, Deposit, `Current guess`, `Paper page`)

format_table(
  reliability,
  "T1 and T2 knowledge-index reliability by poll",
  "reliability.html"
)
format_table(
  poll_estimates,
  "Current guess estimates by poll",
  "poll-estimates.html"
)
format_table(
  gender_gaps,
  "Female-minus-male knowledge and learning gaps",
  "gender-gaps.html"
)
format_table(
  paper_comparison,
  "Paper, Dataverse deposit, and current guess estimates",
  "paper-comparison.html"
)
