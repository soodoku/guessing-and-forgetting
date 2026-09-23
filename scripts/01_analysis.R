source("R/functions.R")

verify_sources()
dir.create("output", showWarnings = FALSE)

analyze_poll <- function(file, poll) {
  message("Modern poll: ", file)
  poll_data <- read_poll(file)
  lucky <- lucky_probabilities[[file]]
  tibble::tibble(items = ncol(poll_data$pre), lucky = length(lucky)) |>
    assertr::verify(items == lucky)

  fit <- fit_item_model(poll_data$pre, poll_data$post)
  standard <- guess::stnd_cor(
    poll_data$pre,
    poll_data$post,
    lucky,
    na_as = "dk"
  )$learn
  raw <- poll_data$post |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0L))) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ .x - tidyr::replace_na(poll_data$pre[[dplyr::cur_column()]], 0L)
      )
    ) |>
    colMeans()
  gof <- guess::assess_item_lca_fit(
    fit,
    poll_data$pre,
    poll_data$post,
    na_as = "dk"
  )$statistics

  item_results <- tibble::tibble(
    poll = poll,
    item = names(raw),
    raw = unname(raw),
    lca = unname(fit$learning),
    standard = unname(standard),
    lca_converged = fit$diagnostics$convergence == 0,
    gof_p_value = gof$p_value
  )
  poll_results <- tibble::tibble(
    poll = poll,
    respondents = nrow(poll_data$pre),
    items = ncol(poll_data$pre),
    raw = mean(raw),
    lca = mean(fit$learning),
    standard = mean(standard),
    proportion_fit = mean(gof$p_value >= .05)
  )
  pre_scored <- poll_data$pre |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0L)))
  post_scored <- poll_data$post |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ tidyr::replace_na(.x, 0L)))
  reliability_results <- tibble::tibble(
    poll = poll,
    alpha_t1 = ltm::cronbach.alpha(pre_scored)$alpha,
    alpha_t2 = ltm::cronbach.alpha(post_scored)$alpha
  )

  raw_people <- score_people(poll_data$pre, poll_data$post, lucky)
  lca_people <- score_people(
    poll_data$pre,
    poll_data$post,
    unname(fit$params["gamma", ])
  )
  gender_results <- tibble::tibble(
    poll = poll,
    estimator = c("raw", "standard", "lca"),
    knowledge_gap = c(
      gender_gap(raw_people$knowledge, poll_data$female),
      gender_gap(raw_people$adjusted_knowledge, poll_data$female),
      gender_gap(lca_people$adjusted_knowledge, poll_data$female)
    ),
    learning_gap = c(
      gender_gap(raw_people$learning, poll_data$female),
      gender_gap(raw_people$adjusted_learning, poll_data$female),
      gender_gap(lca_people$adjusted_learning, poll_data$female)
    )
  )

  list(
    item = item_results,
    poll = poll_results,
    gender = gender_results,
    reliability = reliability_results
  )
}

results <- purrr::map2(poll_manifest$file, poll_manifest$poll, analyze_poll)

write_result <- function(component, filename) {
  results |>
    purrr::map(component) |>
    purrr::list_rbind() |>
    readr::write_csv(file.path("output", filename))
}

write_result("item", "item_level.csv")
write_result("poll", "poll_level.csv")
write_result("gender", "gender_gaps.csv")
write_result("reliability", "reliability.csv")
