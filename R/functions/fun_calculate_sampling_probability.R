
#' Calculate subject-level and group-level sampling probabilities
#'
#' Generalizes the option- and attribute-level sampling probability
#' calculations into a single function. Both use cases share the same
#' nesting structure (id, session, consumption_translation, fixNum) and
#' differ only in which variable is being tabulated (attended_option vs.
#' attended_attribute) and whether a valid_combos filter applies.
#'
#' @param data Data frame (e.g. dfReplicationProcess)
#' @param attended_var Unquoted column name to tabulate (attended_option or
#'   attended_attribute)
#' @param valid_combos Optional data frame for semi_join filtering (used for
#'   the attribute case). NULL skips filtering (option case).
#' @param ci_level Confidence level for the CI (default 0.95)
#'
#' @return A list with two elements:
#'   - subject: subject-level fixation probabilities
#'   - group: aggregated group-level probabilities with CIs
#'   
calculate_sampling_prob <- function(data,
                                    attended_var,
                                    valid_combos = NULL,
                                    ci_level = 0.95) {
  
  attended_var <- enquo(attended_var)
  var_name <- as_name(attended_var)
  
  subj_prob <- data %>%
    count(id, session, consumption_translation, fixNum, !!attended_var, name = "n_fix") %>%
    complete(
      nesting(id, session, consumption_translation, fixNum),
      !!attended_var,
      fill = list(n_fix = 0)
    )
  
  if (!is.null(valid_combos)) {
    subj_prob <- subj_prob %>%
      semi_join(valid_combos, by = c("consumption_translation", var_name, "session"))
  }
  
  subj_prob <- subj_prob %>%
    group_by(id, session, consumption_translation, fixNum) %>%
    mutate(prob_fix = n_fix / sum(n_fix)) %>%
    ungroup()
  
  crit <- 1 - (1 - ci_level) / 2
  
  group_prob <- subj_prob %>%
    group_by(session, consumption_translation, fixNum, !!attended_var) %>%
    summarize(
      mean_prob = mean(prob_fix, na.rm = TRUE),
      sd_prob   = sd(prob_fix, na.rm = TRUE),
      n         = sum(!is.na(prob_fix)),
      .groups = "drop"
    ) %>%
    mutate(
      se_prob  = ifelse(n >= 2, sd_prob / sqrt(n), NA_real_),
      ci_lower = ifelse(n >= 2, mean_prob - qt(crit, df = pmax(n - 1, 1)) * se_prob, NA_real_),
      ci_upper = ifelse(n >= 2, mean_prob + qt(crit, df = pmax(n - 1, 1)) * se_prob, NA_real_)
    )
  
  list(subject = subj_prob, group = group_prob)
}

