
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
                                    fix_var = fixNum,
                                    valid_combos = NULL,
                                    ci_level = 0.95) {
  
  attended_var <- enquo(attended_var)
  var_name <- as_name(attended_var)
  
  fix_var <- enquo(fix_var)
  fix_var_name <- as_name(fix_var)
  
  # Work around tidyr::nesting()'s unreliable handling of injected (!!) quosures
  # by renaming the fixation-index column to a fixed literal name up front,
  # then renaming it back to its original name at the end.
  data <- data %>%
    rename(.fix_idx = !!fix_var)
  
  subj_prob <- data %>%
    count(id, session, consumption_translation, .fix_idx, !!attended_var, name = "n_fix") %>%
    complete(
      nesting(id, session, consumption_translation, .fix_idx),
      !!attended_var,
      fill = list(n_fix = 0)
    )
  
  if (!is.null(valid_combos)) {
    subj_prob <- subj_prob %>%
      semi_join(valid_combos, by = c("consumption_translation", var_name, "session"))
  }
  
  subj_prob <- subj_prob %>%
    group_by(id, session, consumption_translation, .fix_idx) %>%
    mutate(prob_fix = n_fix / sum(n_fix)) %>%
    ungroup()
  
  crit <- 1 - (1 - ci_level) / 2
  
  group_prob <- subj_prob %>%
    group_by(session, consumption_translation, .fix_idx, !!attended_var) %>%
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
  
  # rename the fixation-index column back to whatever was passed as fix_var
  subj_prob <- subj_prob %>% rename(!!fix_var_name := .fix_idx)
  group_prob <- group_prob %>% rename(!!fix_var_name := .fix_idx)
  
  
  list(subject = subj_prob, group = group_prob)
}

