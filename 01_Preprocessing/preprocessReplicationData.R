# Inspect process tracing data ------

# Create aggregated data frame ------

df_aggregate <- df[df$event == "btnClick",]

# remove event columns
drops <- c("event", "name", "time")
df_aggregate <- df_aggregate[ , !(names(df_aggregate) %in% drops)]
rm(drops)


### Fixation duration -----

events_fixations <- df$event == "mouseover"
events_fixationends <- df$event == "mouseout"

time_onset_fixation = df$time[events_fixations]
time_offset_fixation = df$time[events_fixationends]

# mark fixation events with their fixation number
fixNum <- cumsum(events_fixations)*events_fixations
idxFix <- which(events_fixations)

# calculate fixation duration
fixation_duration <- time_offset_fixation - time_onset_fixation 


### Number of fixations -----

numFixations <- df %>%
  group_by(subject) %>%
  summarize(nFixations = sum(event == "mouseover"))

numFixationsPerAttribute <- df %>%
  group_by(name, subject, task) %>%
  summarize(nFixations = sum(event == "mouseover"))


### Add number of fixations to aggregated -------

# wide format
numFixationsPerAttribute <- numFixationsPerAttribute %>%
  pivot_wider(names_from = name, values_from = nFixations)

# remove columns
drops <- c("0", "1", "body")
numFixationsPerAttribute <- numFixationsPerAttribute[ , !(names(numFixationsPerAttribute) %in% drops)]
rm(drops)

# replace NA with 0 as they indicate no fixations
numFixationsPerAttribute[is.na(numFixationsPerAttribute)] <- 0

# rename columns
numFixationsPerAttribute <- numFixationsPerAttribute %>% 
  rename(f_price1 = priceEco,
         f_price0 = priceNonEco,
         f_consumption1 = energyEco,
         f_consumption0 = energyNonEco,
         f_popularity1 = popularityEco,
         f_popularity0 = popularityNonEco)

# add to data frame
df_aggregate <- df_aggregate %>%
  left_join(numFixationsPerAttribute, by = c("subject", "task"))

### Calculate total number of fixations ----

# Overall acquisition frequency excluding choice buttons.
tmp <- df_aggregate %>%
  dplyr::select(starts_with('f_')) %>%
  dplyr::mutate(f_total = rowSums(., na.rm = TRUE))
df_aggregate$f_total <- tmp$f_total

# Expand df_aggregate to time_within_trial
expanded_df <- df %>%
  mutate(
    time_within_trial = purrr::map(t_decision,
                                   ~ seq(0, .x, by = 100))
  ) %>%
  unnest(time_within_trial)

# give subjects a new id
expanded_df$id <- NA

expanded_df <- expanded_df %>%
  mutate(id = dense_rank(subject))

# remove columns
drops <- c("subject")
expanded_df <- expanded_df[ , !(names(expanded_df) %in% drops)]
rm(drops)