####
# https://drbtlr.github.io/beating-the-odds/beating-the-odds-code.html
# https://github.com/OpenSDP/faketucky/blob/master/faketucky.rda
####

# Load packages and data ----
# Note: We do not load the `merTools` package because it masks
# the `select` function from `dplyr` (found in `tidyverse`).
library(tidyverse)
library(lme4)
library(glue)
library(modelbased) # https://easystats.github.io/modelbased/articles/plotting.html

# Set custom ggplot2 theme for BTO guide
sdp_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray80", color = "gray80")
    )
}

## Load "Faketucky" data file ----
load("~/R/dps-demo/faketucky.rda")

# Select variables of interest
my_vars <- c("first_dist_code", "first_dist_name",
             "first_hs_code", "first_hs_name",
             "chrt_ninth", "male", "race_ethnicity",
             "frpl_ever_in_hs", "sped_ever_in_hs",
             "lep_ever_in_hs", "gifted_ever_in_hs",
             "scale_score_8_math", "scale_score_8_read",
             "scale_score_11_math", "scale_score_11_read")

faketucky <- faketucky_20160923[my_vars] |> as_tibble()

# Step 1: Prepare data for analysis ----

## Calculate school averages by cohort year ----
sch_avg_by_cohort <- faketucky |>
  mutate(race_white = ifelse(race_ethnicity == "White", 1, 0)) |>
  group_by(first_hs_code, chrt_ninth) |>
  mutate(sch_male = mean(male, na.rm = TRUE),
         sch_white = mean(race_white, na.rm = TRUE),
         sch_frpl = mean(frpl_ever_in_hs, na.rm = TRUE),
         sch_sped = mean(sped_ever_in_hs, na.rm = TRUE),
         sch_lep = mean(lep_ever_in_hs, na.rm = TRUE),
         sch_gifted = mean(gifted_ever_in_hs, na.rm = TRUE),
         sch_8_math = mean(scale_score_8_math, na.rm = TRUE),
         sch_8_read = mean(scale_score_8_read, na.rm = TRUE)) |>
  ungroup()

## Center variables around grand mean ----
# There are other centering techniques available, if this wasn't a demo project it would make sense to test them.

# Calculate across-year cohort averages
cohort_avg <- sch_avg_by_cohort |>
  mutate(flag_2010_cohort = ifelse(chrt_ninth == 2010, 1, 0)) |>
  group_by(chrt_ninth) |>
  mutate(math_8_center = scale_score_8_math - mean(scale_score_8_math, na.rm = TRUE),
         read_8_center = scale_score_8_read - mean(scale_score_8_read, na.rm = TRUE)) |>
  mutate(sch_male_mean_year = mean(sch_male, na.rm = TRUE),
         sch_white_mean_year = mean(sch_white, na.rm = TRUE),
         sch_frpl_mean_year = mean(sch_frpl, na.rm = TRUE),
         sch_sped_mean_year = mean(sch_sped, na.rm = TRUE),
         sch_lep_mean_year = mean(sch_lep, na.rm = TRUE),
         sch_gifted_mean_year = mean(sch_gifted, na.rm = TRUE),
         sch_8_math_mean_year = mean(sch_8_math, na.rm = TRUE),
         sch_8_read_mean_year = mean(sch_8_read, na.rm = TRUE)) |>
  ungroup()

# Center school-level variables: Subtract across-year averages from school averages
sch_avg_center <- cohort_avg |>
  mutate(sch_male_center = sch_male - sch_male_mean_year,
         sch_white_center = sch_white - sch_white_mean_year,
         sch_frpl_center = sch_frpl - sch_frpl_mean_year,
         sch_sped_center = sch_sped - sch_sped_mean_year,
         sch_lep_center = sch_lep - sch_lep_mean_year,
         sch_gifted_center = sch_gifted - sch_gifted_mean_year,
         sch_8_math_center = sch_8_math - sch_8_math_mean_year,
         sch_8_read_center = sch_8_read - sch_8_read_mean_year)

# Step 2: Build and fit the multilevel models ----
# Fit multilevel models for each subject area
m_math <- lmer(
  formula = scale_score_11_math ~
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs +
    lep_ever_in_hs + gifted_ever_in_hs + math_8_center +
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_math_center + flag_2010_cohort + (1|first_hs_code),
  data = sch_avg_center
)

m_read <- lmer(
  formula = scale_score_11_read ~
    male + race_white + frpl_ever_in_hs + sped_ever_in_hs +
    lep_ever_in_hs + gifted_ever_in_hs + read_8_center +
    sch_male_center + sch_white_center + sch_frpl_center +
    sch_sped_center + sch_lep_center + sch_gifted_center +
    sch_8_read_center + flag_2010_cohort + (1|first_hs_code),
  data = sch_avg_center
)

# Step 3: Inspect summary statistics for model fit ----

## Call summary statistics for the math model ----
summary(m_math)

## Plot random effects for the math model ----
merTools::plotREsim(merTools::REsim(m_math, n.sims = 100), stat = "median", sd = TRUE)

# Step 4: Calculate statistics for BTO schools ----
# We used a measure called “expected rank” to identify BTO schools. Expected rank provides the percentile ranks for the observed groups (i.e., schools) in the random effect distribution taking into account both the magnitude and uncertainty of the estimated effect for each group. Incorporating magnitude and uncertainty in the BTO process is a key advantage of using this technique when assessing the performance of schools with small student populations. Estimates for small schools are more uncertain due to having few student observations. A BTO analysis that relies only on confidence intervals and point estimates biases the results towards small schools with uncertain, but very large positive values. Using expected ranks mitigates these biases.

## Calculate school-level expected ranks ----
ranks_math <- merTools::expectedRank(m_math, groupFctr = "first_hs_code")
ranks_read <- merTools::expectedRank(m_read, groupFctr = "first_hs_code")

## Create & execute function to flag BTO schools ----
calc_bto <- function(.ranks, .var) {
  # Input model expected ranks
  .ranks %>%
    # Flag schools that perform above/below benchmark (i.e., BTOs)
    # assume math/read have same cut offs
    mutate(bto = ifelse(pctER >= 70 | pctER < 30, "yes", "no")) %>%
    # Select and name variables of interest
    select(first_hs_code = groupLevel, "estimate_{{ .var }}" := estimate,
           "pctER_{{ .var }}" := pctER, "bto_{{ .var }}" := bto)
}

bto_math <- calc_bto(ranks_math, math)
bto_read <- calc_bto(ranks_read, read)

## Merge datasets for plotting ----
# Merge BTO datasets
bto_read_math <- left_join(bto_read, bto_math, by = "first_hs_code")

# Pull school and district info from original dataset
sch_names <- faketucky %>%
  select(first_dist_code, first_hs_code, first_dist_name, first_hs_name) %>%
  distinct() %>%
  # Convert high school work to factor for merge
  mutate(first_hs_code = factor(first_hs_code))

# Merge BTO and school info datasets
sch_bto_data <- left_join(sch_names, bto_read_math, by = "first_hs_code")

# Step 5 - Plot BTO schools
# Create scatter plot math/read residuals
sch_bto_data %>%
  mutate(bto_type = case_when(
    bto_math == "yes" & bto_read == "yes" ~ "Math AND Reading",
    bto_math == "yes" | bto_read == "yes" ~ "Math OR Reading",
    TRUE ~ "Neither/Not BTO"
  )) %>%
  ggplot(aes(estimate_math, estimate_read, color = bto_type)) +
  geom_point(size = 2, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#E69F00", "#0049E6", "#999999")) +
  sdp_theme() +
  theme(panel.grid = element_line(color = "grey92")) +
  labs(x = "Math Residual", y = "Reading Residual", color = "",
       title = "Schools that Performed Better/Worse that Expected in Math and Reading")

## Working with the merTools package ----

library(merTools)
m1 <- lmer(y ~ service + lectage + studage + (1|d) + (1|s), data=InstEval)
shinyMer(m1, simData = InstEval[1:100, ]) # just try the first 100 rows of data
