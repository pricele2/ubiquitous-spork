# # # # # # # #
# if picking up here after earlier session
# be sure to load packages and temp_wd value
# # # # # # # #

# Double-check packages are loaded
librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats, fs, codebook,
                 # additional pkgs for this stage
                 lme4, lmerTest, ggrepel, modelbased,
                 update_all =  FALSE, ask = TRUE)
# Confirm everything loaded appropriately
print(.Last.value)

# Inherit analytic_ds from 02_recode or import it locally
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"

# # # # # # # # # # # # # # # #

dir_ls(temp_wd)

analytic_ds =
  read_csv(paste0(temp_wd, "/",
                  today(), # toggle today on if running all at once
                  # "2026-06-14",
                  "_analytic_dataset.csv")) |> as_tibble() |>
  # remove the vars that will complicate / collinearity the model
  select(-ends_with("label")) |>
  select(-ends_with("yes")) |>
  select(-ends_with("cmas_level")) # future work, incl by-grade

# Prep data for analysis in lmer4 ----

## Calculate by-school averages by year ----
# group by school -- year and grade go in the model directly
sch_avg_by_year = analytic_ds |>
  mutate(sch_id = as.character(sch_id)) |>
  mutate(grade = as.character(grade)) |>
  group_by(sch_id) |>
  mutate(
    sch_male = mean(sex, na.rm = TRUE),
    sch_ell = mean(ell, na.rm = TRUE),
    sch_sp_lang = mean(sp_lang, na.rm = TRUE),
    sch_iep = mean(iep, na.rm = TRUE),
    sch_gt = mean(gt, na.rm = TRUE),
    sch_r_latx = mean(race_latx, na.rm = TRUE),
    sch_r_whit = mean(race_whit, na.rm = TRUE),
    sch_r_afam = mean(race_afam, na.rm = TRUE),
    # level for WIDA
    sch_wida_level = mean(wida_level, na.rm = TRUE),
    # scale for both -- cut score for LEVEL is different across grades
    sch_ela_cmas_scale = mean(ela_cmas_scale, na.rm = TRUE),
    sch_mat_cmas_scale = mean(mat_cmas_scale, na.rm = TRUE)) |>
  ungroup() |>
  rename(sch_year = year)

colSums(is.na(sch_avg_by_year)) # just three
# wida_level
# ela_cmas_scale
# mat_cmas_scale

## Center vars around Grand Mean ----
# There are other centering techniques available, if this wasn't a demo project it would make sense to explore & test them.

## Calculate cohort averages regardless of grade
cohort_avg = sch_avg_by_year |>
  group_by(sch_year) |>
  # school level vars
mutate(
  sch_male_mean_year = mean(sch_male, na.rm = TRUE),
  sch_ell_mean_year = mean(sch_ell, na.rm = TRUE),
  sch_sp_lang_mean_year = mean(sch_sp_lang, na.rm = TRUE),
  sch_iep_mean_year = mean(sch_iep, na.rm = TRUE),
  sch_gt_mean_year = mean(sch_gt, na.rm = TRUE),
  sch_r_latx_mean_year = mean(sch_r_latx, na.rm = TRUE),
  sch_r_whit_mean_year = mean(sch_r_whit, na.rm = TRUE),
  sch_r_afam_mean_year = mean(sch_r_afam, na.rm = TRUE),
  sch_wida_lev_mean_year = mean(sch_wida_level, na.rm = TRUE)
  ) |>
# handle the missings with COALESCE
  mutate(wida_level = coalesce(wida_level, sch_wida_lev_mean_year)) |>
  ungroup()

colSums(is.na(cohort_avg)) # hell yeah solved

## Center school-level variables: Subtract across-year averages from school averages
sch_avg_center <- cohort_avg |>
  mutate(
    sch_male_center = sch_male - sch_male_mean_year,
    sch_ell_center = sch_ell - sch_ell_mean_year,
    sch_sp_lang_center = sch_sp_lang - sch_sp_lang_mean_year,
    sch_iep_center = sch_iep - sch_iep_mean_year,
    sch_gt_center = sch_gt - sch_gt_mean_year,
    sch_r_latx_center = sch_r_latx - sch_r_latx_mean_year,
    sch_r_whit_center = sch_r_whit - sch_r_whit_mean_year,
    sch_r_afam_center = sch_r_afam - sch_r_afam_mean_year,
    sch_wida_lev_center = sch_wida_level - sch_wida_lev_mean_year)

## Fixed NAs upstream, now handle missing Y vars for model ----
colSums(is.na(sch_avg_center))
colMeans(is.na(sch_avg_center)) * 100

# Build and fit with lme4 ----
# Linear Mixed-Effects Models
# NB - merTools gets the full spelling instead of loading the package since otherwise it masks 'select' from dplyr
# Could have done all this as LOGIT for WHETHER mat_cmas_yes or ela_cmas_yes
# but the ones 'on the bubble' are of especial administrative importance
# Outcome variable ---- POINTS for ELA or MATH CMAS ASSESSMENT

m_math = lmer(
  formula = mat_cmas_scale ~
  # kid level vars
  sex + ell + sp_lang + iep + gt +
  race_latx + race_whit + race_afam +
  wida_level +
  # sch level vars centered - tried ends_with("center"), alas
  sch_male_center + sch_ell_center + sch_sp_lang_center + sch_iep_center +  sch_gt_center +
  sch_r_latx_center + sch_r_whit_center + sch_r_afam_center +
  sch_wida_lev_center +
    # Campus and year and grade
    (1|sch_id) + (1|sch_year) + (1|grade),
  data = sch_avg_center,
  na.action = na.omit)
# 1065 ela_cmas_scale (2.5%), 288 mat_cmas_scale (0.80%)

m_ela = lmer(
  formula = ela_cmas_scale ~
    # kid level vars
    sex + ell + sp_lang + iep + gt +
    race_latx + race_whit + race_afam +
    wida_level +
    # sch level vars centered - tried ends_with("center"), alas
    sch_male_center + sch_ell_center + sch_sp_lang_center + sch_iep_center +  sch_gt_center +
    sch_r_latx_center + sch_r_whit_center + sch_r_afam_center +
    sch_wida_lev_center +
    # Campus and year and grade
    (1|sch_id) + (1|sch_year) + (1|grade),
  data = sch_avg_center,
  na.action = na.omit)

# Look at model results ----

## estimates, sd, and p-values ----
summary(m_math)

# from lmerTest packages
# 1. Cast your existing lme4 model into an lmerTest object
m_model_with_p <- as_lmerModLmerTest(m_math)

# 2. Extract the table (which now includes p-values)
m_coef_summary <- summary(m_model_with_p)$coefficients
print(m_coef_summary)

# 3. Specifically isolate just the p-value column
m_p_values <- m_coef_summary[, c("Estimate", "Std. Error", "Pr(>|t|)")]
print(m_p_values)

# Can also play through it in the Shiny app from merTools pkg
# library(merTools) # # nb - this will mask SELECT inside dplyr
# merTools::plotREsim(merTools::REsim(m_math, n.sims = 100), stat = "median", sd = TRUE)
# merTools::shinyMer(m_math, simData = sch_avg_center[1:150, ]) # first 150 rows
# librarian::check_attached() # make sure merTools isn't still attached

## And again with ELA ----
summary(m_ela)
e_model_with_p <- as_lmerModLmerTest(m_ela)
e_coef_summary <- summary(e_model_with_p)$coefficients
print(e_coef_summary)
e_p_values <- e_coef_summary[, c("Estimate", "Std. Error", "Pr(>|t|)")]
print(e_p_values)

# All of which is interesting but not actually *THE* most interesting part: I want the expected ranks

## Clean up envt before moving on  ----
rm(sch_avg_by_year, sch_avg_center, cohort_avg, analytic_ds)
