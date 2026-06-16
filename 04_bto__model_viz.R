# # # # # # # #
# Script 4 "Beat the Odds" prep, analysis and dataviz
# # # # # # # #

# Double-check packages are loaded
librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats, fs, codebook,
                 # additional pkgs for this stage
                 lme4, lmerTest, ggrepel, modelbased, tidyplots,
                 update_all =  FALSE, ask = TRUE)
# Confirm everything loaded appropriately
print(.Last.value)
# Set local export WD if it isn't already loaded
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"

## Borrow SDP ggplot2 theme for BTO guide
sdp_theme <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 16, face = "bold"),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 8),
      legend.position = "top",
      legend.text = element_text(size = 8),
      strip.text = element_text(size = 12),
      strip.background = element_rect(fill = "gray80", color = "gray80")
    )
}

# Grab WD and sch_names again
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"
sch_names = read_csv("2026-06-10_sch_crosswalk.csv") |> as_tibble() |>
  mutate(sch_id = as.character(sch_id)) |>
  select(starts_with("sch"))

# # # # # # # # # # # # # # # # END FRONTMATTER

# Prep data for analysis in lmer4 ----
# Inherit from 02_recode or import it locally
analytic_ds =
  read_csv(paste0(temp_wd, "/",
                  # today(), # toggle today on if running all at once
                  "2026-06-14",
                  "_analytic_dataset.csv")) |> as_tibble() |>
  # remove vars that will almost certainly complicate model w collinearity
  select(-ends_with("label")) |>
  select(-ends_with("yes")) |>
  select(-ends_with("cmas_level")) # future work, incl by-grade

## Calculate by-school averages by year ----
# group by school + year + grade
sch_avg_by_year = analytic_ds |>
  mutate(sch_id = as.character(sch_id)) |>
  mutate(grade = as.character(grade)) |>
  group_by(sch_id, year, grade) |>
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

colSums(is.na(sch_avg_by_year)) # just three vars
# wida_level
# ela_cmas_scale
# mat_cmas_scale

## Center vars around Grand Mean ----
# There are other centering techniques available, if this wasn't a demo project it would make sense to explore & test them.

## Calculate across-grade averages regardless of grade
cohort_avg = sch_avg_by_year |>
  group_by(sch_year, grade) |>
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
  # handle the missings in WIDA with COALESCE
  mutate(wida_level = coalesce(wida_level, sch_wida_lev_mean_year)) |>
  ungroup()

colSums(is.na(cohort_avg)) # hell yeah solved missing WIDA var

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
    sch_wida_lev_center = sch_wida_level - sch_wida_lev_mean_year) |>
  rename(male = sex)

## Fixed NAs upstream, now check on missing Y vars for model ----
colSums(is.na(sch_avg_center))
colMeans(is.na(sch_avg_center)) * 100
# 1065 ela_cmas_scale (2.5%), 288 mat_cmas_scale (0.80%)
# this is fine

# Build and fit with lme4 ----
# Linear Mixed-Effects Models
# NB - loading merTools by library masks 'select' from dplyr
# Could have done LOGIT for WHETHER mat_cmas_yes or ela_cmas_yes
# but the ones 'on the bubble' are of especial administrative importance
# Outcome variable ---- POINTS for ELA or MATH CMAS ASSESSMENT

m_math = lmer(
  formula = mat_cmas_scale ~
    # kid level vars
    male + ell + sp_lang + iep + gt +
    race_latx + race_whit + race_afam +
    wida_level +
    # sch level vars centered
    sch_male_center + sch_ell_center + sch_sp_lang_center + sch_iep_center +  sch_gt_center +
    sch_r_latx_center + sch_r_whit_center + sch_r_afam_center +
    sch_wida_lev_center +
    # Campus and year and grade
    (1|sch_id) + (1|sch_year) + (1|grade),
  data = sch_avg_center,
  na.action = na.omit)

summary(m_math)

# Same model for ELA with different Y var
m_ela = lmer(
  formula = ela_cmas_scale ~
    # kid level vars
    male + ell + sp_lang + iep + gt +
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

# try it in base R
m_p_vals = data.frame(Row_Names = rownames(m_p_values), m_p_values, row.names = NULL) |> rename("Variable" = 1, "Std. Error" = 3, "Pr(>|t|)" = 4)

# Write to Google Sheet the first time
# sheet_write(m_p_vals,
#             ss = "https://docs.google.com/spreadsheets/d/1TYbfKqOkfkUJajM-_0b2OYroUIKSBuFhfxuS0-ptf3s/edit?usp=sharing",
#             sheet = "math-model")

## And again with ELA ----
summary(m_ela)
e_model_with_p <- as_lmerModLmerTest(m_ela)
e_coef_summary <- summary(e_model_with_p)$coefficients
# print(e_coef_summary)
e_p_values <- e_coef_summary[, c("Estimate", "Std. Error", "Pr(>|t|)")]
print(e_p_values)

# try it in base R
e_p_vals = data.frame(Row_Names = rownames(e_p_values), e_p_values, row.names = NULL) |> rename("Variable" = 1, "Std. Error" = 3, "Pr(>|t|)" = 4)

# Write to Google Sheet the first time
# sheet_write(e_p_vals,
#             ss = "https://docs.google.com/spreadsheets/d/1TYbfKqOkfkUJajM-_0b2OYroUIKSBuFhfxuS0-ptf3s/edit?usp=sharing",
#             sheet = "ela-model")

# All of which is interesting but not actually *THE* most interesting part: I want the expected ranks

## Clean up envt before moving on  ----
rm(sch_avg_by_year, sch_avg_center, cohort_avg, analytic_ds)

# Expected Rank ----
# NOTES FROM STRATEGIC DATA PROJECT: We used a measure called “expected rank” to identify BTO schools. Expected rank provides the percentile ranks for the observed groups (i.e., schools) in the random effect distribution taking into account both the magnitude and uncertainty of the estimated effect for each group. Incorporating magnitude and uncertainty in the BTO process is a key advantage of using this technique when assessing the performance of schools with small student populations. Estimates for small schools are more uncertain due to having few student observations. A BTO analysis that relies only on confidence intervals and point estimates biases the results towards small schools with uncertain, but very large positive values. Using expected ranks mitigates these biases.

# Calculate school-level ranks expected from this model  ----
ranks_math <- merTools::expectedRank(m_math, groupFctr = "sch_id")

ranks_ela <- merTools::expectedRank(m_ela, groupFctr = "sch_id")

## Create & execute function to flag BTO schools ----
# pctER is "percentile, expected rank"

calc_bto <- function(.ranks, .var) {
  # Input model expected ranks
  .ranks |>
    # Flag schools that perform above/below benchmark
    mutate(bto =
      case_when(
        pctER >= 70 ~ "A-Hi",
        pctER <= 30 ~ "C-Lo",
        TRUE ~ "B-Mid"))  |>
    mutate(bto = factor(bto, levels = c("A-Hi", "B-Mid", "C-Lo"))) |>
    # Select and name variables of interest
    select(sch_id = groupLevel, "estimate_{{ .var }}" := estimate,
           "pctER_{{ .var }}" := pctER, "bto_{{ .var }}" := bto)
}

# Run the function
bto_math = calc_bto(ranks_math, math)
bto_ela = calc_bto(ranks_ela, ela)

## Merge datasets for plotting ----
bto_both = left_join(bto_ela, bto_math, by = "sch_id")

sch_bto_data = left_join(bto_both, sch_names, by = "sch_id") |>
  mutate(
    bto_type =
      case_when(
        bto_math == "A-Hi" & bto_ela == "A-Hi" ~ "Both Hi",
        bto_math == "C-Lo" & bto_ela == "C-Lo" ~ "Both Lo",
        TRUE ~ NA)) |>
  select(starts_with("sch_"), bto_type, ends_with("_math"), ends_with("_ela")) |>
  mutate(bto_type = factor(bto_type,
    levels = c("Both Hi", "Both Lo", "As Modeled")))

table(sch_bto_data$bto_ela, sch_bto_data$bto_math, exclude = NULL)

# Export to use as pivot table
sheet_write(sch_bto_data,
            ss = "https://docs.google.com/spreadsheets/d/1TYbfKqOkfkUJajM-_0b2OYroUIKSBuFhfxuS0-ptf3s/edit?usp=sharing",
            sheet = "LME-expected-hi-mid-lo")

# Scatterplot the win/lose schools ----

#ff664f orangey-red
#51bc84 minty green
#6ad2e9 tiffany blue
#ffe636 accent yellow
#fdbad9 blossom pink
#feffef neutral background
#7570b3 stronger purple
# "Both Hi", "Math Only", "ELA Only", "Both Lo", "Both Mid"

sch_bto_data |>
  ggplot(aes(estimate_math, estimate_ela, color = bto_type)) +
  # facet_grid(~ bto_type) +
  geom_point(size = 2, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#6ad2e9", "#ff664f", "#999999")) +
  sdp_theme() +
  theme(panel.grid = element_line(color = "grey92")) +
  ggrepel::geom_text_repel(
    # data = filter(sch_bto_data, outlier_neg == TRUE | outlier_pos == TRUE),
    aes(label = sch_name),
    box.padding = 0.5,      # Distance between label and point
    point.padding = 0.3,    # Distance between text edge and point
    segment.color = "grey50" # Color of the line connecting text to point
  ) +
  labs(x = "Math Scaled Residual",
       y = "ELA Scaled Residual",
       # title = "Campuses by Performance Category"
       title = "Twenty Campuses CMAS Performance vs. ELA & Math Models",
       subtitle = "Six beat the model on both subjects, and four underperform on both"
      ) +
  theme(legend.position = "bottom")

ggsave(
      "g_both_scatter.png",
      # "g_bto-type_panel.png",
       plot = get_last_plot(),
       device = "png",
       path = "C:/Users/Lauren/Documents/Search/DPS-Presentation/",
       width = 10,
       height = 4.5,
       units = c("in"),
       dpi = 300)


