# # # # # # # #
# Run up through line 20 of 01_import.R if picking up here after earlier session
# # # # # # # #

# Inherit analytic_ds from 02_recode or import it locally
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"
dir_ls(temp_wd)

analytic_ds = read_csv(paste0(temp_wd, "/", "2026-06-13_analytic_dataset.csv"))

# Calculate by-school averages by year ----
# group by school and year -- subject and grade go in the model directly

sch_avg_by_year = analytic_ds |>
  group_by(sch_id, year) |>
  mutate(
    sch_male = mean(sex, na.rm = TRUE),
    sch_ell = mean(ell, na.rm = TRUE),
    sch_sp_lang = mean(sp_lang, na.rm = TRUE),
    sch_iep = mean(iep, na.rm = TRUE),
    sch_gt = mean(gt, na.rm = TRUE),
    sch_r_latx = mean(race_latx, na.rm = TRUE),
    sch_r_whit = mean(race_whit, na.rm = TRUE),
    sch_r_afam = mean(race_afam, na.rm = TRUE),
    sch_wida_level = mean(wida_level, na.rm = TRUE),
    sch_wida_yes = mean(wida_yes, na.rm = TRUE),
    sch_cmas_scale = mean(cmas_scale, na.rm = TRUE),
    sch_cmas_level = mean(cmas_level, na.rm = TRUE),
    sch_cmas_yes = mean(cmas_yes, na.rm = TRUE)) |>
  ungroup()

# Center vars around Grand Mean ----

yearly_avg = sch_avg_by_year |>
  # Flag
