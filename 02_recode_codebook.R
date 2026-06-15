# Recoding! for modeling at campus level ----
# Run first 20 lines of 01_import to inherit orig_local, or import it from local
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"

dir_ls(temp_wd) # see line 20 of 01_import.R

orig_local = read_csv("2026-06-10_orig_df.csv") |> as_tibble()
sch_names = read_csv("2026-06-10_sch_crosswalk.csv") |> as_tibble()

# Double-check packages are loaded
librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats, fs, codebook,
update_all =  FALSE, ask = TRUE)

# Confirm everything loaded appropriately
print(.Last.value)

## what are we starting with? then make a plan
colnames(orig_local)

# Make a copy to hold the edits in progress but reorder vars ----
temp_orig = orig_local |>
  rename(wida_label = access) |>
  rename(sp_lang = test) |>
  rename(cmas_scale = score) |>
  rename(cmas_level = level) |>
  rename(cmas_label = lev_label) |>
  select(year, sch_id, grade, stu_id, sex, race, ell, iep, gt, sp_lang, subject, wida_label, starts_with("cmas") )

# Now for the many mutates and recodings ----

working_df = temp_orig |>
  # year : character to factor
  mutate(year =
    recode_values(year,
    "2022-2023" ~ "22-23",
    "2023-2024" ~ "23-24",
    "2024-2025" ~ "24-25",
    "2025-2026" ~ "25-26")) |>
    mutate(year = as.factor(year)) |>
  # sch_id - numeric to factor
  mutate(sch_id = as.factor(sch_id)) |>
  # grade - num to char
  mutate(grade = as.character(grade)) |>
  # stu_id - num to char
  mutate(stu_id = as.character(stu_id)) |>
  # sex : M is most frequent per prop.table()
  mutate(sex =
    recode_values(sex,
    "M" ~ "1",
    "F" ~ "0",
    "N" ~ "0")) |>
    mutate(sex = as.numeric(sex)) |>
  # race var -- annoying but necessary to calculate school-level pcts
  # I'll do it at the end by reshaping wide and rejoining
  mutate(race =
    recode_values(race,
    "Native American" ~ "AIAN",
    "Black" ~ "AFAM",
    "Asian" ~ "ASAM",
    "Hispanic" ~ "LATX",
    "Native Hawaiian/PI" ~ "NHPI",
    "Two+ Races" ~ "TWO+",
    "White" ~ "WHIT")) |>
  # ELL var -- recoding NA as zero
  mutate(ell = recode_values(ell, "Y" ~ "1", "N" ~ "0", NA ~ "0")) |>
    mutate(ell = as.numeric(ell))  |>
  # IEP var
  mutate(iep = recode_values(iep,  "Y" ~ "1", "N" ~ "0")) |>
    mutate(iep = as.numeric(iep)) |>
  # GT var
  mutate(gt = recode_values(gt, "GT" ~ "1", "Non-GT" ~ "0")) |>
    mutate(gt = as.numeric(gt)) |>
  # language arts testing - Eng or Span
  mutate(sp_lang =
    recode_values(sp_lang, "CMAS SLA" ~ "1", "CMAS ELA" ~ "0", "CMAS Math" ~ "0")) |>
    mutate(sp_lang = as.numeric(sp_lang)) |>
  # subject needs no modifications
  # WIDA label needs the actual level for use in model
  mutate(wida_level = case_when(
    wida_label == "Entering" ~ "1",
    wida_label == "Emerging" ~ "2",
    wida_label == "Developing" ~ "3",
    wida_label == "Expanding" ~ "4",
    wida_label == "Bridging" ~ "5",
    wida_label == "Reaching" ~ "6",
    TRUE ~ NA)) |>
    mutate(wida_level = as.numeric(wida_level)) |>
  # CMAS label here is aNNOYing, let's make it shorter
  mutate(cmas_label =
    replace_values(cmas_label,
    "Did not yet meet expectations" ~ "Not Yet Met",
    "Partially Met Expectations" ~ "Partially",
    "Approached Expectations" ~ "Approached",
    "Met Expectations" ~ "Met",
    "Exceeded Expectations" ~ "Exceeded")) |>
  # cmas_scale scores for Met+Exceeded -- 750 or up, for every grade (whew!)
  # "Overall scale scores range from 650 to 850."
  mutate(cmas_yes = case_when(
    cmas_scale >= 750 ~ 1,
    cmas_scale < 750 ~ 0,
    TRUE ~ NA)) |>
  mutate(wida_yes = case_when(
    wida_level >= 4 ~ 1,
    wida_level < 4 ~ 0,
    TRUE ~ NA)) |>
  select(year, sch_id, grade, stu_id, sex, race, ell, iep, gt, sp_lang, subject, starts_with("wida"), starts_with("cmas"))

colSums(is.na(working_df))
# So far just WIDA related vars

## Convert RACEvar to dummies ----
work2 = working_df |> select(stu_id, race) |> rename(value = race) |>
  mutate(dummy = as.character("1")) |> unique()

work3 = pivot_wider(work2, names_from = value, names_prefix = "race_") |>
  mutate(stu_id = as.numeric(stu_id)) |>
  select(-dummy) |>
  mutate(across(where(is.character), ~ifelse(!is.na(.), "1",.))) |>
  mutate(across(everything(), ~as.numeric(.))) |> # !! it worked!
  mutate(across(where(is.numeric), ~replace_na(., 0))) |> # FIXED BAYBEEEEEE
  mutate(stu_id = as.character(stu_id)) |>
  clean_names()

work4 = left_join(working_df, work3, by = "stu_id") |>
  select(year, sch_id, grade, subject, stu_id, sex, ell, sp_lang, iep, gt, starts_with("race"), starts_with("wida"), starts_with("cmas")) |>
  mutate(sch_id = as.numeric(sch_id))

# Add the school names ! ----

work5 = sch_names |> select(sch_id, sch_name, sch_category)

work6 = left_join(work4, work5, by = "sch_id") |>
  select(year, sch_id, sch_name, sch_category, grade, subject, stu_id, sex, ell, sp_lang, iep, gt, starts_with("race"), starts_with("wida"), starts_with("cmas"))

# Do the scores need to be reshaped? Y
# 18791 unique kids, not much mobility either within-year or between
# test0 = work6 |>
#   mutate(stu_id = as.numeric(stu_id)) |>
#   group_by(stu_id, year) |>
#   summarise(n_schools = n_distinct(sch_id)) |>
#   ungroup()

work7 = work6 |> filter(subject == "ELA") |>
  rename(ela_cmas_scale = cmas_scale,
         ela_cmas_level = cmas_level,
         ela_cmas_label = cmas_label,
         ela_cmas_yes = cmas_yes) |>
  select(year, sch_id, stu_id, starts_with("ela_"))

work8 = work6 |> filter(subject == "Math") |>
  rename(mat_cmas_scale = cmas_scale,
         mat_cmas_level = cmas_level,
         mat_cmas_label = cmas_label,
         mat_cmas_yes = cmas_yes) |>
  select(year, sch_id, stu_id, starts_with("mat_"))

work9 = work6 |> select(-c(subject, race, starts_with("cmas"))) |> unique()

colSums(is.na(work9)) # great, still just WIDA related vars
dim(work9) # 68193 down to 36121

# Merge it all back together
temp01 = left_join(work9, work7, by = c("year", "sch_id", "stu_id"))
  colnames(temp01)
temp02 = left_join(temp01, work8, by = c("year", "sch_id", "stu_id"))
  colnames(temp02)

analytic_ds = temp02 |> unique()

## Spit out the flat file for later as RDA and CSV
write_excel_csv(analytic_ds,
  paste0(temp_wd, "/", today(), "_analytic_dataset.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")

write_rds(analytic_ds, paste0(temp_wd, "/", today(), "_analytic_RDS.rds"))
dir_ls(temp_wd) # confirm

##  Clean up the intermittent dataframes
rm(list = ls(pattern = "^work"))
rm(list = ls(pattern = "^temp"))

# Will need to break into MATH and ELA models, eventually
codebook(analytic_ds,
         detailed_variables = TRUE, # mandatory images annoying
         detailed_scales = FALSE,
         missingness_report = FALSE,
         metadata_table = FALSE,
         metadata_json = FALSE,
         exclude_from_detailed_display = c("stu_id", "sch_id", "sch_name"),
         indent = "#"
)
# All seems well! on to build the model
