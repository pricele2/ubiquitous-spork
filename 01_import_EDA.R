# # # # #
# LPRICE5
# Demonstration project for Manager, Reporting & Analytics
# # # # #

# Define packages to load, then bring in using Librarian ----
# NB: run update_all the first time or w any new packages added
library(librarian)
stock(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, forcats, fs, codebook,
      # update_all =  TRUE,
      ask = TRUE)

librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats, fs, codebook,
    update_all =  FALSE, ask = TRUE)

# Confirm everything loaded appropriately
print(.Last.value)

# Define local WD for exports ----
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"

# Read in flat files from Google Sheets ----
# NB: Requires initial authorization in browser (after: choose option 2)

## Student level data ----
# First time from gsheets
{
gs4_auth()

tic()
orig_df = read_sheet(
  "https://docs.google.com/spreadsheets/d/1TcTHO673AmR1zOWkgqduxcj8_0PFASdnwb2z_877TZ8/edit?",
  sheet = "Sheet1",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  .name_repair = "unique") |>
  as_tibble() |>
  clean_names() |>
  rename(year = 1, test = 2, subject = 3, stu_id = 4, cmas_scale = 5, perf_level = 6, perf_label = 7, sch_id = 8, grade = 9, sex = 10, race = 11, ell = 12, swd = 13, g_t = 14, wida_level = 15)

Sys.sleep(1) # pause for google api if retrying
toc() ## this takes crazy long! export to local for future utility

### Export stu-level data the first time ----
write_excel_csv(orig_df,
    paste0(temp_wd, "/", today(), "_orig_df.csv"),
    na = "",
    append = FALSE,
    col_names = TRUE,
    quote = "needed",
    escape = "none",  eol = "\r\n")
}

## School names crosswalk ----
# First time from gsheets
{tic()
sch_names = read_sheet(
  "https://docs.google.com/spreadsheets/d/1TYbfKqOkfkUJajM-_0b2OYroUIKSBuFhfxuS0-ptf3s/edit?usp=sharing",
  range = "sch-name-id!A6:L26",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  .name_repair = "unique") |>
  as_tibble() |>
  clean_names() |>
  select(-c(starts_with("district"), starts_with("cde_"), setting, school_year))

Sys.sleep(1)
toc() # fine but make a local backup in case of connectivity

### Export crosswalk the first time ----
write_excel_csv(sch_names,
  paste0(temp_wd, "/", today(), "_sch_crosswalk.csv"),
  na = "",
  append = FALSE,
  col_names = TRUE,
  quote = "needed",
  escape = "none",  eol = "\r\n")
}

# Read in local versions ----
# NB: DPS crew will need to be careful of today() value
# use instead `read_csv(paste0(temp_wd, "/", today(), "_orig_df.csv"))`

dir_ls(temp_wd) # see line 20 above

orig_local = read_csv("2026-06-10_orig_df.csv") |> as_tibble()
sch_names = read_csv("2026-06-10_sch_crosswalk.csv") |> as_tibble()

# Exploratory on descriptives & missings, could move these to the QMD ----

## School mobility prevalence ----
temp0 = orig_local |>
  group_by(stu_id) |>
  summarise(unq_schs = n_distinct(sch_id)) |>
  ungroup()
max(temp0$unq_schs) # three -- see EDA_notes.qmd for decision

## Panel data for same kids - confirm? ----
length(unique(orig_local$stu_id))
# 18791
temp1 = orig_local |>
  group_by(stu_id) |>
  summarise(unq_years = n_distinct(year)) |>
  ungroup()
max(temp1$unq_years) # NOPE! no kids repeat across years? Demo data!

## Tests in spanish plus ELs ----
table(orig_local$test, orig_local$subject, orig_local$year)
table(orig_local$test, orig_local$subject, orig_local$grade)

### WIDA is a EL proficiency test ----
# parallel to TELPAS, probably? Who is taking this WIDA instrument?
table(orig_local$access, orig_local$test, orig_local$grade, exclude = NULL)
# oooh yikes for grades 3 and 4
# ... do the later years show continued monitoring ? check if it aligns with EL flag
temp2 = orig_local |>
  filter(test == "CMAS SLA") |>
  select(sch_id, grade, ell, access)

table(temp2$ell, temp2$access, exclude = NULL) # ok, not just the "ell" kids

### Who sat the WIDA?
temp3 = orig_local |>
  filter(!is.na(access)) |>
  select(sch_id, grade, ell, test, year)

table(temp3$ell, temp3$test, temp3$grade, exclude = NULL) # there are 77 kids *not* flagged as ELL who sat the WIDA

table(temp3$ell, temp3$test, temp3$year, exclude = NULL) # mostly in 2526 (74)

# No, doesn't align with the EL flag in grades 3 and 4
temp4 = orig_local |> filter(ell == "Y", grade <= 4)

length(unique(temp4$stu_id)) # 1975 students over 4 years

# Some in every year?
table(temp4$access, temp4$test, temp4$grade, temp4$year, exclude = NULL)
# yes: in every year there are gr3 and gr4 kids taking CMAS in english
# too bad we don't have their actual EL program codes to check if this is OK

## Look for missing data in key vars ----
temp5 = orig_local |> filter(is.na(year)) # 0 score, lev_l, year, gt, iep
temp6 = orig_local |> filter(is.na(ell)) # 22 unq IDs for ELL

## Clean up from exploratory----
rm(list = ls(pattern = "^temp"))
