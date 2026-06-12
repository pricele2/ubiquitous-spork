# # # # #
# LPRICE5
# Demonstration project for Manager, Reporting & Analytics
# # # # #

# Define packages to load, then bring in using Librarian ----
library(librarian)
stock(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, forcats,
      # update_all =  TRUE, # run update_all the first time or w new packages
      ask = TRUE)

librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats,
    update_all =  FALSE, ask = TRUE)

print(.Last.value) # Confirm everything loaded appropriately

# Define local WD for exports ----
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"
# Read in flat files from Google Sheets ----
# NB: Requires initial authorization in browser (after: choose option 2)

## Student level data ----
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

## Read in flat file of student data from local ----
orig_local <- read_csv(paste0(temp_wd, "/", today(),"_orig_df.csv")) |>

## School names crosswalk ----
tic()
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
# write_excel_csv(sch_names,
#   paste0(temp_wd, "/", today(), "_sch_crosswalk.csv"),
#   na = "",
#   append = FALSE,
#   col_names = TRUE,
#   quote = "needed",
#   escape = "none",  eol = "\r\n")

# Exploratory on descriptives & missings ----

## School mobility prevalence ----
temp0 = orig_local |>
  group_by(stu_id) |>
  summarise(unq_schs = n_distinct(sch_id)) |>
  ungroup()
max(temp0$unq_schs)

## Panel data for same kids - confirm? ----
length(unique(orig_local$stu_id))
# 18791
temp1 = orig_local |>
  group_by(stu_id) |>
  summarise(unq_years = n_distinct(year)) |>
  ungroup()
max(temp1$unq_years) # how TF do none of the kids repeat across years?

## kiddos testing in spanish - trends? ----
table(orig_local$test, orig_local$subject, orig_local$year)
table(orig_local$test, orig_local$subject, orig_local$grade)

## WIDA is a EL proficiency test ----
# parallel to TELPAS, probably? Who is taking this WIDA instrument?
table(orig_local$access, orig_local$test, orig_local$grade, exclude = NULL)
# oooh yikes for grades 3 and 4
# ... do the later years show continued monitoring ? check if it aligns with EL flag
temp2 = orig_local |>
  filter(test == "CMAS SLA") |>
  select(sch_id, grade, ell, access)

table(temp2$ell, temp2$access, exclude = NULL) # ok, not perfectly "ell" kids

temp3 = orig_local |>
  filter(!is.na(access)) |>
  select(sch_id, grade, ell, test, year)

table(temp3$ell, temp3$test, temp3$grade, exclude = NULL) # there are 77 kids *not* flagged as ELL who sat the WIDA

table(temp3$ell, temp3$test, temp3$year, exclude = NULL)

# No, doesn't align with the ELL flag in grades 3 and 4
temp4 = orig_local |> filter(ell == "Y", grade <= 4)

# How many kids?
length(unique(temp4$stu_id))

# Every year? yes
table(temp4$access, temp4$test, temp4$grade, temp4$year, exclude = NULL)


## Clean up from exploratory----
rm(list = ls(pattern = "^temp"))


# Recodes / forcats! for multilevel modeling ----

### sex - F ----
# NB: three factor levels per C.R.S. 25-2- 113.8



### race - Wh b/c 'achievement gap' ----



### ell - b/c Consent Decree re ELL students ----



### lang tested (eng/sp) for littles (var: subject) ----



### students with disabilities, and GT ----



### any twice-exceptional kiddos? ----



### change 'access' to label, and cat var (forcats), fix NA ----



### baseline year, and add label (forcats) ----


## Add school ID names from crosswalk ----



