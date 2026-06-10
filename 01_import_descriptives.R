# # # # #
# LPRICE5
#
# # # # #

# Define packages to load, then bring in using Librarian ----

# install.packages("librarian") # NB for DPS crew: Run this the first time

library(librarian)
stock(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4,   # lib = lib_paths()[1L], # use to force-fix dir if necessary
      # update_all =  TRUE, # run update_all the very first time
      ask = TRUE)

librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, update_all =  FALSE, ask = TRUE)

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

## School names crosswalk ----
tic()
sch_names = read_sheet(
  "https://docs.google.com/spreadsheets/d/1TYbfKqOkfkUJajM-_0b2OYroUIKSBuFhfxuS0-ptf3s/edit?usp=sharing",
  range = "sch-name-id!A6:K26",
  col_names = TRUE,
  col_types = NULL,
  na = "",
  trim_ws = TRUE,
  .name_repair = "unique") |>
  as_tibble() |>
  clean_names() |>
  select(-c(starts_with("district"), ends_with("grade"), setting, school_year))

Sys.sleep(1)
toc()


## Export files the first time ----
write_excel_csv(orig_df,
    paste0(temp_wd, "/", today(), "_orig_df.csv"),
    na = "",
    append = FALSE,
    col_names = TRUE,
    quote = "needed",
    escape = "none",  eol = "\r\n")



# Read in flat file from local ----
orig_local <- read.csv(paste0(temp_wd, "/", today(),"_orig_df.csv"), stringsAsFactors=TRUE)

## Recodes! for multilevel modeling ----

### sex - F ----
# NB: three factor levels per C.R.S. 25-2- 113.8



### race - Wh b/c 'achievement gap' ----



### ell - b/c Consent Decree re ELL students ----



### lang tested (eng/sp) for littles (var: subject) ----



### students with disabilities, and GT ----



### any twice-exceptional kiddos? ----



### change 'access' to label, and cat var (forcats), fix NA ----



### baseline year, and add label (forcats) ----





## Codebook out the vardefs, descriptives & missings ----
table(orig_df$sch_id, orig_df$grade, orig_df$year) # yup, got some K8s here

# length(unique(orig_df$stu_id))
# 18791

# ACCESS_Overall is the student’s WIDA ACCESS level.





# Refs & Notes for later in the project ----

# https://drbtlr.github.io/beating-the-odds/beating-the-odds-code.html

# source("https://raw.githubusercontent.com/UrbanInstitute/urban_R_theme/master/urban_theme_windows.R")
