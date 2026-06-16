# # # # # # # #
# Script 3 -- build the descriptive barcharts showing trends
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

# # # # # # # # # # # # # # # # END FRONTMATTER

# pull in data to graph annual trends ----
# Inherit from 02_recode or import it locally
to_graph =
  read_csv(paste0(temp_wd, "/",
                  # today(), # toggle today on if running all at once
                  "2026-06-14",
                  "_analytic_dataset.csv")) |> as_tibble() |>
  select(year, stu_id, grade, mat_cmas_level, ela_cmas_level)

## Math results/trends (district-wide) ----
temp = to_graph |>
  select(-ela_cmas_level) |>
  unique() |>
  mutate(cmas_label =
    case_when(
     mat_cmas_level == 1 ~ "Did Not Meet",
     mat_cmas_level == 2 ~ "Partially",
     mat_cmas_level == 3 ~ "Approached",
     mat_cmas_level == 4 ~ "Meet|Exceed",
     mat_cmas_level == 5 ~ "Meet|Exceed",
      TRUE ~ NA)) |>
  select(-mat_cmas_level) |>
  select(year, grade, cmas_label, stu_id)

temp0 = temp |>
  group_by(year, grade) |>
  summarise(denom = n_distinct(stu_id)) |>
  ungroup()

temp1 = temp |>
  group_by(year, grade, cmas_label) |>
  summarise(numer = n_distinct(stu_id)) |>
  ungroup()

temp2 = left_join(temp1, temp0, by = c("year", "grade"))

tg_mat = temp2 |> mutate(pct = (numer / denom)*100) |>
  filter(!is.na(cmas_label)) |>
  mutate(cmas_label = factor(cmas_label,
  levels = c("Meet|Exceed", "Approached", "Partially", "Did Not Meet"))) |>
  mutate(yr =
    recode_values(year,
    "22-23" ~ "23",
    "23-24" ~ "24",
    "24-25" ~ "25",
    "25-26" ~ "26"))

rm(list = ls(pattern = "^temp"))

## ELA results/trends (district-wide) ----
temp = to_graph |>
  select(-mat_cmas_level) |>
  unique() |>
  mutate(cmas_label =
           case_when(
             ela_cmas_level == 1 ~ "Did Not Meet",
             ela_cmas_level == 2 ~ "Partially",
             ela_cmas_level == 3 ~ "Approached",
             ela_cmas_level == 4 ~ "Meet|Exceed",
             ela_cmas_level == 5 ~ "Meet|Exceed",
             TRUE ~ NA)) |>
  select(-ela_cmas_level) |>
  select(year, grade, cmas_label, stu_id)

temp0 = temp |>
  group_by(year, grade) |>
  summarise(denom = n_distinct(stu_id)) |>
  ungroup()

temp1 = temp |>
  group_by(year, grade, cmas_label) |>
  summarise(numer = n_distinct(stu_id)) |>
  ungroup()

temp2 = left_join(temp1, temp0, by = c("year", "grade"))

tg_ela = temp2 |> mutate(pct = (numer / denom)*100)|>
  filter(!is.na(cmas_label)) |>
  mutate(cmas_label = factor(cmas_label,
           levels = c("Meet|Exceed", "Approached", "Partially", "Did Not Meet"))) |>
  mutate(yr =
           recode_values(year,
                         "22-23" ~ "23",
                         "23-24" ~ "24",
                         "24-25" ~ "25",
                         "25-26" ~ "26"))

rm(list = ls(pattern = "^temp"))
rm(to_graph)

## Graphics themselves ----
# colnames are "year" "grade" "cmas_label" "pct"
# Stacked Bar chart, bars by year, facet by grade

### MATH ---
ggplot(tg_mat, aes(x = yr, y = pct, fill = cmas_label)) +
  geom_col(position = "fill") +
  facet_grid(~ grade) +
scale_fill_manual(values = c("#51bc84", "#6ad2e9", "#ffe636","#FF664F")) +
  scale_y_continuous(name = "% Students per CMAS Level", labels = scales::percent) +
  sdp_theme() +
  theme(panel.grid = element_line(color = "grey92")) +
  labs(
    x = "Grade Level, per Testing Year (20XX)",
    y = "Percentage",
    fill = "CMAS Level",
    title = "CMAS Math Improves Each Year Overall",
    subtitle = "Modest Losses in Grades 3+4 Outweighed by Increases in Grades 5 - 8") +
  theme(legend.position = "bottom")

ggsave("g_mat_bygrade.png",
       plot = get_last_plot(),
       device = "png",
       path = "C:/Users/Lauren/Documents/Search/DPS-Presentation/",
       width = 9,
       height = 4.5,
       units = c("in"),
       dpi = 300)

### ELA ----

ggplot(tg_ela, aes(x = yr, y = pct, fill = cmas_label)) +
  geom_col(position = "fill") +
  facet_grid(~ grade) +
  # facet_wrap(~ grade, nrow = 2) +
  scale_fill_manual(values = c("#7570b3", "#fdbad9", "#ffe636","#FF664F")) +
  scale_y_continuous(name = "% Students per CMAS Level", labels = scales::percent) +
  sdp_theme() +
  theme(panel.grid = element_line(color = "grey92")) +
  labs(
    x = "Grade Level, per Testing Year (20XX)",
    y = "Percentage",
    fill = "CMAS Level",
    title = "CMAS ELA/SLA Sees Less Consistency in Trends",
    subtitle = "Reminder: Grade 3+4 EL Students May Test in Spanish") +
  theme(legend.position = "bottom")

ggsave("g_ela_bygrade.png",
       plot = get_last_plot(),
       device = "png",
       path = "C:/Users/Lauren/Documents/Search/DPS-Presentation/",
       width = 9,
       height = 4.5,
       units = c("in"),
       dpi = 300)

# Time permits: Run N-counts (table) for each and Write to Google Sheet
