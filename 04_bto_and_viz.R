# # # # # # # #
# Script 4 "Beat the Odds" analysis and dataviz
# # # # # # # #

# Front matter ICYMI
librarian::shelf(tibble, dplyr, tidyr, readr, stringr, janitor, readxl, lubridate, openxlsx, ggplot2, googlesheets4, tictoc, forcats, fs, codebook,
                 # additional pkgs for this stage
                 lme4, lmerTest, ggrepel, modelbased,
                 update_all =  FALSE, ask = TRUE)
# Confirm everything loaded appropriately
print(.Last.value)
# Grab WD and sch_names again
temp_wd = "C:/Users/Lauren/Documents/R/dps-demo"
sch_names = read_csv("2026-06-10_sch_crosswalk.csv") |> as_tibble() |>
  mutate(sch_id = as.character(sch_id)) |>
  select(starts_with("sch"))

# # # # # # # # # # # # # # # #

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
        pctER >= 70 ~ "Hi",
        pctER <= 30 ~ "Lo",
        TRUE ~ "No"))  |>
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
  # mutate(bto_type = case_when(     ))

# Scatterplot! ----
#

sch_bto_data |>
  ggplot(aes(estimate_math, estimate_read, color = bto_type)) +
  geom_point(size = 2, alpha = .6) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("#E69F00", "#0049E6", "#FF00FF", "#999999", "#999999")) +
  sdp_theme() +
  theme(panel.grid = element_line(color = "grey92")) +
  ggrepel::geom_text_repel(
    data = filter(sch_bto_data, outlier_neg == TRUE | outlier_pos == TRUE),
    aes(label = first_hs_name),
    box.padding = 0.5,      # Distance between label and point
    point.padding = 0.3,    # Distance between text edge and point
    segment.color = "grey50" # Color of the line connecting text to point
  ) +
  labs(x = "Math Residual", y = "ELA Residual", color = "",
       title = "Schools that Performed Better/Worse that Expected in Math and ELA")



