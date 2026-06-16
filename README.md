# Demo project
repo title: ubiquitous-spork

## Task directions

Create a visual presentation of no more than 10 slides with your analysis.

Provides an overview of performance results, trends, and recommendations that district leaders should know about CMAS student performance.

Uses Google Slides, and incorporates visualizations and interpretations.

You will have approximately 15 minutes to discuss your performance task – 10 minutes to walk through your presentation and 5 minutes to share your data approach.

### 1.  Title page & intro & photo of presenter

### 
2.  Task at hand, overview: "Provide an overview of performance results, trends, and 
recommendations that district leaders should know about CMAS student performance"

- Anybody can give you a district-wide frequency table by grade and subject and year, which I have done and I'll show my work 

- But expertise can go beyond organizing the facts into tables: wanted recommendations to be meaningful / actionable 

- Results and trends and THEN recommendations

3.  20 schools and 4 acad years, n unique students
- Assigned dummy names to campus IDs 1:20
- Student-level vars: Grade (6), Sex (per C.R.S. 25-2- 113.8), 
- Some hiccups in the dataset, which is appropriate for project -- noted in eda_notes.qmd
- Curiously: No metrics for FRL/Eco-Dis, or attendance, or which form of test (alt?!), or prior formative scores, or EL program type, or 504, etc. 

4.  District POV: ELA
facet_wrap by grade by year (stacked bar? gif?)

4.  District POV: Math
facet_wrap by grade by year (stacked bar? gif?)

<verbal transition> Simple descriptive statistics can be ... deceptively simple
<Anscombe's quartet, or Datasaurus dozen>

6. Recommendations: Predict school performance CMAS ELA & CMAS math, and compare the model with actual school performance. Considering the non-school factors captured in the student demographic characteristics.

- Which schools are performing as suggested by the model? 
- Which schools are over- or under-performing in math or ELA? 

- Drawing heavily on the BTO Analysis project (2020) from the Strategic Data Project at Harvard GSE, with Appalachia REL and Kentucky DOE. 

- Lots of documentation at ____ 

# Colors in gslides deck template
#ff664f orangey-red
#51bc84 minty green
#6ad2e9 tiffany blue
#ffe636 accent yellow
#fdbad9 blossom pink
#feffef neutral background
#7570b3 stronger purple

#### Tried to play through it in the Shiny app from merTools pkg 
library(merTools) # # nb - this will mask SELECT inside dplyr
merTools::plotREsim(merTools::REsim(m_math, n.sims = 100), stat = "median", sd = TRUE)
merTools::shinyMer(m_math, simData = sch_avg_center[1:100, ]) # first 150 rows
librarian::check_attached() # make sure merTools isn't still attached


7. Math scatterplot

8. ELA scatterplot

9. Both scatterplot

10. If this was not a demo project: 

- Campus Report Card with grade levels and special pops, with Quarto and a for-loop 
- Either PDF or HTML 
- Create an 'ever / never / now ' var for EL 
- Make it interactive! Compare two schools on any 2 metrics
- Talked to university and industry colleagues about the model assumptions! 
- Identify clusters of actionable elements across schools

11. More details at pricele2.github.io / Appendix, baybeeeee

- Citations
- `lmer4` and author workshop at https://pages.stat.wisc.edu/~bates/UseR2008/WorkshopD.pdf 
- `PLM` and suggested tests at https://libguides.princeton.edu/R-Panel 
- `merTools` (mixed-effects representation)
- Lewis, Crystal. 2026. “Trying Out Dplyr 1.2.0.” February 9. https://cghlewis.com/blog/dplyr_update/. 

