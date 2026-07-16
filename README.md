# Slide Outlines for Demo
repo title: ubiquitous-spork

### Intro & photo of presenter

### Task at hand, overview

<!-- 
- Anybody can give you a district-wide frequency table by grade and subject and year, which I have done and I'll show my work 

- But expertise can go beyond organizing the facts into tables: wanted recommendations to be meaningful / actionable 

- Results and trends and THEN recommendations  
--> 

### Parameters and inputs  

N schools and Y acad years, n unique students  

- Campus names & campus IDs 1:N  

- Student-level vars:  

- Hiccups in the dataset, appropriate for task -- noted in eda_notes.qmd  

- Curiously, no metrics for:   

<!-- 
#FRL/Eco-Dis, or attendance, or which form of test (alt?!), or prior formative scores, or EL program type, or 504, etc. 
--> 

### Graphic 1 

Set to facet_wrap by grade by year (stacked bar)

### Graphic 2 

Set to facet_wrap by grade by year (stacked bar)
<!-- 

<verbal transition> Simple descriptive statistics can be ... deceptively simple
<Anscombe's quartet, or Datasaurus dozen>
--> 

### Recommendations:  

<!-- 

Predict school performance CMAS ELA & CMAS math, and compare the model with actual school performance. Considering the non-school factors captured in the student demographic characteristics.

- Which schools are performing as suggested by the model?
- Which schools are over- or under-performing in math or ELA?

- Drawing heavily on the BTO Analysis project (2020) from the Strategic Data Project at Harvard GSE, with Appalachia REL and Kentucky DOE.

- Lots of documentation at ____
-->
- Lorem ipsum  

- Sit dolor  

- Amet 


### Colors in gslides deck template
Hex ff664f orangey-red  

Hex 51bc84 minty green  

Hex 6ad2e9 tiffany blue  

Hex ffe636 accent yellow  

Hex fdbad9 blossom pink  

Hex feffef neutral background  

Hex 7570b3 stronger purple

<!-- 
Tried to play through the model outputs in the Shiny app from merTools pkg 

library(merTools) # # nb - this will mask SELECT inside dplyr
merTools::plotREsim(merTools::REsim(m_math, n.sims = 100), stat = "median", sd = TRUE)
merTools::shinyMer(m_math, simData = sch_avg_center[1:100, ]) # first 150 rows
librarian::check_attached() # make sure merTools isn't still attached
-->

### Graphic 1 as scatterplot


### Graphic 2 as scatterplot


### Both scatterplot


### If this was not a demo project: 
<!-- 
- Campus Report Card with grade levels and special pops, with Quarto and a for-loop
- Either PDF or HTML
- Create an 'ever / never / now ' var for EL
- Make it interactive! Compare two schools on any 2 metrics
- Talked to university and industry colleagues about the model assumptions!
- Identify clusters of actionable elements across schools
-->



### Finally: 

<!-- 
More details at pricele2.github.io / Appendix
- Citations
# - `lmer4` and author workshop at https://pages.stat.wisc.edu/~bates/UseR2008/WorkshopD.pdf 
# - `PLM` and suggested tests at https://libguides.princeton.edu/R-Panel 
# - `merTools` (mixed-effects representation)
# - Lewis, Crystal. 2026. “Trying Out Dplyr 1.2.0.” February 9. https://cghlewis.com/blog/dplyr_update/. 
-->
