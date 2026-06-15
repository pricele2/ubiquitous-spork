# Demo project
repo title: ubiquitous-spork

## Task directions

Create a visual presentation of no more than 10 slides with your analysis.

Provides an overview of performance results, trends, and recommendations that district leaders should know about CMAS student performance.

Uses Google Slides, and incorporates visualizations and interpretations.

You will have approximately 15 minutes to discuss your performance task – 10 minutes to walk through your presentation and 5 minutes to share your data approach.

1.  Title page & intro & photo of presenter

2.  Task at hand, overview: "Provide an overview of performance results, trends, and recommendations that district leaders should know about CMAS student performance"

- Anybody can give you a district-wide frequency table by grade and subject, which I have done at [private url]

- My Goal: Predict school performance on a constructed metric of proficiency for ELA & math each (logit), and compare these predictions with actual school performance. 

- Which types of schools are over- or under-performing in math or ELA, considering the non-school factors captured in the student demographic characteristics? 

- Drawing heavily on the BTO Analysis project (2020) from the Strategic Data Project at Harvard GSE, with Appalachia REL and Kentucky DOE. 

3.  20 schools and 4 acad years, n unique students
- 
- Student-level categorical vars: Grade (6), Sex (3 per C.R.S. 25-2- 113.8), 
- Curiously: No metrics for FRL/Eco-Dis, or attendance, or which form of test (alt?!), or prior formative scores, or EL program type, or 504, etc. 
- Chose not to create an 'ever / never / now ' var for EL but could have yielded additional insights

4.  District POV: Math

5.  District POV: ELA

6.  District POV: ELL & WIDA Scores
- EL Proficiency at 4.5 (rounds to 5)

7.  GT and SPED

8.  

9.

10. If this was not a demo project: 

- Campus Report Card with grade levels and special pops, with Quarto and a for-loop 
- Either PDF or HTML 
- Make it interactive! Compare two schools on any 2 metrics
**- See my mockup built in Lucid at URL**
- Talked to university and industry colleagues about the model assumptions! 
- Identify clusters of actionable elements across schools

11. More details at pricele2.github.io / Appendix, baybeeeee

- Citations
- `lmer4` and author workshop at https://pages.stat.wisc.edu/~bates/UseR2008/WorkshopD.pdf 
- `PLM` and suggested tests at https://libguides.princeton.edu/R-Panel 
- `merTools` (mixed-effects representation)
- Lewis, Crystal. 2026. “Trying Out Dplyr 1.2.0.” February 9. https://cghlewis.com/blog/dplyr_update/. 

