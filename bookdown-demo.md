--- 
title: "Supplement to Early childhood linear growth failure in low-and middle-income countries"
author: "Jade Benjamin-Chung et al."
date: "2021-06-03"
site: bookdown::bookdown_site
output: bookdown::gitbook
documentclass: book
bibliography: [book.bib, packages.bib]
biblio-style: apalike
link-citations: yes
github-repo: rstudio/bookdown-demo
description: "This is supplementary information to Early childhood linear growth failure in low-and middle-income countries"
---

# Overview

**Recommended citation:** Benjamin-Chung J, et al. 2020. Early childhood linear growth failure in low-and middle-income countries. *Submitted*. doi. 

This site contains supplementary information to the *Early childhood linear growth failure in low-and middle-income countries*. 




<!--chapter:end:index.Rmd-->

# Quality assurance {#QA}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

To check for outliers in length measurements, we plotted the distribution of raw length measurements by age and sex against different percentiles of the World Health Organization child growth standard distribution. The majority of observations fell within 3 standard deviations of the mean of the standard for males and females. There were certain cohorts, such as COHORTs Guatemala, that had a larger proportion of observations outside of 3 standard deviations from the mean. There was a higher variability of length measurements in the PROBIT, ZVITAMBO, and Keneba cohorts.  




\includegraphics[width=33.33in]{/data/KI/ki-manuscript-output/figures//shared/laz_QA} 



\includegraphics[width=104.17in]{/data/KI/ki-manuscript-output/figures//shared/laz_QA_cohort} 


<!--chapter:end:01-quality-assurance.Rmd-->

# Severe stunting analyses {#severe-stunting}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Below, we display plots for the age-specific prevalence and cumulative incidence of severe stunting (LAZ < -3). Overall, the patterns are the same as for stunting (LAZ < -2), with the peak in prevalence at ages 18-24 months and the highest incidence proportion from 0-3 months. 




## Age-specific severe stunting prevalence

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-3-prev-overall_region--allage-primary} 


## Age-specific severe stunting incidence

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-3-inc-overall_region--allage-primary} 

<!--chapter:end:02-severe-stunting.Rmd-->

# Assessment of potential secular trends {#secular-trends}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

This study included cohorts that measured child growth from 1969 to 2014. To assess potential secular trends, we plotted the mean length-for-age Z-score (LAZ) over time. The plot below shows the individual observations from included studies over this range of years. Curves were fit to the data using generalized additive models, with the solid line fit through the median LAZ by birth year, and the dashed lines mark the 1st and 3rd quartiles. There does not appear to be a secular trend in LAZ. 





\includegraphics[width=33.33in]{/data/KI/ki-manuscript-output/figures//shared/laz_secular_trend} 



<!--chapter:end:03-secular-trends.Rmd-->

# Analyses of age at first measurement {#age-meas}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Our analyses of stunting incidence assumed that children whose first measurement occurred after birth were assumed to have experienced stunting onset at the age halfway between birth and the first measurement. To assess the extent to which this assumption influenced our estimates, we plotted the distribution of age at first measurement and the age at enrollment. The vast majority of children were enrolled close to birth, and the majority were less than 5 days of age at their first measurement. 



## Histogram of first measurement within age 0-30 days

### All cohorts

\includegraphics[width=33.33in]{/data/KI/ki-manuscript-output/figures//shared/age_histogram_first_month} 

### Cohort-stratified

\includegraphics[width=45.83in]{/data/KI/ki-manuscript-output/figures//shared/age_histogram_first_month_cohort} 



## Histogram of age at enrollment

### All cohorts

\includegraphics[width=33.33in]{/data/KI/ki-manuscript-output/figures//shared/enrollment_age_histogram_over_7d} 

### Cohort-stratified

\includegraphics[width=45.83in]{/data/KI/ki-manuscript-output/figures//shared/enrollment_age_histogram_over_7d_cohort} 



<!--chapter:end:04-age-at-first-measurement.Rmd-->

# Comparison to DHS surveys from matching countries {#DHS}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

In the manuscript, we compared the mean length-for-age Z-score by age and region in our included studies and in Demographic and Health Survey (DHS) datasets from the same countries as those included in our study. Here we present analogous plots including all of the countries from each region. The plots are kernel density distributions of length-for-age z-scores (LAZ) in Demographic and Health Survey (DHS) datasets (dashed lines) and ki cohorts (solid lines). DHS estimates use the most recent survey from all countries in each region. Open circles indicate median LAZ in ki cohorts; closed circles indicate median LAZ in DHS cohorts. 




#####NEED TO UPDATE FIGURE-NOT FOUND




<!--chapter:end:05-DHS-ki-countries.Rmd-->

# Cohort-specific results {#cohort}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Here, we present cohort-specific estimates of length-for-age Z-score by age, age-specific prevalence, and age-specific incidence. 



<!-- ##################################################################################### -->
## Mean length-for-age Z-score by age

### Africa

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-quant-cohort-africa-allage-primary} 

### Latin America

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-quant-cohort-latamer-allage-primary} 

### South Asia

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-quant-cohort-asia-allage-primary} 


<!-- ##################################################################################### -->
## Age-specific prevalence

### Africa

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-cohort-africa-allage-primary} 

### Latin America

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-cohort-latamer-allage-primary} 

### South Asia

\includegraphics[width=62.5in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-cohort-asia-allage-primary} 


<!-- ##################################################################################### -->
## Age-specific incidence

### Africa

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-cohort-africa-allage-primary} 

### Latin America

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-cohort-latamer-allage-primary} 

### South Asia

\includegraphics[width=62.5in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-cohort-asia-allage-primary} 


<!-- ##################################################################################### -->
## Length velocity by age and sex

### Africa

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-length-2-length_vel-cohort-africa-allage-primary} 

### Latin America

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-length-2-length_vel-cohort-latamer-allage-primary} 

### South Asia

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-length-2-length_vel-cohort-asia-allage-primary} 


<!-- ##################################################################################### -->
## LAZ velocity by age and sex

### Africa

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-laz_vel-cohort-africa-allage-primary} 

### Latin America

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-laz_vel-cohort-latamer-allage-primary} 

### South Asia

\includegraphics[width=75in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-laz_vel-cohort-asia-allage-primary} 


<!--chapter:end:06-cohort-specific.Rmd-->

# Primary analyses excluding the PROBIT study {#exclude-PROBIT}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

Only one cohort from Europe met the inclusion criteria for this study -- the PROBIT study. To assess whether inclusion of this study altered our overall study inference, we repeated analyses excluding the PROBIT cohort, as shown below in the "Overall" panels. Results were very similar with and without the PROBIT cohort. Stunting prevalence and incidence were slightly higher at birth when excluding PROBIT, but overall age-specific patterns remained the same. For this reason, we chose to retain PROBIT in the primary analyses presented in this manuscript.  



<!-- ##################################################################################### -->
## Mean length-for-age Z-score by age

### Including PROBIT

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-mean-overall_region--allage-primary} 

### Excluding PROBIT

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-mean-overall_region--allage-primary_no_probit} 


<!-- ##################################################################################### -->
## Age-specific prevalence

### Including PROBIT

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-primary} 

### Excluding PROBIT

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-primary_no_probit} 

<!-- ##################################################################################### -->
## Age-specific incidence

### Including PROBIT

\includegraphics[width=66.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-primary} 

### Excluding PROBIT

\includegraphics[width=66.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-primary} 





<!--chapter:end:07-excluding-PROBIT.Rmd-->

# Analysis with monthly cohorts {#monthly}

---
output:
  pdf_document:
    keep_tex: yes
fontfamily: mathpazo
fontsize: 9pt
---

\raggedright

To explore the influence of differing numbers of cohorts contributing data at different ages, we conducted a sensitivity analysis in which we subset data to cohorts that measured anthropometry monthly from birth to 24 months.

In this sensitivity analysis, the mean length-for-age Z-score was higher in Latin America and exhibited less of a downwards trajectory with age. Age-specific stunting prevalence and incidence was slightly lower in Latin America and Asia and slightly higher in Africa. Standard errors were smaller for Latin America because the analyses with monthly cohorts excluded the COHORTS Guatemala study, which had a very high stunting prevalence compared to other Latin American cohorts. 



<!-- ##################################################################################### -->
## Mean length-for-age Z-score by age

### All eligible cohorts

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-mean-overall_region--allage-primary} 

### Cohorts that measured monthly from birth to 24 months


\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-laz-2-mean-overall_region--allage-month24} 

<!-- ##################################################################################### -->
## Age-specific severe stunting prevalence

### All eligible cohorts


\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-primary} 

### Cohorts that measured monthly from birth to 24 months


\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-month24} 

<!-- ##################################################################################### -->
## Age-specific severe stunting incidence

### All eligible cohorts


\includegraphics[width=66.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-primary} 

### Cohorts that measured monthly from birth to 24 months


\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-month24} 

<!-- ##################################################################################### -->
## Linear growth velocity

### All eligible cohorts


\includegraphics[width=50in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-vel-overall--allage-primary} 

### Cohorts that measured monthly from birth to 24 months


\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-vel-overall--allage-month24} 


<!--chapter:end:08-monthly-cohorts.Rmd-->

# Sensitivity analysis using fixed effects {#fixed-effects}

The primary analyses presented in this manuscript pooled across individual studies using random effects. Inferences about estimates from fixed effects models are restricted to only the included studies.[^1] The random effects approach was more conservative in the presence of study heterogeneity, as evidenced by larger confidence intervals around each point estimates. Overall, the inference from results produced by each method was similar. 



<!-- ##################################################################################### -->
## Age-specific prevalence

### Random effects

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-primary} 

### Fixed effects

\includegraphics[width=58.33in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-prev-overall_region--allage-fe} 

[ADD CAPTION]

<!-- ##################################################################################### -->
## Age-specific incidence

### Random effects

\includegraphics[width=66.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-primary} 

### Fixed effects

\includegraphics[width=66.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-inc-overall_region--allage-fe} 

[ADD CAPTION]

<!-- ##################################################################################### -->
## Changes in stunting status by age

### Random effects

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-flow-overall--allage-re} 

### Fixed effects

\includegraphics[width=41.67in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-flow-overall--allage-fe} 

[ADD CAPTION] 


<!-- ##################################################################################### -->
## Linear growth velocity

### Random effects

\includegraphics[width=50in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-vel-overall--allage-primary} 

### Fixed effects

\includegraphics[width=50in]{/data/KI/ki-manuscript-output/figures//stunting/fig-stunt-2-vel-overall--allage-primary} 


[^1]: Hedges, L. V. & Vevea, J. L. Fixed- and random-effects models in meta-analysis. Psychol. Methods 3, 486–504 (1998).

<!--chapter:end:09-fixed-effects.Rmd-->

