# Unequal Structures and Agentic Selves: The Determinants of University Enrolment for Children of Migrants in Korea

Quantitative analysis code for an MSc dissertation (Inequalities and Social Science) examining how family background, social capital, social-psychological mediators, and ethnicity (maternal country of origin) shape the university outcomes of multicultural (mixed-origin) adolescents in South Korea

---

## Overview

This repository contains the R code used to construct variables, fit models, and produce publication-ready tables from the **Multicultural Adolescents Panel Study (MAPS / 다문화청소년패널조사)**. The analysis sits within the **status-attainment tradition** — Blau and Duncan (1967), the Wisconsin model (Sewell, Haller and Portes, 1969), and segmented assimilation (Portes and Zhou, 1993) — and asks how far structural background factors versus mediating aspirations and achievement account for the university trajectories of this group.

The study asks three linked questions:

1. **Attainment.** What predicts whether a multicultural adolescent attends university at all, and whether they enter a four-year (bachelor's) rather than a junior/associate programme?
2. **Mechanisms.** To what extent are the effects of family background transmitted through educational aspirations, academic achievement, and social capital?
3. **Ethnicity** Does (maternal) country of origin have any residual effects after accounting for family background and intermediary mechanisms?
4. **Gender.** Do these processes operate differently for young men and women?

## Data

The analysis uses **MAPS Panel 1 (1기)**, a longitudinal survey following a nationally distributed cohort of multicultural adolescents. This project draws on **waves 1–10 and 12**, restricting the analytic sample to respondents present at wave 10.

> **Data availability.** The MAPS microdata are third-party and **not included in this repository**. They are available on application to the survey administrator (the National Youth Policy Institute). Place the wave files under `data/raw/` using the naming pattern the scripts expect (`w1_c.csv`, `w2_c.csv`, … `w10_c.csv`, `w12_c.csv`). Variable definitions follow the *MAPS 1기 데이터유저가이드*.

Key measures constructed in the pipeline:

| Domain | Variables |
|---|---|
| **Outcomes** | University attendance (any); bachelor's vs. below; a three-category ordinal attainment measure (below university / associate / bachelor) |
| **Family SES** | Equivalised household income (logged; high-school and childhood windows); parental education |
| **Social capital** | Parental, peer, and teacher support composites |
| **Ethnicity** | China, Japan, Southeast Asia, Others |
| **Mediators** | Educational aspiration (years); subjective academic achievement (grades) |
| **Background / controls** | Mother's country of origin; gender; region; area type; household structure |

## Methods

- **Binary logistic regression** (`glm`, logit link) for the two attainment outcomes, entered in five theory-driven blocks (base → social capital → aspiration → achievement → full).
- **Gender-stratified models** run separately for young men and women.
- **OLS models** for the mediators, examining the social-background determinants of aspiration, achievement, and parental support.
- **Panel attrition model** characterising who remains in the sample at wave 10.
- **Continuous predictors standardised** following Gelman (2008), rescaling by two standard deviations (`arm::rescale`) so their magnitudes are comparable to binary predictors.
- **Average marginal effects** reported via `marginaleffects::avg_comparisons()`, with **HC0 heteroskedasticity-robust standard errors**.
- **Tables** generated with `modelsummary` and `gtsummary` and exported to Word.
> **Multiple imputation:** An alternative approach that addresses item missingness (NOT attrition bias) has been added (analysis_mice.R). This R script recreates Tables 2-7 using the "mice" package. For this approach, standard errors are pooled using Rubin's rule - Not an issue, as the design effect was minimal to begin with (average 1.7 students sampled by school/cluster).

## Reproducing the analysis

1. **Install R** (≥ 4.2) and the required packages:

   ```r
   install.packages(c(
     "tidyverse", "arm", "sandwich", "lmtest", "car",
     "marginaleffects", "modelsummary", "gtsummary", "flextable"
   ))
   ```

2. **Add the data.** Obtain the MAPS wave files and place them in `data/raw/` (see *Data availability* above).

3. **Create the output folder** if it does not exist:

   ```r
   dir.create("output", showWarnings = FALSE)
   ```

4. **Run the scripts** from the project root:

   ```r
   source("analysis_main.R")   # data preparation, models, and tables
   source("analysis_attrition.R")   # attrition model
   ```

   `analysis_main.R` runs end to end: it merges the waves, cleans and recodes variables, fits the models, and writes the tables to `output/`.

### Notes on dependencies

- `dplyr::select()` and `dplyr::filter()` are qualified explicitly in places because `MASS` (loaded via `arm`) masks `select`. If you adapt the code, keep the `dplyr::` prefixes or load the `conflicted` package to surface clashes.
- Word export runs through `flextable`; switch the `output =` argument to `.html` or `.tex` in the `modelsummary()` calls if you prefer another format.

## Theoretical framework

The empirical strategy operationalises the status-attainment model. Where Blau and Duncan (1967) modelled attainment as continuous years of schooling, this study uses ordinal outcomes, aligning more closely with the aspirations-extended Wisconsin approach (Sewell, Haller and Portes, 1969). Modelling decisions — block entry, the confounder/mediator distinction, and the treatment of parental education — are theory- rather than data-driven.

## Ethics

The broader mixed-methods study from which this component is drawn received ethical approval from the **LSE Research Ethics Committee**. This repository contains only code for the secondary analysis of de-identified survey data; no personal or interview data are included.

## Citation

If you refer to this work, please cite:

> Park, J. (2026). *Unequal Structures and Agentic Selves: The Determinants of University Enrolment for Children of Migrants in Korea* [MSc dissertation]. London School of Economics and Political Science.

## License

The **code** in this repository is released under the MIT License (see `LICENSE`). The **MAPS data are not covered by this license** and remain subject to the terms of the data provider.
