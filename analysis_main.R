################################################################################
################################################################################
# Title: Quantitative Analysis Using the MAPS Database
# For: MSc Inequalities and Social Sciences Dissertation
# Version: Online
# Created: 28 July 2026
# Updated: 17 Aug 2026
################################################################################
################################################################################

## 0. SETTING UP ---------------------------------------------------------------

# 0.1. Load relevant packages
library(tidyverse)
library(sandwich)  # robust vcov estimators
library(lmtest)    # for robust coefficient tables
library(arm)       # Gelman (2008)

# 0.2. Load data
W01 <- read.csv("data/raw/w1_c.csv")
W02 <- read.csv("data/raw/w2_c.csv")
W03 <- read.csv("data/raw/w3_c.csv")
W04 <- read.csv("data/raw/w4_c.csv")
W05 <- read.csv("data/raw/w5_c.csv")
W06 <- read.csv("data/raw/w6_c.csv")
W07 <- read.csv("data/raw/w7_c.csv")
W08 <- read.csv("data/raw/w8_c.csv")
W09 <- read.csv("data/raw/w9_c.csv")
W10 <- read.csv("data/raw/w10_c.csv")
W12 <- read.csv("data/raw/w12_c.csv")


################################################################################

## 1. DATA MANIPULATION --------------------------------------------------------

# 1.1. Merge data and clean ----------------------------------------------------

# 1.1.1. Merge data
maps <- list(W01, W02, W03, W04, W05, W06, W07, W08, W09, W10, W12) %>%
  reduce(left_join)

# 1.1.2. Filter to only leave participants in w10
maps <- maps %>% filter(SURVEY1_w10 == 1)

# 1.1.3. Convert negative values to NA
maps <- maps %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x < 0, NA, .x)
    )
  )

# 1.1.4. Fix coding errors in income w1 ("1809") & w9 ("9999")
maps <- maps %>%
  mutate(
    income_01_w1 = ifelse(income_01_w1 > 1800, NA_integer_, income_01_w1),
    income_01_w9 = ifelse(income_01_w9 > 9000, NA_integer_, income_01_w9)
  )

# 1.1.5. Convert all variables < 14 to factors (highest # of categories = 13)
maps <- maps %>%
  mutate(
    across(
      where(~ (is.integer(.x) | is.numeric(.x)) & dplyr::n_distinct(.x) < 14),
      ~ as.factor(.x)
    )
  )


# 1.2. Recoding variables ------------------------------------------------------

# 1.2.1. Convert variables back to integer
vars_reconvert <- intersect(
  c("household_w1", "household_w7",  # household size
    "score_01_w5", "score_02_w5", "score_03_w5", "score_04_w5", "score_05_w5",
    "score_01_w8", "score_02_w8", "score_03_w8", "score_04_w8", "score_05_w8"
  ),
  names(maps)
)
maps <- maps %>%
  mutate(
    across(
      .cols = all_of(vars_reconvert),
      .fns  = ~ as.integer(as.character(.x))
    )
  )

# 1.2.2. Reconvert round 2
vars_reconvert2 <- grep(
  "^(par_support|fr_rela|te_rela)_b\\d{2}_w[789]$",
  names(maps), value = TRUE
)
maps <- maps %>%
  mutate(
    across(
      .cols = all_of(vars_reconvert2),
      .fns  = ~ as.integer(as.character(.x))
    )
  )

# 1.2.3. Dependent variables

# 1.2.3.1. Enrolled in university
maps <- maps %>%
  mutate(
    uni_attendance = factor(
      ifelse(uni_a01_w10 %in% 1 | uni_a01_w12 %in% 1, "Attended", "Not attended"),
      levels = c("Not attended", "Attended")
    )
  )

# 1.2.3.2. Enrolled in bachelor's degree
maps <- maps %>%
  mutate(
    ba_or_not = factor(
      ifelse((uni_a01_w10 %in% 1 & uni_a02_1_w10 %in% 2) |
               (uni_a01_w12 %in% 1 & uni_a02_1_w12 %in% 2), "Bachelor", "Below bachelor"),
      levels = c("Below bachelor", "Bachelor")
    )
  )

# 1.2.3.3. Dis-aggregated by outcome
maps <- maps %>%
  mutate(
    uni_type = factor(
      case_when(
        (uni_a01_w10 %in% 1 & uni_a02_1_w10 %in% 2) |
          (uni_a01_w12 %in% 1 & uni_a02_1_w12 %in% 2) ~ 'Bachelor',
        (uni_a01_w10 %in% 1 & uni_a02_1_w10 %in% 1) |
          (uni_a01_w12 %in% 1 & uni_a02_1_w12 %in% 1) ~ 'Associate',
        TRUE ~ 'Below uni'
      ),
      levels = c('Below uni', 'Associate', 'Bachelor')
    )
  )

# 1.2.4. Explanatory variables

# 1.2.4.1. Parental SES

# 1.2.4.1.1. Education
maps$par_education <- factor(ifelse(maps$par_edu_1_w1 %in% 1:2 & maps$par_edu_2_w1 %in% 1:2, "No",
                                    ifelse(maps$par_edu_1_w1 %in% 3:5 | maps$par_edu_2_w1 %in% 3:5, "Yes",
                                           NA)))

# 1.2.4.1.2. Income (high school)
maps <- maps %>%
  mutate(
    income_perm = rowMeans(
      across(c(income_01_w7, income_01_w8, income_01_w9)),
      na.rm = TRUE
    )
  )

# 1.2.4.1.2.1. Equivalised (high school)
maps$income_equi <- maps$income_perm / sqrt(maps$household_w7)

# 1.2.4.1.2.2. Log (high school)
maps$income_equi_log <- log(maps$income_equi)

# 1.2.4.1.3. Income (primary school)
maps <- maps %>%
  mutate(
    income_childhood = rowMeans(
      across(c(income_01_w1, income_01_w2, income_01_w3)),
      na.rm = TRUE
    )
  )

# 1.2.4.1.3.1. Equivalised (primary school)
maps$income_childhood_equi <- maps$income_childhood / sqrt(maps$household_w1)

# 1.2.4.1.3.2. Log (primary school)
maps$income_childhood_equi_log <- log(maps$income_childhood_equi)

# 1.2.5. Explanatory variables

# 1.2.5.1. Social capital

# 1.2.5.1.1. Parents
maps <- maps %>%
  mutate(par_sup_avg = rowMeans(
    across(matches("^par_support_b0")),
    na.rm = TRUE
  ))

# 1.2.5.1.2. Friends
maps <- maps %>%
  mutate(fr_rel_avg = rowMeans(
    across(matches("^fr_rela_b0")),
    na.rm = TRUE
  ))

# 1.2.5.1.3. Teachers
maps <- maps %>%
  mutate(te_rel_avg = rowMeans(
    across(matches("^te_rela_b0")),
    na.rm = TRUE
  ))

# 1.2.5.1.4. Parents, by type of support
ps_cols <- function(items, waves = 7:9) {
  grid <- expand.grid(item = items, wave = waves)
  nm <- sprintf("par_support_b%02d_w%d", grid$item, grid$wave)
  intersect(nm, names(maps))
}

maps <- maps %>%
  mutate(
    par_sup_emotional = rowMeans(across(all_of(ps_cols(1:3))), na.rm = TRUE),
    par_sup_knowledge = rowMeans(across(all_of(ps_cols(4:6))), na.rm = TRUE),
    par_sup_financial = rowMeans(across(all_of(ps_cols(7:9))), na.rm = TRUE)
  )

# 1.2.5.1.5. During middle school (parents & friends only)
par_cols <- grep("^par_support_a0[1-5]_w\\d+$", names(maps), value = TRUE)

fr_grid <- expand.grid(item = 5:7, wave = 4:6)
fr_cols <- intersect(sprintf("fr_sup_%02d_w%d", fr_grid$item, fr_grid$wave),
                     names(maps))

maps <- maps %>%
  mutate(
    across(all_of(c(par_cols, fr_cols)), ~ as.integer(as.character(.x))),
    soccap_par = rowMeans(across(all_of(par_cols)), na.rm = TRUE),
    soccap_fr  = rowMeans(across(all_of(fr_cols)),  na.rm = TRUE)
  )

# 1.2.5.2. Social-psychological mediating variables

# 1.2.5.2.1. Educational aspiration (2nd year middle school)
maps <- maps %>%
  mutate(
    edu_asp = case_when(
        edu_plan_01_w6 == 2 ~ 9,
        edu_plan_01_w6 == 3 ~ 12,
        edu_plan_01_w6 == 4 & edu_plan_02_w6 == 1 ~ 14.5,
        edu_plan_01_w6 == 4 & edu_plan_02_w6 %in% 2:5 ~ 16,
        edu_plan_01_w6 == 5 & edu_plan_03_w6 == 1 ~ 18,
        edu_plan_01_w6 == 5 & edu_plan_03_w6 == 2 ~ 23,
        TRUE ~ NA_integer_
        ),
      )

# 1.2.5.2.2. Grades (3rd year middle school & 2nd year high school)
cols5 <- c("score_01_w5","score_02_w5","score_03_w5","score_04_w5","score_05_w5")
cols8 <- c("score_01_w8","score_02_w8","score_03_w8","score_04_w8","score_05_w8")

maps <- maps %>%
  mutate(
    score_w5_avg = rowMeans(across(all_of(cols5)), na.rm = TRUE),
    score_w8_avg = rowMeans(across(all_of(cols8)), na.rm = TRUE),
    across(c(score_w5_avg, score_w8_avg), ~ ifelse(is.nan(.x), NA, .x))
  )

# 1.2.6. Country of Origin (mother)
maps <- maps %>%
  mutate(
    mom_origin = factor(
      case_when(
        par_nat_1_w1 %in% c(1, 8) ~ "Other",
        par_nat_1_w1 %in% c(2, 3) ~ "China",
        par_nat_1_w1 %in% c(4, 5, 7) ~ "SEA",
        par_nat_1_w1 == 6 ~ "Japan",
        TRUE ~ NA_character_
      ),
      levels = c("China", "Japan", "SEA", "Other")
    )
  )

# 1.2.7. Control Variables

# 1.2.7.1. S_AREA1 -> region
maps <- maps %>%
  mutate(
    region = factor(
      case_when(
        S_AREA1_w7 == 1 ~ "Seoul",
        S_AREA1_w7 == 2 ~ "Gyeongin",
        S_AREA1_w7 == 3 ~ "Chungcheong_Gangwon",
        S_AREA1_w7 == 4 ~ "Gyeongsang",
        S_AREA1_w7 == 5 ~ "Jeolla_Jeju",
        TRUE ~ NA_character_
      ),
      levels = c("Gyeongin", "Seoul", "Chungcheong_Gangwon","Gyeongsang", "Jeolla_Jeju")
    )
  )

# 1.2.7.2. S_AREA2 -> area_type
maps <- maps %>%
  mutate(
    area_type = factor(
      case_when(
        S_AREA2_w7 == 1 ~ "City",
        S_AREA2_w7 == 2 ~ "Town",
        S_AREA2_w7 == 3 ~ "Village",
        TRUE ~ NA_character_
      ),
      levels = c("Town", "City", "Village")
    )
  )

# 1.2.7.3. family_a01_w7 -> household_status
maps <- maps %>%
  mutate(
    household_status = factor(
      case_when(
        family_a01_w7 == 1 ~ "Together",
        family_a01_w7 %in% 2:4 ~ "Separated",
        family_a01_w7 == 5 ~ "Together",
        TRUE ~ NA_character_  # Should I treat this as separated?
      ),
      levels = c("Together", "Separated")
    )
  )

# 1.2.8. Remove unnecessary data & values
rm(fr_grid, W01, W02, W03, W04, W05, W06, W07, W08, W09, W10, W12,   # Data
   cols5, cols8, fr_cols, par_cols, vars_reconvert, vars_reconvert2, # Values
   ps_cols                                                           # Functions
   )

################################################################################

## 2. DATA ANALYSIS

# 2.0. Additional data preparation ---------------------------------------------

# 2.0.1. Subsetting DF for analysis
maps_analysis <- maps %>%
  dplyr::select(
    ID,
    uni_attendance,
    ba_or_not,
    income_equi_log,
    income_childhood_equi_log,
    par_education,
    par_sup_avg,
    fr_rel_avg,
    te_rel_avg,
    par_sup_emotional,
    par_sup_knowledge,
    par_sup_financial,
    soccap_par,
    soccap_fr,
    edu_asp,
    score_w8_avg,
    score_w5_avg,
    mom_origin,
    S_GENDER_w1,
    region,
    area_type,
    household_status
  )

# 2.0.2. Rescaling continuous variables
maps_analysis <- maps_analysis %>%
  mutate(across(where(is.numeric) & !ID,
                ~ arm::rescale(.x),
                .names = "z.{.col}")
         )

# 2.1. University Attendance ---------------------------------------------------

# 2.1.1. Model 1 (Base)
M1_1 <- glm(uni_attendance ~ z.income_equi_log + par_education +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M1_1)

# 2.1.2. Model 2 (Base + Social Capital)
M1_2 <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M1_2)

# 2.1.3. Model 3 (Base + Aspiration)
M1_3 <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.edu_asp +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M1_3)

# 2.1.4. Model 4 (Base + Score)
M1_4 <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.score_w8_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M1_4)

# 2.1.5. Model 5 (Base + Social Capital + Aspiration + Score)
M1_5 <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M1_5)

# 2.2. Bachelor's Degree -------------------------------------------------------

# 2.2.1. Model 1 (Base)
M2_1 <- glm(ba_or_not ~ z.income_equi_log + par_education +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M2_1)

# 2.2.2. Model 2 (Base + Social Capital)
M2_2 <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M2_2)

# 2.2.3. Model 3 (Base + Aspiration)
M2_3 <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.edu_asp +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M2_3)

# 2.2.4. Model 4 (Base + Score)
M2_4 <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.score_w8_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M2_4)

# 2.2.5. Model 5 (Base + Social Capital + Aspiration + Score)
M2_5 <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              S_GENDER_w1 + region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_analysis)
summary(M2_5)


################################################################################

## 3. FURTHER ANALYSIS

# 3.1. Gender differentials ----------------------------------------------------

# 3.1.1. Create subsets by gender
maps_boys <- subset(maps_analysis, S_GENDER_w1 == 1)
maps_girls <- subset(maps_analysis, S_GENDER_w1 == 2)

# 3.1.2. Boys

# 3.1.2.1. Boys: University
M1_b <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_boys)
summary(M1_b)

# 3.1.2.2. Boys: Bachelor's
M2_b <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_boys)
summary(M2_b)

# 3.1.3. Girls

# 3.1.3.1. Girls: University
M1_g <- glm(uni_attendance ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_girls)
summary(M1_g)

# 3.1.3.1. Girls: Bachelor's
M2_g <- glm(ba_or_not ~ z.income_equi_log + par_education +
              z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
              z.edu_asp + z.score_w8_avg +
              mom_origin +
              region + area_type + household_status,
            family = binomial(link = "logit"),
            data = maps_girls)
summary(M2_g)

# 3.2. Determinants of determinants --------------------------------------------

# 3.2.1. Educational Aspiration
M_eduasp <- lm(edu_asp ~ z.income_childhood_equi_log + par_education +
                 mom_origin +
                 S_GENDER_w1 + region + area_type + household_status,
               data = maps_analysis)
summary(M_eduasp)

# 3.2.2. Score
M_score <- lm(score_w8_avg ~ z.income_childhood_equi_log + par_education +
                mom_origin +
                S_GENDER_w1 + region + area_type + household_status,
              data = maps_analysis)
summary(M_score)

# 3.2.3. Parental Support
M_parsup <- lm(par_sup_avg ~ income_childhood_equi_log + par_education +
                 mom_origin +
                 S_GENDER_w1 + region + area_type + household_status,
               data = maps_analysis)
summary(M_parsup)

# 3.3. Is social capital insignificant? ----------------------------------------

# 3.3.1. Social capital during middle school

# 3.3.1.1. Uni attendance
M1_soccap <- glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.soccap_par + z.soccap_fr +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit"),
                 data = maps_analysis)
summary(M1_soccap)

# 3.3.1.2. Bachelor's degree
M2_soccap <- glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.soccap_par + z.soccap_fr +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit"),
                 data = maps_analysis)
summary(M2_soccap)

# 3.3.2. Parental financial support

# 3.3.2.1. Uni attendance
M1_finsup <- glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.par_sup_financial +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit"),
                 data = maps_analysis)
summary(M1_finsup)

# 3.3.2.2. Bachelor's degree
M2_finsup <- glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.par_sup_financial +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit"),
                 data = maps_analysis)
summary(M2_finsup)


################################################################################

## 4. REGRESSION TABLES --------------------------------------------------------
library(marginaleffects)
library(modelsummary)
library(sandwich)

# Shared settings
stars_codes <- c('*' = .05, '**' = .01, '***' = .001)
gof_glm <- list(list(raw = "nobs", clean = "N", fmt = 0))
gof_lm  <- list(list(raw = "nobs", clean = "N",  fmt = 0),
                list(raw = "r.squared", clean = "R2", fmt = 3))

# Helper: average comparisons with HC0-robust vcov
ac <- function(m) avg_comparisons(m, vcov = "HC0")

# Set variable order
var_order <- c(
  "z.income_equi_log", "z.income_childhood_equi_log", "par_education",
  "z.par_sup_avg", "z.fr_rel_avg", "z.te_rel_avg",
  "z.soccap_par", "z.soccap_fr", "z.par_sup_financial",
  "z.edu_asp", "z.score_w8_avg",
  "mom_origin", "S_GENDER_w1",
  "region", "area_type", "household_status"
)
cmap <- setNames(var_order, var_order)

# 4.1. Main analysis -----------------------------------------------------------

# 4.1.1. University attendance (block entry, Models 1-5)
modelsummary(
  list("(1)" = ac(M1_1), "(2)" = ac(M1_2), "(3)" = ac(M1_3),
       "(4)" = ac(M1_4), "(5)" = ac(M1_5)),
  shape   = term + contrast ~ model,
  coef_map = cmap,
  stars   = stars_codes,
  gof_map = gof_glm,
  output  = "output/2tab_main_uni.docx"
)

# 4.1.2. Bachelor's degree (block entry, Models 1-5)
modelsummary(
  list("(1)" = ac(M2_1), "(2)" = ac(M2_2), "(3)" = ac(M2_3),
       "(4)" = ac(M2_4), "(5)" = ac(M2_5)),
  shape   = term + contrast ~ model,
  coef_map = cmap,
  stars   = stars_codes,
  gof_map = gof_glm,
  output = "output/3tab_main_ba.docx"
)

# 4.2. Gender analysis (boys | girls, per threshold) ---------------------------

# 4.2.1. University attendance
modelsummary(
  list("Boys" = ac(M1_b), "Girls" = ac(M1_g)),
  shape   = term + contrast ~ model,
  coef_map = cmap,
  stars   = stars_codes,
  gof_map = gof_glm,
  output = "output/tab4_gender_uni.docx"
)

# 4.2.2. Bachelor's degree
modelsummary(
  list("Boys" = ac(M2_b), "Girls" = ac(M2_g)),
  shape   = term + contrast ~ model,
  coef_map = cmap,
  stars   = stars_codes,
  gof_map = gof_glm,
  output = "output/tab5_gender_ba.docx"
)

# 4.3. Determinants of determinants (OLS, separate tables, + R2) ---------------
modelsummary(list("Aspiration" = M_eduasp), vcov = "HC0",
             shape = term ~ model + statistic,
             stars = stars_codes,
             gof_map = gof_lm,
             output = "output/tab6_det_aspiration.docx")

modelsummary(list("Grades" = M_score), vcov = "HC0",
             shape = term ~ model + statistic,
             stars = stars_codes,
             gof_map = gof_lm,
             output = "output/tab7_det_score.docx")

modelsummary(list("Parental support" = M_parsup), vcov = "HC0",
             stars = stars_codes, gof_map = gof_lm,
             output = "output/tab_det_parsup.docx")

# 4.4. Social capital extras (uni | bachelor's, separate tables) ---------------

# 4.4.1. Middle-school social capital
modelsummary(
  list("University" = ac(M1_soccap), "Bachelor's" = ac(M2_soccap)),
  stars = stars_codes, gof_map = gof_glm,
  output = "output/tab_soccap_mid.docx"
)

# 4.4.2. Parental financial support
modelsummary(
  list("University" = ac(M1_finsup), "Bachelor's" = ac(M2_finsup)),
  stars = stars_codes, gof_map = gof_glm,
  output = "output/tab_soccap_finsup.docx"
)
