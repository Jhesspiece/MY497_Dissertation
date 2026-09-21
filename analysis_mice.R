################################################################################
################################################################################
# Title: Quantitative Analysis Using the MAPS Database
# For: Adding multiple imputation into original pipeline
# Created: 1 Sep 2026
################################################################################
################################################################################

## 0. SETTING UP ---------------------------------------------------------------

# 0.1. Load relevant packages
library(tidyverse)
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

# 1.2.5.2.2. Grades (high school)
cols8 <- c("score_01_w8","score_02_w8","score_03_w8","score_04_w8","score_05_w8")

maps <- maps %>%
  mutate(
    score_w8_avg = rowMeans(across(all_of(cols8)), na.rm = TRUE),
    score_w8_avg = ifelse(is.nan(score_w8_avg), NA, score_w8_avg)
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
        TRUE ~ NA_character_
      ),
      levels = c("Together", "Separated")
    )
  )


################################################################################

## 1.5. MULTIPLE IMPUTATION (for item missingness)
library(mice)
library(miceadds)

# 1.5.1. Natural-scale variables to impute
imp_vars <- maps %>%
  dplyr::select(
    uni_attendance, ba_or_not,                                   # outcomes
    income_equi_log, income_childhood_equi_log, par_education,   # SES
    par_sup_avg, fr_rel_avg, te_rel_avg,                         # social capital
    edu_asp, score_w8_avg,                                       # mediators
    mom_origin, S_GENDER_w1, region, area_type, household_status # gender/controls
  )

# 1.5.2. Dry run to grab and edit method + predictor matrix
ini  <- mice(imp_vars, maxit = 0)
meth <- ini$method
pred <- ini$predictorMatrix

# 1.5.3. Impute
imp <- mice(imp_vars, m = 20, method = meth, predictorMatrix = pred,
            seed = 12345, printFlag = FALSE)

# 1.5.4. Complete → standardise (2SD, full-sample) within EACH dataset
cont <- c("income_equi_log","income_childhood_equi_log",
          "par_sup_avg","fr_rel_avg","te_rel_avg",
          "edu_asp","score_w8_avg")

imp_list <- mice::complete(imp, "all")
imp_list <- lapply(imp_list, function(d) {
  d[paste0("z.", cont)] <- lapply(d[cont], arm::rescale)
  d
})
imp_mids <- miceadds::datlist2mids(imp_list)


################################################################################

## 2. DATA ANALYSIS  (multiply imputed)

# 2.1. University Attendance ---------------------------------------------------

# 2.1.1. Model 1 (Base)
M1_1 <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M1_1))

# 2.1.2. Model 2 (Base + Social Capital)
M1_2 <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M1_2))

# 2.1.3. Model 3 (Base + Aspiration)
M1_3 <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.edu_asp +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M1_3))

# 2.1.4. Model 4 (Base + Score)
M1_4 <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.score_w8_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M1_4))

# 2.1.5. Model 5 (Base + Social Capital + Aspiration + Score)
M1_5 <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M1_5))

# 2.2. Bachelor's Degree -------------------------------------------------------

# 2.2.1. Model 1 (Base)
M2_1 <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M2_1))

# 2.2.2. Model 2 (Base + Social Capital)
M2_2 <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M2_2))

# 2.2.3. Model 3 (Base + Aspiration)
M2_3 <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.edu_asp +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M2_3))

# 2.2.4. Model 4 (Base + Score)
M2_4 <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.score_w8_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M2_4))

# 2.2.5. Model 5 (Base + Social Capital + Aspiration + Score)
M2_5 <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   S_GENDER_w1 + region + area_type + household_status,
                 family = binomial(link = "logit")))
summary(pool(M2_5))


################################################################################

## 3. FURTHER ANALYSIS  (multiply imputed)

# 3.1. Gender differentials ----------------------------------------------------

# 3.1.2. Boys

# 3.1.2.1. Boys: University
M1_b <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   region + area_type + household_status,
                 family = binomial(link = "logit"),
                 subset = S_GENDER_w1 == 1))
summary(pool(M1_b))

# 3.1.2.2. Boys: Bachelor's
M2_b <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   region + area_type + household_status,
                 family = binomial(link = "logit"),
                 subset = S_GENDER_w1 == 1))
summary(pool(M2_b))

# 3.1.3. Girls

# 3.1.3.1. Girls: University
M1_g <- with(imp_mids,
             glm(uni_attendance ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   region + area_type + household_status,
                 family = binomial(link = "logit"),
                 subset = S_GENDER_w1 == 2))
summary(pool(M1_g))

# 3.1.3.2. Girls: Bachelor's
M2_g <- with(imp_mids,
             glm(ba_or_not ~ z.income_equi_log + par_education +
                   z.par_sup_avg + z.fr_rel_avg + z.te_rel_avg +
                   z.edu_asp + z.score_w8_avg +
                   mom_origin +
                   region + area_type + household_status,
                 family = binomial(link = "logit"),
                 subset = S_GENDER_w1 == 2))
summary(pool(M2_g))

# 3.2. Determinants of determinants --------------------------------------------

# 3.2.1. Educational Aspiration
M_eduasp <- with(imp_mids,
                 lm(edu_asp ~ z.income_childhood_equi_log + par_education +
                      mom_origin +
                      S_GENDER_w1 + region + area_type + household_status))
summary(pool(M_eduasp))
pool.r.squared(M_eduasp)      # pooled R2 (not returned by pool())

# 3.2.2. Score
M_score <- with(imp_mids,
                lm(score_w8_avg ~ z.income_childhood_equi_log + par_education +
                     mom_origin +
                     S_GENDER_w1 + region + area_type + household_status))
summary(pool(M_score))
pool.r.squared(M_score)


################################################################################

## 4. REGRESSION TABLES --------------------------------------------------------
library(marginaleffects)
library(modelsummary)

# Shared settings
stars_codes <- c('*' = .05, '**' = .01, '***' = .001)
gof_glm <- list(list(raw = "nobs", clean = "N", fmt = 0))
ac <- function(m) avg_comparisons(m)

# Set variable order
var_order <- c(
  "z.income_equi_log", "z.income_childhood_equi_log", "par_education",
  "z.par_sup_avg", "z.fr_rel_avg", "z.te_rel_avg",
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
  output  = "output/tab2_main_uni.docx"
)

# 4.1.2. Bachelor's degree (block entry, Models 1-5)
modelsummary(
  list("(1)" = ac(M2_1), "(2)" = ac(M2_2), "(3)" = ac(M2_3),
       "(4)" = ac(M2_4), "(5)" = ac(M2_5)),
  shape   = term + contrast ~ model,
  coef_map = cmap,
  stars   = stars_codes,
  gof_map = gof_glm,
  output = "output/tab3_main_ba.docx"
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
r2_row <- function(mira_obj) {
  r2 <- mice::pool.r.squared(mira_obj)[1, "est"]
  data.frame(term = "R2", est = sprintf("%.3f", r2), se = "")
}

modelsummary(list("Aspiration" = pool(M_eduasp)),
             shape    = term ~ model + statistic,
             stars    = stars_codes,
             gof_map  = list(list(raw = "nobs", clean = "N", fmt = 0)),
             add_rows = r2_row(M_eduasp),
             output   = "output/tab6_det_aspiration.docx")

modelsummary(list("Grades" = pool(M_score)),
             shape    = term ~ model + statistic,
             stars    = stars_codes,
             gof_map  = list(list(raw = "nobs", clean = "N", fmt = 0)),
             add_rows = r2_row(M_score),
             output   = "output/tab7_det_score.docx")
