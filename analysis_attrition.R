#########################################################################################
#########################################################################################
# Title: Attrition Analysis
# By: Jisung Park
# For: MSc Inequalities and Social Sciences Dissertation
# Version: Online (Onedrive)
# Created: 07 July 2026
# Updated: 12 Aug 2026
#########################################################################################
#########################################################################################

# Load packages
library(tidyverse)
library(modelsummary)
library(sandwich)

# Load data
W01 <- read.csv('data/raw/w1_c.csv')
W10 <- read.csv('data/raw/w10_c.csv')

# Merge data
attrition <- list(W01, W10) %>%
  reduce(left_join)

# Convert negative values to NA
attrition <- attrition %>%
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(.x < 0, NA, .x)
    )
  )

# Fix coding error
attrition <- attrition %>%
  mutate(income_01_w1 = ifelse(income_01_w1 > 1800, NA_integer_, income_01_w1))

# Convert all variables < 14 to factors
attrition <- attrition %>%
  mutate(
    across(
      where(~ (is.integer(.x) | is.numeric(.x)) & dplyr::n_distinct(.x) < 14),
      ~ as.factor(.x)
    )
  )

# Create dropout variable
attrition$dropout <- factor(ifelse(attrition$SURVEY1_w10 == 1, 0, 1),
                            levels = c(0, 1))

# Sanity check
levels(attrition$dropout)
summary(attrition$dropout)

# Household income
attrition$income_equi <- attrition$income_01_w1 / sqrt(as.numeric(attrition$household_w1))

# Parental education variable
attrition$par_education <- factor(ifelse(attrition$par_edu_1_w1 %in% 1:2 & 
                                           attrition$par_edu_2_w1 %in% 1:2, "No",
                                    ifelse(attrition$par_edu_1_w1 %in% 3:5 | 
                                             attrition$par_edu_2_w1 %in% 3:5, "Yes",
                                           NA)))

# Mother's country of origin
attrition <- attrition %>%
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

# Household status
attrition <- attrition %>%
  mutate(
    household_status = factor(
      case_when(
        family_a01_w1 == 1 ~ "Together",
        family_a01_w1 %in% 2:4 ~ "Separated",
        family_a01_w1 == 5 ~ "Together",
        TRUE ~ NA_character_
      ),
      levels = c("Together", "Separated")
    )
  )

# 1. Compare means (descriptive)
tapply(attrition$income_01_w1, attrition$dropout, mean, na.rm = TRUE)

# 1.1. Compare means (t-test)
t.test(income_01_w1 ~ dropout, data = attrition)

# 2. Compare proportions (descriptive)
prop.table(table(attrition$dropout, attrition$par_edu_2_w1))

# 2.1. Compare proportions (Chi-square)
chisq.test(table(attrition$dropout, attrition$S_AREA1_w1))

# 3. Logistic regression
M_att <- glm(dropout ~ income_equi + par_education + 
               mom_origin +
               S_GENDER_w1 + S_AREA1_w1 + household_status,
             family = binomial(link = "logit"),
             data = attrition)
summary(M_att)

# 4. Print model
cm <- c(
  "income_equi"               = "Equivalised income",
  "par_educationYes"          = "Attended university",
  "mom_originJapan"           = "Japan",
  "mom_originSEA"             = "Southeast Asia",
  "mom_originOther"           = "Other",
  "S_GENDER_w12"              = "Female",
  "S_AREA1_w12"               = "Gyeongin",
  "S_AREA1_w13"               = "Chungcheong & Gangwon",
  "S_AREA1_w14"               = "Gyeongsang",
  "S_AREA1_w15"               = "Jeolla & Jeju",
  "household_statusSeparated" = "Separated"
)

modelsummary(
  list("Attrition" = M_att),
  exponentiate = TRUE,
  vcov         = "HC0",
  stars        = c('*' = .05, '**' = .01, '***' = .001),
  coef_map     = cm,
  gof_map      = list(list(raw = "nobs", clean = "N", fmt = 0)),
  shape        = term ~ model + statistic,   # SE in its own column, right of the OR
  output       = "output/attrition_table.docx"
)
