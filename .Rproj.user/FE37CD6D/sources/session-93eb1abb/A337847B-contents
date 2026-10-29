############                  COMPLETE CASES                       ############

# This script creates the complete-cases datasets used in the analyses

# A complete case is a respondent with nonmissing values for all variables 
# required for the analyses

# Complete-case samples may differ across analyses because different models use
# different sets of variables

# New datasets are created for each complete-case analysis-case sample

# General process:
# STEP 1: Select the variables needed for the analysis
# STEP 2: Remove respondents missing any required variable
# STEP 3: Confirm the resulting sample size
# STEP 4: Check which observations were excluded

################################################################################
############            DEMOGRAPHIC ANALYSIS SAMPLES                 ############
################################################################################

# These samples are used to examine the relationship between
# demographic characteristics and information status.
#
# Missing values are removed only for the two variables being
# examined in each analysis.
#
# All gender categories are retained in this section.


############                  Gender                     ############

# STEP 1: Examine missing values

table(submersion$gender, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

gender_status <- submersion %>%
  filter(
    !is.na(gender),
    !is.na(information_status)
  ) %>%
  select(gender, information_status)


# STEP 3: Confirm sample

nrow(gender_status)

table(gender_status$gender, useNA = "ifany")
table(gender_status$information_status, useNA = "ifany")


############                  Generation                     ############

# STEP 1: Examine missing values

table(submersion$generation, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

generation_status <- submersion %>%
  filter(
    !is.na(generation),
    !is.na(information_status)
  ) %>%
  select(generation, information_status)


# STEP 3: Confirm sample

nrow(generation_status)

table(generation_status$generation, useNA = "ifany")
table(generation_status$information_status, useNA = "ifany")


############                  Education                     ############

# STEP 1: Examine missing values

table(submersion$education, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

education_status_demographic <- submersion %>%
  filter(
    !is.na(education),
    !is.na(information_status)
  ) %>%
  select(education, information_status)


# STEP 3: Confirm sample

nrow(education_status_demographic)

table(education_status_demographic$education, useNA = "ifany")
table(education_status_demographic$information_status, useNA = "ifany")


############                  Race                     ############

# STEP 1: Examine missing values

table(submersion$race_group, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

race_status_demographic <- submersion %>%
  filter(
    !is.na(race_group),
    !is.na(information_status)
  ) %>%
  select(race_group, information_status)


# STEP 3: Confirm sample

nrow(race_status_demographic)

table(race_status_demographic$race_group, useNA = "ifany")
table(race_status_demographic$information_status, useNA = "ifany")


############                  Number of Children                     ############

# STEP 1: Examine missing values

table(submersion$children_group, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

children_status <- submersion %>%
  filter(
    !is.na(children_group),
    !is.na(information_status)
  ) %>%
  select(children_group, information_status)


# STEP 3: Confirm sample

nrow(children_status)

table(children_status$children_group, useNA = "ifany")
table(children_status$information_status, useNA = "ifany")


############                  Parental Status                     ############

# STEP 1: Examine missing values

table(submersion$parent_status, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

parent_status_analysis <- submersion %>%
  filter(
    !is.na(parent_status),
    !is.na(information_status)
  ) %>%
  select(parent_status, information_status)


# STEP 3: Confirm sample

nrow(parent_status_analysis)

table(parent_status_analysis$parent_status, useNA = "ifany")
table(parent_status_analysis$information_status, useNA = "ifany")


################################################################################
############       TABLE 2: CONFIDENCE BY DEMOGRAPHICS                ############
################################################################################

# These samples are used to examine confidence by gender, race,
# and education.
#
# Each sample requires a valid confidence response and a valid
# response for the demographic variable being examined.


############                  Gender                     ############

# STEP 1: Examine missing values

table(submersion$gender, useNA = "ifany")
table(submersion$confidence, useNA = "ifany")


# STEP 2: Create sample
#
# Keep only Man and Woman because the original analysis excludes
# the Other gender category due to its small sample size.

gender_confidence <- submersion %>%
  filter(
    gender %in% c("Man", "Woman"),
    !is.na(confidence)
  ) %>%
  select(gender, confidence)


# STEP 3: Confirm sample

nrow(gender_confidence)

table(gender_confidence$gender, useNA = "ifany")
table(gender_confidence$confidence, useNA = "ifany")


############                  Race                     ############

# STEP 1: Examine missing values

table(submersion$race_group, useNA = "ifany")
table(submersion$confidence, useNA = "ifany")


# STEP 2: Create sample

race_confidence <- submersion %>%
  filter(
    !is.na(race_group),
    !is.na(confidence)
  ) %>%
  select(race_group, confidence)


# STEP 3: Confirm sample

nrow(race_confidence)

table(race_confidence$race_group, useNA = "ifany")
table(race_confidence$confidence, useNA = "ifany")


############                  Education                     ############

# STEP 1: Examine missing values

table(submersion$education, useNA = "ifany")
table(submersion$confidence, useNA = "ifany")


# STEP 2: Create sample

education_confidence <- submersion %>%
  filter(
    !is.na(education),
    !is.na(confidence)
  ) %>%
  select(education, confidence)


# STEP 3: Confirm sample

nrow(education_confidence)

table(education_confidence$education, useNA = "ifany")
table(education_confidence$confidence, useNA = "ifany")


################################################################################
############       TABLE 4: INFORMATION STATUS BY DEMOGRAPHICS         ############
################################################################################

# Table 4 excludes respondents in the Other gender category due to
# the small sample size.
#
# It requires a valid gender and a valid information status.
#
# The Other race category is retained in the race analysis.
#
# Race and education samples are created separately because some
# respondents may be missing those particular demographic variables.


############                 TABLE 4 SAMPLE                           ############

# STEP 1: Examine the variables

table(submersion$gender, useNA = "ifany")
table(submersion$information_status, useNA = "ifany")


# STEP 2: Create sample

table4_data <- submersion %>%
  filter(
    gender %in% c("Man", "Woman"),
    !is.na(information_status)
  ) %>%
  select(gender, information_status, race_group, education)


# STEP 3: Confirm sample

nrow(table4_data)

table(table4_data$gender, useNA = "ifany")
table(table4_data$information_status, useNA = "ifany")


############                       RACE                             ############

# STEP 1: Examine missing values

table(table4_data$race_group, useNA = "ifany")


# STEP 2: Create sample
#
# Start with the Table 4 sample and remove respondents missing race.

race_status <- table4_data %>%
  filter(
    !is.na(race_group)
  ) %>%
  select(race_group, information_status)


# STEP 3: Confirm sample

nrow(race_status)

table(race_status$race_group, useNA = "ifany")
table(race_status$information_status, useNA = "ifany")


############                      EDUCATION                           ############

# STEP 1: Examine missing values

table(table4_data$education, useNA = "ifany")


# STEP 2: Create sample
#
# Start with the Table 4 sample and remove respondents missing education.

education_status <- table4_data %>%
  filter(
    !is.na(education)
  ) %>%
  select(education, information_status)


# STEP 3: Confirm sample

nrow(education_status)

table(education_status$education, useNA = "ifany")
table(education_status$information_status, useNA = "ifany")


################################################################################
############                FINAL SAMPLE SIZE CHECK                  ############
################################################################################

# Compare the number of respondents in the original dataset
# with the number included in each analysis sample.

nrow(submersion)

nrow(gender_status)
nrow(generation_status)
nrow(education_status_demographic)
nrow(race_status_demographic)
nrow(children_status)
nrow(parent_status_analysis)

nrow(gender_confidence)
nrow(race_confidence)
nrow(education_confidence)

nrow(table4_data)
nrow(race_status)
nrow(education_status)