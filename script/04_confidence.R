################################################################################
############                   ANALYSIS DATASET                      ############
################################################################################

my_dataset <- submersion %>%
  select(CNJ201, CNJ202, CNJ203, CNJ204, confidence_group, confidence, all_correct, 
         confidence_competence, confidence_knowledge, gender, generation, education, 
         race_group, hispanic, region_group, number_children, children_group, 
         parent_status, pid3_model, yes_social_media, watched_tv_news, read_newspaper, 
         listened_radio_news, none_media, posted_political_content, commented_politics, 
         read_watched_political, followed_political_event, forwarded_political_content, 
         none_political_social_media, social_media_engagement, active_social_media, passive_social_media,
         income_group, home_ownership, employment_group, information_status)

summary(my_dataset)


################################################################################
############                  DEMOGRAPHIC ANALYSIS                   ############
################################################################################

############                  Gender                     ############

# Chi-squared test

table(submersion$gender, submersion$information_status)

chisq.test(
  table(submersion$gender, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$gender, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


############                  Generation                     ############

# Chi-squared test

table(submersion$generation, submersion$information_status)

chisq.test(
  table(submersion$generation, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$generation, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


############                  Education                     ############

# Chi-squared test

table(submersion$education, submersion$information_status)

chisq.test(
  table(submersion$education, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$education, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


############                  Race                     ############

# Chi-squared test

table(submersion$race_group, submersion$information_status)

chisq.test(
  table(submersion$race_group, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$race_group, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


############                  Number of children                     ############

# Chi-squared test

table(submersion$children_group, submersion$information_status)

chisq.test(
  table(submersion$children_group, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$children_group, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


############                  Parental status                    ############

# Chi-squared test

table(submersion$parent_status, submersion$information_status)

chisq.test(
  table(submersion$parent_status, submersion$information_status)
)

# Row percentages

round(
  prop.table(
    table(submersion$parent_status, submersion$information_status),
    margin = 1
  ) * 100,
  1
)


################################################################################
############                    HIGH-RISK GROUP                      ############
################################################################################

# High-risk respondents are confident but answered at least one
# of the three knowledge questions incorrectly.
# 1 = Misinformed
# 0 = All other nonmissing categories

submersion <- submersion %>%
  mutate(
    high_risk = case_when(
      information_status == "Misinformed" ~ 1,
      is.na(information_status) ~ NA_real_,
      TRUE ~ 0
    )
  )

# Confirm creation
table(submersion$high_risk, useNA = "ifany")
table(submersion$information_status, submersion$high_risk, useNA = "ifany")

################################################################################
############       TABLE 1: DEMOGRAPHIC CHARACTERISTICS              ############
################################################################################

# Table 1 uses the original education categories rather than the
# three-category education variable used in later analyses.


submersion <- submersion %>%
  mutate(
    education_detail = case_when(
      educ == "No HS" ~ "No high school",
      educ == "High school graduate" ~ "High school graduate",
      educ == "Some college" ~ "Some college",
      educ == "2-year" ~ "2-year degree",
      educ == "4-year" ~ "4-year degree",
      educ == "Post-grad" ~ "Post-graduate degree",
      TRUE ~ NA_character_
    )
  )

round(
  prop.table(
    table(submersion$education_detail)
  ) * 100,
  1
)

############                  Gender                     ############

table(submersion$gender, useNA = "ifany")

round(
  prop.table(
    table(submersion$gender)
  ) * 100,
  1
)


############                  Race                       ############

table(submersion$race_group, useNA = "ifany")

round(
  prop.table(
    table(submersion$race_group)
  ) * 100,
  1
)


############                  Education                  ############

table(submersion$education_detail, useNA = "ifany")

round(
  prop.table(
    table(submersion$education_detail)
  ) * 100,
  1
)

################################################################################
############       TABLE 2: CONFIDENCE BY DEMOGRAPHICS                ############
################################################################################

# For this table:
# High = Confident
# Low = Not confident
#
# Respondents who answered "Unsure" on the original confidence question
# are included in the Not confident category because that is how the
# binary confidence variable was created.

############                  Gender                     ############

# Remove "Other" gender identities from this table because of
# the small cell size.

gender_confidence <- submersion %>%
  filter(
    gender %in% c("Man", "Woman"),
    !is.na(confidence)
  )

# Counts
table(
  gender_confidence$gender,
  gender_confidence$confidence
)

# Row percentages
round(
  prop.table(
    table(
      gender_confidence$gender,
      gender_confidence$confidence
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each gender group
table(gender_confidence$gender)

# Chi-squared test
chisq.test(
  table(
    gender_confidence$gender,
    gender_confidence$confidence
  )
)


############                  Race                       ############

race_confidence <- submersion %>%
  filter(
    !is.na(race_group),
    !is.na(confidence)
  )

# Counts
table(
  race_confidence$race_group,
  race_confidence$confidence
)

# Row percentages
round(
  prop.table(
    table(
      race_confidence$race_group,
      race_confidence$confidence
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each race group
table(race_confidence$race_group)

# Chi-squared test
chisq.test(
  table(
    race_confidence$race_group,
    race_confidence$confidence
  )
)


############                  Education                  ############

education_confidence <- submersion %>%
  filter(
    !is.na(education),
    !is.na(confidence)
  )

# Counts
table(
  education_confidence$education,
  education_confidence$confidence
)

# Row percentages
round(
  prop.table(
    table(
      education_confidence$education,
      education_confidence$confidence
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each education group
table(education_confidence$education)

# Chi-squared test
chisq.test(
  table(
    education_confidence$education,
    education_confidence$confidence
  )
)

################################################################################
############       TABLE 3: OBJECTIVE COMPETENCE                      ############
################################################################################

# These tables show how many respondents selected each answer
# and the percentage selecting each answer.


############                  Question 1                  ############
# Safest escape method

table(submersion$plan_escape, useNA = "ifany")

round(
  prop.table(
    table(submersion$plan_escape)
  ) * 100,
  1
)


############                  Question 2                  ############
# Time available to escape

table(submersion$time_escape, useNA = "ifany")

round(
  prop.table(
    table(submersion$time_escape)
  ) * 100,
  1
)


############                  Question 3                  ############
# Who should be helped first

table(submersion$who_save, useNA = "ifany")

round(
  prop.table(
    table(submersion$who_save)
  ) * 100,
  1
)


############          Overall Correct Answers             ############

# These are useful checks for the competence section.

table(submersion$correct_CNJ202, useNA = "ifany")
round(prop.table(table(submersion$correct_CNJ202)) * 100, 1)

table(submersion$correct_CNJ203, useNA = "ifany")
round(prop.table(table(submersion$correct_CNJ203)) * 100, 1)

table(submersion$correct_CNJ204, useNA = "ifany")
round(prop.table(table(submersion$correct_CNJ204)) * 100, 1)

# Percentage who answered all three correctly

table(submersion$all_correct, useNA = "ifany")

round(
  prop.table(
    table(submersion$all_correct)
  ) * 100,
  1
)

################################################################################
############       TABLE 4: INFORMATION STATUS BY DEMOGRAPHICS         ############
################################################################################

# Information status:
#
# Informed =
#   confident and all three knowledge questions correct
#
# Misinformed =
#   confident and at least one knowledge question incorrect
#
# Underinformed =
#   not confident and all three knowledge questions correct
#
# Uninformed =
#   not confident and at least one knowledge question incorrect
#
# Respondents in the "Other" gender category are excluded from
# Table 4 because of the small sample size.
#
# The "Other" RACE category is still included.


################################################################################
############                 TABLE 4 SAMPLE                           ############
################################################################################

# Create the sample used for all parts of Table 4.
#
# Keep only respondents coded as Man or Woman
# and respondents with a valid information status.

table4_data <- submersion %>%
  filter(
    gender %in% c("Man", "Woman"),
    !is.na(information_status)
  )

# Confirm sample size

nrow(table4_data)

# Check gender counts

table(table4_data$gender, useNA = "ifany")


################################################################################
############             OVERALL INFORMATION STATUS                  ############
################################################################################

# Overall counts

table(table4_data$information_status, useNA = "ifany")

# Overall percentages

round(
  prop.table(
    table(table4_data$information_status)
  ) * 100,
  1
)


################################################################################
############                       GENDER                             ############
################################################################################

# Counts

table(
  table4_data$gender,
  table4_data$information_status
)

# Row percentages

round(
  prop.table(
    table(
      table4_data$gender,
      table4_data$information_status
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each gender group

table(table4_data$gender)

# Chi-squared test

gender_status_chisq <- chisq.test(
  table(
    table4_data$gender,
    table4_data$information_status
  )
)

gender_status_chisq

# Check expected cell counts

gender_status_chisq$expected


################################################################################
############                         RACE                             ############
################################################################################

# Remove only respondents missing race.
# Keep the "Other" race category.

race_status <- table4_data %>%
  filter(
    !is.na(race_group)
  )

# Counts

table(
  race_status$race_group,
  race_status$information_status
)

# Row percentages

round(
  prop.table(
    table(
      race_status$race_group,
      race_status$information_status
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each race group

table(race_status$race_group)

# Chi-squared test

race_status_chisq <- chisq.test(
  table(
    race_status$race_group,
    race_status$information_status
  )
)

race_status_chisq

# Check expected cell counts

race_status_chisq$expected


################################################################################
############                      EDUCATION                           ############
################################################################################

# Remove only respondents missing education.

education_status <- table4_data %>%
  filter(
    !is.na(education)
  )

# Counts

table(
  education_status$education,
  education_status$information_status
)

# Row percentages

round(
  prop.table(
    table(
      education_status$education,
      education_status$information_status
    ),
    margin = 1
  ) * 100,
  1
)

# Total n in each education group

table(education_status$education)

# Chi-squared test

education_status_chisq <- chisq.test(
  table(
    education_status$education,
    education_status$information_status
  )
)

education_status_chisq

# Check expected cell counts

education_status_chisq$expected
