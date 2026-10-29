####################################################################################
############              PHASE 1: CLEAN DATA FOR ANALYSIS              ############
####################################################################################

# This script is used to clean and recode variables for analysis
# Original variables are examined first, then cleaned versions are created

# Cleaning process: 
# STEP 1: Examine the original variable and coding schema
# STEP 2: Create a new cleaned/re-coded variable
# STEP 3: Confirm that the new variable was created correctly

# Missing values:
# Missing or unusable responses are coded as NA where appropriate.

# This script focuses only on data cleaning and variable creation
# Descriptive statistics, graphs, and regression analyses are completed in 
# separate scripts.

############                       Confidence                  ############

# STEP 1: Examine variable and coding schema

table(submersion$CNJ201, useNA = "ifany")

# STEP 2: Create three-category confidence variable

submersion <- submersion %>%
  mutate(
    confidence_group = case_when(
      CNJ201 %in% c(1, 2) ~ "Not confident",
      CNJ201 == 3 ~ "Unsure",
      CNJ201 %in% c(4, 5) ~ "Confident",
      TRUE ~ NA_character_
    ),
    
    confidence = case_when(
      CNJ201 %in% c(4, 5) ~ "Confident",
      CNJ201 %in% c(1, 2, 3) ~ "Not confident",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$confidence_group, useNA = "ifany")
table(submersion$CNJ201, submersion$confidence_group, useNA = "ifany")

table(submersion$confidence, useNA = "ifany")
table(submersion$CNJ201, submersion$confidence, useNA = "ifany")

############                       Knowledge/Competence                  ############

# STEP 1: Look at the original responses to the three knowledge questions

table(submersion$CNJ202, useNA = "ifany")
table(submersion$CNJ203, useNA = "ifany")
table(submersion$CNJ204, useNA = "ifany")


# STEP 2: Create readable versions of each response

submersion <- submersion %>%
  mutate(
    plan_escape = case_when(
      CNJ202 == 1 ~ "Window ASAP",
      CNJ202 == 2 ~ "Door ASAP",
      CNJ202 == 3 ~ "Window wait",
      CNJ202 == 4 ~ "Door wait",
      CNJ202 == 9 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    
    time_escape = case_when(
      CNJ203 == 1 ~ "Less than 1 minute",
      CNJ203 == 2 ~ "1 minute",
      CNJ203 == 3 ~ "2-5 minutes",
      CNJ203 == 4 ~ "6-10 minutes",
      CNJ203 == 5 ~ "More than 10 minutes",
      CNJ203 == 9 ~ "Don't know",
      TRUE ~ NA_character_
    ),
    
    who_save = case_when(
      CNJ204 == 1 ~ "Adult first",
      CNJ204 == 2 ~ "Child first",
      CNJ204 == 9 ~ "Don't know",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$plan_escape, useNA = "ifany")
table(submersion$CNJ202, submersion$plan_escape, useNA = "ifany")

table(submersion$time_escape, useNA = "ifany")
table(submersion$CNJ203, submersion$time_escape, useNA = "ifany")

table(submersion$who_save, useNA = "ifany")
table(submersion$CNJ204, submersion$who_save, useNA = "ifany")

############                       Correct Answers                 ############

# Correct answers:
# CNJ202 = Window ASAP
# CNJ203 = Less than 1 minute OR 1 minute
# CNJ204 = Child first

submersion <- submersion %>%
  mutate(
    correct_CNJ202 = case_when(
      CNJ202 == 1 ~ 1,
      is.na(CNJ202) ~ NA_real_,
      TRUE ~ 0
    ),
    
    correct_CNJ203 = case_when(
      CNJ203 %in% c(1, 2) ~ 1,
      is.na(CNJ203) ~ NA_real_,
      TRUE ~ 0
    ),
    
    correct_CNJ204 = case_when(
      CNJ204 == 2 ~ 1,
      is.na(CNJ204) ~ NA_real_,
      TRUE ~ 0
    )
  )

# A respondent is coded as all_correct = 1 only if all three
# knowledge questions were answered correctly.

submersion <- submersion %>%
  mutate(
    all_correct = case_when(
      
      is.na(correct_CNJ202) |
        is.na(correct_CNJ203) |
        is.na(correct_CNJ204) ~ NA_real_,
      
      correct_CNJ202 == 1 &
        correct_CNJ203 == 1 &
        correct_CNJ204 == 1 ~ 1,
      
      TRUE ~ 0
    )
  )

# Confirm creation

table(submersion$correct_CNJ202, useNA = "ifany")
table(submersion$CNJ202, submersion$correct_CNJ202, useNA = "ifany")

table(submersion$correct_CNJ203, useNA = "ifany")
table(submersion$CNJ203, submersion$correct_CNJ203, useNA = "ifany")

table(submersion$correct_CNJ204, useNA = "ifany")
table(submersion$CNJ204, submersion$correct_CNJ204, useNA = "ifany")

table(submersion$all_correct, useNA = "ifany")

table(submersion$correct_CNJ202, submersion$all_correct, useNA = "ifany")
table(submersion$correct_CNJ203, submersion$all_correct, useNA = "ifany")
table(submersion$correct_CNJ204, submersion$all_correct, useNA = "ifany")

############                       Confidence + Knowledge Typology                 ############

# Six-category version keeps "Unsure" separate.

submersion <- submersion %>%
  mutate(
    confidence_competence = case_when(
      is.na(confidence_group) | is.na(all_correct) ~ NA_character_,
      
      confidence_group == "Not confident" &
        all_correct == 0 ~ "Not confident and incorrect",
      
      confidence_group == "Not confident" &
        all_correct == 1 ~ "Not confident but correct",
      
      confidence_group == "Unsure" &
        all_correct == 0 ~ "Unsure and incorrect",
      
      confidence_group == "Unsure" &
        all_correct == 1 ~ "Unsure but correct",
      
      confidence_group == "Confident" &
        all_correct == 0 ~ "Confident but incorrect",
      
      confidence_group == "Confident" &
        all_correct == 1 ~ "Confident and correct"
    )
  )

# Confirm creation

table(submersion$confidence_competence, useNA = "ifany")

# Four-category version combines "Unsure" with the not-confident group.
# This is the version used for the main confidence/competence analysis.

submersion <- submersion %>%
  mutate(
    confidence_knowledge = case_when(
      is.na(confidence) | is.na(all_correct) ~ NA_character_,
      
      confidence == "Confident" &
        all_correct == 1 ~ "Confident and correct",
      
      confidence == "Confident" &
        all_correct == 0 ~ "Confident but incorrect",
      
      confidence == "Not confident" &
        all_correct == 1 ~ "Not confident but correct",
      
      confidence == "Not confident" &
        all_correct == 0 ~ "Not confident and incorrect"
    )
  )


############              Misinformed/Uninformed/Underinformed/Informed STATUS   ############


# Classify respondents based on their confidence and knowledge.
# confidence = whether the respondent is confident
# all_correct = whether all three knowledge questions were answered correctly

submersion <- submersion %>%
  mutate(
    information_status = case_when(
      is.na(confidence) | is.na(all_correct) ~ NA_character_,
      confidence == "Confident" & all_correct == 1 ~ "Informed",
      confidence == "Confident" & all_correct == 0 ~ "Misinformed",
      confidence == "Not confident" & all_correct == 1 ~ "Underinformed",
      confidence == "Not confident" & all_correct == 0 ~ "Uninformed"
    )
  )


# Confirm creation

table(submersion$information_status, useNA = "ifany")
table(submersion$confidence, submersion$information_status, useNA = "ifany")
table(submersion$all_correct, submersion$information_status, useNA = "ifany")



############                   DEMOGRAPHICS                        ############


############                   Gender                        ############

# STEP 1: Examine variable and coding schema

table(submersion$gender4, useNA = "ifany")

# STEP 2: Create gender variables 

submersion <- submersion %>%
  mutate(
    gender = case_when(
      gender4 == "Man" ~ "Man",
      gender4 == "Woman" ~ "Woman",
      gender4 %in% c("Non-binary", "Other") ~ "Other",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$gender, useNA = "ifany")
table(submersion$gender4, submersion$gender, useNA = "ifany")

############                   Generation                        ############

# STEP 1: Examine variable and coding schema

table(submersion$birthyr, useNA = "ifany")

# STEP 2: Create generation variables 

submersion <- submersion %>%
  mutate(
    generation = case_when(
      birthyr < 1928 ~ "Before Silent Generation",
      birthyr >= 1928 & birthyr <= 1945 ~ "Silent Generation",
      birthyr >= 1946 & birthyr <= 1964 ~ "Baby Boomer",
      birthyr >= 1965 & birthyr <= 1980 ~ "Gen X",
      birthyr >= 1981 & birthyr <= 1996 ~ "Millennial",
      birthyr >= 1997 & birthyr <= 2012 ~ "Gen Z",
      birthyr >= 2013 ~ "Gen Alpha",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$generation, useNA = "ifany")
table(submersion$birthyr, submersion$generation, useNA = "ifany")

############                   Education                        ############

# STEP 1: Examine variable and coding schema

table(submersion$educ, useNA = "ifany")

# STEP 2: Create generation variables 

submersion <- submersion %>%
  mutate(
    education = case_when(
      educ %in% c("No HS", "High school graduate") ~
        "High school or less",
      
      educ %in% c("Some college", "2-year") ~
        "Some college / 2-year degree",
      
      educ %in% c("4-year", "Post-grad") ~
        "College degree or higher",
      
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$education, useNA = "ifany")
table(submersion$educ, submersion$education, useNA = "ifany")

############                   Race                        ############

# STEP 1: Examine variable and coding schema

table(submersion$race, useNA = "ifany")

# STEP 2: Create generation variables 

submersion <- submersion %>%
  mutate(
    race_group = case_when(
      race == "White" ~ "White",
      race == "Black" ~ "Black",
      race == "Hispanic" ~ "Hispanic",
      race == "Asian" ~ "Asian",
      !is.na(race) ~ "Other",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$race_group, useNA = "ifany")
table(submersion$race, submersion$race_group, useNA = "ifany")

############                   Number of Children                        ############

# STEP 1: Examine variable and coding schema

table(submersion$numchildren, useNA = "ifany")

# STEP 2: Create number of children variables 

submersion <- submersion %>%
  mutate(
    number_children = as.numeric(numchildren),
    
    children_group = case_when(
      number_children == 0 ~ "No children",
      number_children >= 1 & number_children <= 2 ~ "One to two children",
      number_children >= 3 & number_children <= 4 ~ "Three to four children",
      number_children >= 5 ~ "Five or more children",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$number_children, useNA = "ifany")
table(submersion$numchildren, submersion$number_children, useNA = "ifany")

table(submersion$children_group, useNA = "ifany")
table(submersion$numchildren, submersion$children_group, useNA = "ifany")

############                   Parental Status                        ############
# parent_1 = respondent selected that they have a child under age 18
# parent_2 = respondent selected that they have a child age 18 or older
# parent_97 = respondent selected that they are not a parent

# STEP 1: Examine variable and coding schema

table(submersion$parent_1, useNA = "ifany")
table(submersion$parent_2, useNA = "ifany")
table(submersion$parent_97, useNA = "ifany")

# STEP 2: Create parent variables 

submersion <- submersion %>%
  mutate(
    child_under_18 = case_when(
      parent_1 == "selected" ~ 1,
      parent_1 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    child_18_or_older = case_when(
      parent_2 == "selected" ~ 1,
      parent_2 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    not_parent = case_when(
      parent_97 == "selected" ~ 1,
      parent_97 == "not selected" ~ 0,
      TRUE ~ NA_real_
    )
  )

# Combine the three survey items into one parent/not-parent variable
submersion <- submersion %>%
  mutate(
    parent_status = case_when(
      not_parent == 1 ~ "Not a parent",
      child_under_18 == 1 | child_18_or_older == 1 ~ "Parent",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$child_under_18, useNA = "ifany")
table(submersion$parent_1, submersion$child_under_18, useNA = "ifany")

table(submersion$child_18_or_older, useNA = "ifany")
table(submersion$parent_2, submersion$child_18_or_older, useNA = "ifany")

table(submersion$not_parent, useNA = "ifany")
table(submersion$parent_97, submersion$not_parent, useNA = "ifany")

table(submersion$parent_status, useNA = "ifany")

table(submersion$parent_1, submersion$parent_status, useNA = "ifany")
table(submersion$parent_2, submersion$parent_status, useNA = "ifany")
table(submersion$parent_97, submersion$parent_status, useNA = "ifany")


############                   MEDIA VARIABLES                      ############


# These variables are specifically for Paper 2

############                   Primary Media Channels                        ############

# STEP 1: Examine variable and coding schema

table(submersion$CC24_300_1, useNA = "ifany")
table(submersion$CC24_300_2, useNA = "ifany")
table(submersion$CC24_300_3, useNA = "ifany")
table(submersion$CC24_300_4, useNA = "ifany")
table(submersion$CC24_300_5, useNA = "ifany")

# STEP 2: Create media variables 

submersion <- submersion %>%
  mutate(
    
    yes_social_media = case_when(
      CC24_300_1 == "selected" ~ 1,
      CC24_300_1 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    watched_tv_news = case_when(
      CC24_300_2 == "selected" ~ 1,
      CC24_300_2 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    read_newspaper = case_when(
      CC24_300_3 == "selected" ~ 1,
      CC24_300_3 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    listened_radio_news = case_when(
      CC24_300_4 == "selected" ~ 1,
      CC24_300_4 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    none_media = case_when(
      CC24_300_5 == "selected" ~ 1,
      CC24_300_5 == "not selected" ~ 0,
      TRUE ~ NA_real_
    )
  )


# STEP 3: Confirm creation

table(submersion$yes_social_media, useNA = "ifany")
table(submersion$watched_tv_news, useNA = "ifany")
table(submersion$read_newspaper, useNA = "ifany")
table(submersion$listened_radio_news, useNA = "ifany")
table(submersion$none_media, useNA = "ifany")

############                  Social Media Engagement                      ############

# These questions ask about different types of political activity
# on social media.

# STEP 1: Examine variable and coding schema

table(submersion$CC24_300d_1, useNA = "ifany")
table(submersion$CC24_300d_2, useNA = "ifany")
table(submersion$CC24_300d_3, useNA = "ifany")
table(submersion$CC24_300d_4, useNA = "ifany")
table(submersion$CC24_300d_5, useNA = "ifany")
table(submersion$CC24_300d_6, useNA = "ifany")

# STEP 2: Create social media variables 

submersion <- submersion %>%
  mutate(
    
    posted_political_content = case_when(
      CC24_300d_1 == "selected" ~ 1,
      CC24_300d_1 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    commented_politics = case_when(
      CC24_300d_2 == "selected" ~ 1,
      CC24_300d_2 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    read_watched_political = case_when(
      CC24_300d_3 == "selected" ~ 1,
      CC24_300d_3 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    followed_political_event = case_when(
      CC24_300d_4 == "selected" ~ 1,
      CC24_300d_4 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    forwarded_political_content = case_when(
      CC24_300d_5 == "selected" ~ 1,
      CC24_300d_5 == "not selected" ~ 0,
      TRUE ~ NA_real_
    ),
    
    none_political_social_media = case_when(
      CC24_300d_6 == "selected" ~ 1,
      CC24_300d_6 == "not selected" ~ 0,
      TRUE ~ NA_real_
    )
  )

# STEP 3: Confirm creation

# STEP 3: Confirm creation

table(submersion$posted_political_content, useNA = "ifany")
table(submersion$CC24_300d_1, submersion$posted_political_content, useNA = "ifany")

table(submersion$commented_politics, useNA = "ifany")
table(submersion$CC24_300d_2, submersion$commented_politics, useNA = "ifany")

table(submersion$read_watched_political, useNA = "ifany")
table(submersion$CC24_300d_3, submersion$read_watched_political, useNA = "ifany")

table(submersion$followed_political_event, useNA = "ifany")
table(submersion$CC24_300d_4, submersion$followed_political_event, useNA = "ifany")

table(submersion$forwarded_political_content, useNA = "ifany")
table(submersion$CC24_300d_5, submersion$forwarded_political_content, useNA = "ifany")

table(submersion$none_political_social_media, useNA = "ifany")
table(submersion$CC24_300d_6, submersion$none_political_social_media, useNA = "ifany")

############                  Social Media Engagement Index                      ############
# Total engagement ranges from 0 to 5.
#
# Active engagement:
# posting, commenting, or forwarding political content
#
# Passive engagement:
# reading/watching political content or following political events

submersion <- submersion %>%
  mutate(
    
    social_media_engagement =
      posted_political_content +
      commented_politics +
      read_watched_political +
      followed_political_event +
      forwarded_political_content,
    
    active_social_media =
      posted_political_content +
      commented_politics +
      forwarded_political_content,
    
    passive_social_media =
      read_watched_political +
      followed_political_event
  )

# Confirm creation

table(submersion$social_media_engagement, useNA = "ifany")
table(submersion$active_social_media, useNA = "ifany")
table(submersion$passive_social_media, useNA = "ifany")

############                  Political Party                      ############

# STEP 1: Examine variable and coding schema

table(submersion$pid3, useNA = "ifany")

# STEP 2: Create political party variables 

submersion <- submersion %>%
  mutate(
    pid3_model = case_when(
      pid3 == "Democrat" ~ "Democrat",
      pid3 == "Republican" ~ "Republican",
      pid3 == "Independent" ~ "Independent",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$pid3_model, useNA = "ifany")
table(submersion$pid3, submersion$pid3_model, useNA = "ifany")


############                  Family Income                     ############

# STEP 1: Examine variable and coding schema

table(submersion$faminc_new, useNA = "ifany")

# STEP 2: Create income variables 

submersion <- submersion %>%
  mutate(
    income_group = case_when(
      
      faminc_new %in% c(
        "Less than $10,000",
        "$10,000 - $19,999",
        "$20,000 - $29,999",
        "$30,000 - $39,999"
      ) ~ "Under $40,000",
      
      faminc_new %in% c(
        "$40,000 - $49,999",
        "$50,000 - $59,999",
        "$60,000 - $69,999",
        "$70,000 - $79,999"
      ) ~ "$40,000 to $79,999",
      
      faminc_new %in% c(
        "$80,000 - $99,999",
        "$100,000 - $119,999",
        "$120,000 - $149,999"
      ) ~ "$80,000 to $149,999",
      
      faminc_new %in% c(
        "$150,000 - $199,999",
        "$200,000 - $249,999",
        "$250,000 - $349,999",
        "$350,000 - $499,999",
        "$500,000 or more"
      ) ~ "$150,000 or more",
      
      faminc_new == "Prefer not to say" ~ NA_character_,
      
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$income_group, useNA = "ifany")
table(submersion$faminc_new, submersion$income_group, useNA = "ifany")

############                  Home Ownership                     ############

# STEP 1: Examine variable and coding schema

table(submersion$ownhome, useNA = "ifany")

# STEP 2: Create home ownership variables 

submersion <- submersion %>%
  mutate(
    home_ownership = case_when(
      ownhome == "Own" ~ "Own",
      ownhome == "Rent" ~ "Rent",
      ownhome == "Other" ~ "Other",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$home_ownership, useNA = "ifany")
table(submersion$ownhome, submersion$home_ownership, useNA = "ifany")

############                  Region                     ############

# STEP 1: Examine variable and coding schema

table(submersion$region, useNA = "ifany")

# STEP 2: Create region variables 

submersion <- submersion %>%
  mutate(
    region_group = case_when(
      region == "Northeast" ~ "Northeast",
      region == "Midwest" ~ "Midwest",
      region == "South" ~ "South",
      region == "West" ~ "West",
      TRUE ~ NA_character_
    )
  )

# STEP 3: Confirm creation

table(submersion$region_group, useNA = "ifany")
table(submersion$region, submersion$region_group, useNA = "ifany")

############                  Employment Status                     ############

# STEP 1: Examine variable and coding schema

table(submersion$employ, useNA = "ifany")

# STEP 2: Create employment variables 

submersion <- submersion %>%
  mutate(
    employment_group = case_when(
      
      employ %in% c(
        "Full-time",
        "Part-time"
      ) ~ "Employed",
      
      employ %in% c(
        "Temporarily laid off",
        "Unemployed"
      ) ~ "Unemployed",
      
      employ %in% c(
        "Retired",
        "Permanently disabled",
        "Homemaker",
        "Student"
      ) ~ "Not in labor force",
      
      TRUE ~ NA_character_
    )
  )


# STEP 3: Confirm creation

table(submersion$employment_group, useNA = "ifany")
table(submersion$employ, submersion$employment_group, useNA = "ifany")