---
title: "Organ Donation Survey 2025"
author: "Adwoa Odoom"
date: "2026-01-08"
output: html_document
---

# clear console
cat("\014")

# install packages
if (!require("pacman")) install.packages("pacman")

# install additional packages
pacman::p_load(
  rio,
  here,
  tidyverse,
  ggplot2,
  dplyr,
  ggthemes,
  gtsummary,
  flextable,
  janitor,
  stringr,
  broom.helpers,
  broom,
  MASS,
  officer,
  brant,
  VGAM,
  forestplot,
  writexl
)

# import
hrsa_data <- read_csv("new_organ_don.csv")

# view col names
colnames(hrsa_data)

# clean column names
hrsa_data <- hrsa_data %>%
  rename(
    signed_up_yn = `Q13. Are you signed up to be an organ donor?`,
    support_yn = `Q4. In general, do you strongly support, support, oppose, or strongly oppose the donation of organs for transplantation?`,
    donate_yn = `Q5. Would you want your organs to be donated after your death?`,
    trust = `Q32. How much of the time do you think you can trust the health care system to do what is right for you?`,
    indicate_donor = `Q12L. If you indicate you intend to be a donor, doctors will be less likely to try to save your life. - Agreement`,
    save_before_don = `Q12P. Doctors do everything they can to save a person's life before organ donation is even considered. - Agreement`,
    against_rel = `Q12Q. Organ donation is against my religion. - Agreement`,
    income = `Income (no missing)`,
    location = `Rural (no missing)`,
    edu = `Educational Category`,
    sex = `Sex`,
    sex_3_lvl = `Sex, 3 levels`,
    age = `Age Category`,
    race = `Race Category`
  )

# clean rows
hrsa_data <- hrsa_data %>%
  mutate(across(c(edu, sex, sex_3_lvl, age, race, income, location), ~ str_replace(.x, "^[^:]+:\\s*", "")))

# outcomes: signup_yn > donor_signup, support_yn > support, donate_yn > donate
# predictors: trust, indicate_donor, save_before_don, against_rel, income, location, sex, sex_3_lvl, race, age, edu


# convert variables to factors
hrsa_data <- hrsa_data %>%
  mutate(race = factor(race, labels= c("White","Black","Hispanic","Asian","American Indian", "Multiple/Other")))

hrsa_data <- hrsa_data %>%
  mutate(age = factor(age, labels= c("18 - 34","35 - 49","50 - 64","65+"),
                      levels = c("18 - 34","35 - 49","50 - 64","65+")))

hrsa_data <- hrsa_data %>%
  mutate(edu = factor(edu, labels= c("Up to High School","Some College/Trade School","College Bachelors","Postgraduate"),
                      levels = c("Up to High School","Some College/Trade School","College Bachelors","Postgraduate")))

hrsa_data <- hrsa_data %>%
  mutate(sex = factor(sex, labels= c("Male","Female"),
                      levels = c("Male","Female")))

hrsa_data <- hrsa_data %>%
  mutate(sex_3_lvl = factor(sex_3_lvl, labels= c("Male","Female","Transgender"),
                            levels = c("Male","Female","Transgender")))

hrsa_data <- hrsa_data %>%
  mutate(income = factor(income, levels = c("Less than $30,000", "$30,000 to $59,000", "$60,000 to $99,999", "$100,000 or more"), labels = c("Less than $30,000", "$30,000 to $59,999", "$60,000 to $99,999", "$100,000 or more")))

hrsa_data <- hrsa_data %>%
  mutate(location = factor(location, labels= c("Rural","Urban"),
                           levels = c("Rural","Urban")))

hrsa_data <- hrsa_data %>%
  mutate(trust = factor(trust, labels= c("Almost none of the time","Some of the time", "Almost all of the time", "Most of the time"),
                        levels = c("Almost none of the time","Some of the time", "Almost all of the time", "Most of the time")))


hrsa_data <- hrsa_data %>%
  mutate(indicate_donor = factor(indicate_donor, labels= c("Strongly Disagree","Somewhat Disagree", "Somewhat Agree", "Strongly Agree"),
                                 levels = c("Strongly Disagree","Somewhat Disagree", "Somewhat Agree", "Strongly Agree")))

hrsa_data <- hrsa_data %>%
  mutate(against_rel = factor(against_rel, labels= c("Strongly Disagree","Somewhat Disagree", "Somewhat Agree", "Strongly Agree"),
                              levels = c("Strongly Disagree","Somewhat Disagree", "Somewhat Agree", "Strongly Agree")))

hrsa_data <- hrsa_data %>%
  mutate(save_before_don = factor(save_before_don, labels= c("Strongly Disagree","Somewhat Disagree", "Somewhat Agree", "Strongly Agree"),


# convert outcomes to 1/0
hrsa_data <- hrsa_data %>%
  mutate(
    signup = case_when(
      signed_up_yn == "Yes"        ~ 1,
      signed_up_yn == "No"         ~ 0,
      signed_up_yn %in% c("Don't know", "Refused") ~ NA_real_))

hrsa_data <- hrsa_data %>%
  mutate(
    support = case_when(
      support_yn %in% c("Strongly support", "Support") ~ 1,
      support_yn %in% c("Oppose", "Strongly oppose" ) ~ 0,
      support_yn %in% c("Don't Know", "Refused") ~ NA_real_))

hrsa_data <- hrsa_data %>%
  mutate(
    donate = case_when(
      donate_yn %in% c("Definitely Yes", "Probably Yes") ~ 1,
      donate_yn %in% c("Probably No", "Definitely No" ) ~ 0,
      donate_yn %in% c("Don't know", "Refused") ~ NA_real_))

# logistic regression functions
run_logit <- function(outcome, predictors, data) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  glm(formula, data = data, family = binomial(link = "logit"))
}

# define predictors
predictors <- c("age", "trust", "indicate_donor", "save_before_don", "against_rel", "income", "location", "sex", "sex_3_lvl", "race", "edu")

# model 1 - signup to be a donor
signup_model <- run_logit("signup", predictors, hrsa_data)
summary(signup_model)
coef(signup_model)

# model 2 - supporting organ donation
support_model <- run_logit("support", predictors, hrsa_data)
summary(support_model)
coef(support_model)

# model 3 - supporting organ donation
donate_model <- run_logit("donate", predictors, hrsa_data)
summary(donate_model)
coef(donate_model)

# create table function

make_logit_table <- function(model, caption, labels) {
  tbl_regression(
    model,
    exponentiate = TRUE,
    label = labels
  ) %>%
    bold_labels() %>%
    as_flex_table() %>%
    set_caption(as_paragraph(as_b(caption)))
}

# define labels
labels <- list(
  age ~ "Age, years",
  race     ~ "Race/Ethnicity",
  edu     ~ "Education",
  sex      ~ "Sex",
  sex_3_lvl ~ "Sex, 3 levels",
  income ~ "Annual income, $",
  location ~ "Location",
  trust ~ "Trust",
  save_before_don ~ "Doctors will save life",
  indicate_donor ~ "Doctors less likely to save life",
  against_rel ~ "Religion")


# table 1 - sign up to be donor
signup_table <- make_logit_table(signup_model, "Q13. Have you signed up to be an organ donor?", labels)
signup_table

# save
doc1 <- read_docx() %>%
  body_add_flextable(signup_table) %>% print(doc1, target = "Donor Sign-up Adjusted Model Logistic Regression Results.docx")


# table 2 - support organ donation
support_table <- make_logit_table(support_model, "Q4. In general, do you strongly support, support, oppose, or strongly oppose the donation of organs for transplantation?", labels)
support_table

# save
doc2 <- read_docx() %>%
  body_add_flextable(signup_table) %>% print(doc2, target = "Support Organ Donation Adjusted Model Logistic Regression Results.docx")

# table 3 - donating posthumously
donate_table <- make_logit_table(donate_model, "Q5. Would you want your organs to be donated after your death?", labels)
donate_table

# save
doc3 <- read_docx() %>%
  body_add_flextable(signup_table) %>% print(doc3, target = "Posthumous Donation Adjusted Model Logistic Regression Results.docx")
