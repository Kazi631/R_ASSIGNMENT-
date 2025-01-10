#loading library 
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(epitools)


#load data 
tdata <- read_excel("clean_data/clean_kap.xlsx")

#Table 1 Demographic characteristics of study participants 
tdata |> 
  select(1:11) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tables/Table1.docx")

#Table 2 Major sources of information about antibiotic parents
tdata |> 
  select(19:27) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tables/Table2.docx")

#Table 3 Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school-going children
tdata |> 
  select(13, 15, 17) |>
  tbl_summary() |>
  as_gt() |>
  gtsave("Tables/Table3.docx")

#Table 4 Factors associated with the level of knowledge among parents of school-going children
#recode Knowledge status
data <- tdata |> 
  mutate(Knowledge_code = case_when(
    Knowledge_status == "Poor" ~ 0,
    Knowledge_status == "Moderate" ~ 0.5,
    Knowledge_status == "Good" ~ 1,
  ))

mv_logreg <- glm(
  Knowledge_code ~ 
    `Parent’s age (years)` + 
    `Parent’s sex` + 
    `Parent’s education level` + 
    `Employment status` + 
    `Family type` + 
    `Your average household income per month (BDT)` + 
    `Child’s sex` + 
    `Child’s age (years)` + 
    `Number of children`,
  data = data,
  family = "binomial")
#regression table
mv_logreg |>
  tbl_regression(exponentiate = TRUE)|>
  bold_p()|>
  as_gt()|>
  gtsave("Tables/Table4.docx")



#Table 5  Factors associated with the level of attitudes towards antibiotic resistance among parents of school-going children
#recode attitude status
data <- tdata |> 
  mutate(Attitude_code = case_when(
    Attitude_status == "Negative" ~ 0,
    Attitude_status == "Uncertain" ~ 0.5,
    Attitude_status == "Positive" ~ 1,
  ))

mv_logreg <- glm(
  Attitude_code ~ 
    `Parent’s age (years)` + 
    `Parent’s sex` + 
    `Parent’s education level` + 
    `Employment status` + 
    `Family type` + 
    `Your average household income per month (BDT)` + 
    `Child’s sex` + 
    `Child’s age (years)` + 
    `Number of children`,
  data = data,
  family = "binomial")
#regression table
mv_logreg |>
  tbl_regression(exponentiate = TRUE)|>
  bold_p()|>
  as_gt()|>
  gtsave("Tables/Table5.docx")

#Table 6 Factors associated with the level of practices regarding antibiotic resistance among parents of school-going children 
#recoding practice status
data <- tdata |> 
  mutate(Practice_code = case_when(
    Practice_status == "Inappropriate" ~ 0,
    Practice_status == "Appropriate" ~ 1
  ))

mv_logreg <- glm(
  Practice_code ~ 
    `Parent’s age (years)` + 
    `Parent’s sex` + 
    `Parent’s education level` + 
    `Employment status` + 
    `Family type` + 
    `Your average household income per month (BDT)` + 
    `Child’s sex` + 
    `Child’s age (years)` + 
    `Number of children`,
  data = data,
  family = "binomial")
#regression table
mv_logreg |>
  tbl_regression(exponentiate = TRUE)|>
  bold_p()|>
  as_gt()|>
  gtsave("Tables/Table6.docx")













































































