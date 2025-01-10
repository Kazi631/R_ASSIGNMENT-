#library 
library(tidyverse)
library(dplyr)
library(readxl)
library(xlsx)

#import data 
rdata <- read_excel("raw_data/AMR_KAP_Data.xlsx")

#removing missing data 
rdata <- na.omit(rdata)
#Domains 
# 1. Demographic 
demographic <- rdata |>
  select(1:11)

# 2. 28 questions
Q28 <- rdata |>
  select(12:39)
colnames(Q28) <- paste0("Q", 1:28)

# 3. Sources of information
Si<- rdata |>
  select(40:50)

#Knowledge 
Kd <- Q28 |>
  select(Q1:Q12) |>
  mutate(across(Q1:Q12, ~case_when(
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(KD = mean(c_across(Q1:Q12), na.rm = TRUE)) |>
  mutate(Knowledge_status = case_when(
    KD <= 0.49 ~ "Poor",
    KD > 0.49 & KD <= 0.79 ~ "Moderate",
    KD > 0.79 ~ "Good"
  ))

#Attitude
Ad <- Q28 |>
  select(Q13:Q22) |>
  mutate(across(Q13:Q22, ~case_when(
    . == "Disagree" ~ 0,
    . == "Agree" ~ 1,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(AD = mean(c_across(Q13:Q22), na.rm = TRUE)) |>
  mutate(Attitude_status = case_when(
    AD <= 0.49 ~ "Negative",
    AD > 0.49 & AD <= 0.79 ~ "Uncertain",
    AD > 0.79 ~ "Positive"
  ))

#Practice 
Pd <- Q28 |>
  select(Q23:Q28) |>
  mutate(across(Q23:Q28, ~case_when(
    . == "No" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(PD = mean(c_across(Q23:Q28), na.rm = TRUE)) |>
  mutate(Practice_status = case_when(
    PD <= 0.79 ~ "Inappropriate",
    PD > 0.79 ~ "Appropriate"
  ))

status <- cbind(Kd, Ad, Pd) |>
  select(KD, Knowledge_status, AD, Attitude_status, PD, Practice_status)

#clean data
cdata <- cbind(demographic, status, Si)

#exporting data 
write.xlsx(cdata, "clean_data/clean_kap.xlsx", row.names = FALSE)

























