library(ggplot2)
library(reshape2)
library(dplyr)
library(readxl)
library(xlsx)
library(tidyr)

#loading data
data <- read_excel("raw_data/AMR_KAP_Data.xlsx")

#Fig-1
kdata <- data |>
  select(12:23)
#transposing
kd <- as.data.frame(t(kdata))

yes <- kd |>
  mutate(across(1:704, ~case_when(
  . == "Yes" ~ 1,
  . == "No" ~ 0,
  . == "Don't know" ~ 0,
  TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(yes_count = sum(c_across(1:704), na.rm = TRUE))


no <- kd |>
  mutate(across(1:704, ~case_when(
    . == "Yes" ~ 0,
    . == "No" ~ 1,
    . == "Don't know" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(no_count = sum(c_across(1:704), na.rm = TRUE))


combi1 <- cbind(yes, no) |>
  select(yes_count, no_count)

combi2 <- combi1 |>
  rowwise() |>  
  mutate(dk_count = 704 - sum(c_across(1:2), na.rm = TRUE))



combi3 <- cbind(kd, combi2)

combi4 <- combi3 |>
  select(705:707) |>
  mutate(yes_per = (yes_count / 704) * 100) |>
  mutate(no_per = (no_count / 704) * 100) |>
  mutate(dk_per = (dk_count / 704) * 100)

combi5 <- combi4 |>
  select(4:6)


# Reshape the data for ggplot
combi5$description <- c(
  "Antibiotic kills the bacteria",
  "Amoxicillin is an antibiotic",
  "Azithromycin is an antibiotic",
  "Paracetamol is an antibiotic",
  "Antibiotic kills the virus",
  "Antibiotics used to treat diarrhoea",
  "Antibiotics are useful for flu and cough",
  "Antibiotic-resistant bacteria are difficult to treat",
  "Misuse of antibiotics can lead to antibiotic-resistant bacteria",
  "Antibiotics can cause allergic reactions",
  "Antibiotics can kill normal flora",
  "Infectious diseases are becoming difficult to treat with antibiotics"
)


viz_data1 <- combi5 |>
  pivot_longer(
    cols = c(yes_per, no_per, dk_per),  # Columns to reshape
    names_to = "Response",
    values_to = "Percentage"
  )
str(viz_data1)
head(viz_data1)


#plotting
ggplot(viz_data, aes(x = Percentage, y = reorder(description, -Percentage), fill = Response)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(
    aes(label = round(Percentage, 1)),
    position = position_stack(vjust = 0.5),  # honestly idk about the aesthetics, took assistance from chatgpt
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("yes_per" = "seagreen", "no_per" = "gold", "dk_per" = "lightblue"),
    labels = c("Yes", "No", "Don't Know")
  ) +
  labs(
    title = "Distribution of knowledge regarding antibiotic resistance",
    x = "Percentage (%)",
    y = "",
    fill = "Response"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),  # Bold descriptions
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered title
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.y = element_blank()  # Remove horizontal grid lines
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend to match stacking


#Fig-2
adata <- data |>
  select(24:33)
#transposing
ad <- as.data.frame(t(adata))

agree <- ad |>
  mutate(across(1:704, ~case_when(
    . == "Agree" ~ 1,
    . == "Disagree" ~ 0,
    . == "Neutral" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(agree_count = sum(c_across(1:704), na.rm = TRUE))


disagree <- ad |>
  mutate(across(1:704, ~case_when(
    . == "Agree" ~ 0,
    . == "Disagree" ~ 1,
    . == "Neutral" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(disagree_count = sum(c_across(1:704), na.rm = TRUE))


combi6 <- cbind(agree, disagree) |>
  select(agree_count, disagree_count)


combi7 <- combi6 |>
  rowwise() |>  
  mutate(neutral_count = 704 - sum(c_across(1:2), na.rm = TRUE))


combi8 <- cbind(ad, combi7)

combi9 <- combi8 |>
  select(705:707) |>
  mutate(agree_per = (agree_count / 704) * 100) |>
  mutate(disagree_per = (disagree_count / 704) * 100) |>
  mutate(neutral_per = (neutral_count / 704) * 100)



combi10 <- combi9 |>
  select(4:6)
#checking data
head(combi10)
#reshaping data
combi10$description <- c(
  "I will see another doctor if the first one has not been prescribed antibiotics",
  "I am not satisfied if the doctor does not prescribe an antibiotic to me",
  "Antibiotics are safe and hence can be used commonly",
  "Sick child is given antibiotics, even there is no indication",
  "Antibiotics can improve fever in children",
  "A child with cold is given antibiotics",
  "I stop antibiotics when my child condition improves",
  "I reusing the same antibiotics for similar symptoms",
  "Leftover antibiotics are good to keep at home in case I might need them for my child later on",
  "Doctors often take time to inform parents how antibiotics should be used for their children"
)


viz_data2 <- combi10 |>
  pivot_longer(
    cols = c(agree_per, disagree_per, neutral_per),  # Columns to reshape
    names_to = "Response",
    values_to = "Percentage"
  )
str(viz_data2)
head(viz_data2)


#plotting
ggplot(viz_data2, aes(x = Percentage, y = reorder(description, -Percentage), fill = Response)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(
    aes(label = round(Percentage, 1)),
    position = position_stack(vjust = 0.5),  # # honestly idk about the aesthetics, took assistance from chatgpt
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("agree_per" = "seagreen", "disagree_per" = "gold", "neutral_per" = "lightblue"),
    labels = c("Agree", "Disagree", "Neutral")
  ) +
  labs(
    title = "Attitude towards misuse of antibiotics",
    x = "Percentage (%)",
    y = "",
    fill = "Response"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),  # Bold descriptions
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered title
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.y = element_blank()  # Remove horizontal grid lines
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend to match stacking


#fig3

pdata <- data |>
  select(34:39)
#transposing
pd <- as.data.frame(t(pdata))

yes2 <- pd |>
  mutate(across(1:704, ~case_when(
    . == "Yes" ~ 1,
    . == "No" ~ 0,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(yes_count = sum(c_across(1:704), na.rm = TRUE))


no2 <- pd |>
  mutate(across(1:704, ~case_when(
    . == "Yes" ~ 0,
    . == "No" ~ 1,
    TRUE ~ NA_real_
  ))) |>
  rowwise() |>
  mutate(no_count = sum(c_across(1:704), na.rm = TRUE))


combi11 <- cbind(yes2, no2) |>
  select(yes_count, no_count)

combi12 <- cbind(pd, combi11)

combi13 <- combi12 |>
  select(705:706) |>
  mutate(yes_per = (yes_count / 704) * 100) |>
  mutate(no_per = (no_count / 704) * 100) 



combi14 <- combi13 |>
  select(3:4)
#checking data
head(combi14)
#reshaping data
combi14$description <- c(
  "I give my children antibiotics",
  "I check expiring date of antibiotic before giving to children",
  "I seek medical advice before giving antibiotic to my children",
  "I give my children antibiotics when they get cough",
  "I like to take antibiotic from pharmacy instead of taking from doctor",
  "My child should complete a given dose, even he improve after 2 dose"
)

viz_data3 <- combi14 |>
  pivot_longer(
    cols = c(yes_per, no_per),  # Columns to reshape
    names_to = "Response",
    values_to = "Percentage"
  )
str(viz_data3)
head(viz_data3)


#final
ggplot(viz_data3, aes(x = Percentage, y = reorder(description, -Percentage), fill = Response)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +
  geom_text(
    aes(label = round(Percentage, 1)),
    position = position_stack(vjust = 0.5),  # # honestly idk about the aesthetics, took assistance from chatgpt
    size = 3.5,
    color = "black"
  ) +
  scale_fill_manual(
    values = c("yes_per" = "seagreen", "no_per" = "gold"),
    labels = c("Yes", "No")
  ) +
  labs(
    title = "Practices regarding the use of antibiotics",
    x = "Percentage (%)",
    y = "",
    fill = "Response"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 10, face = "bold"),  # Bold descriptions
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered title
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    panel.grid.major.y = element_blank()  # Remove horizontal grid lines
  ) +
  guides(fill = guide_legend(reverse = TRUE))  # Reverse legend to match stacking


