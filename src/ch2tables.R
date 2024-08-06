library(tidyverse)
library(readxl)
library(kableExtra)

pef.data <- read_xlsx("data/DataExample_Ch2_JK.xlsx")
pef.data <- pef.data %>%
  arrange(Group, Subject) %>%
  mutate(Sequence = if_else(Group == 1, "AB", "BA")) %>%
  mutate(across(c(Group, Sequence, Subject, Subject_Label), as_factor))

# Render chunk of table in latex
table.latex <- pef.data %>% 
  select(Sequence, Subject, Subject_Label, Period1, Period2) %>%
  arrange(Sequence, Subject) %>%
  rename(`Subject Label` = "Subject_Label", `Period 1` = "Period1",
         `Period 2` = "Period2") %>%
  head(10) %>%
  kbl(caption = "Mean PEFR (L/min)",
      format = "latex") %>%
  kable_paper()

# writeLines(table.latex, "report/tables/pefData.tex")

# Creating summary table

# Reformat data for summaries by only period or sequence
pef.data.long <- pef.data %>%
  rename(`1` = "Period1", `2` = "Period2") %>%
  pivot_longer(cols = c(`1`, `2`), names_to = "Period", values_to = "PEFR",
               names_transform = list(Period = as_factor)) %>%
  mutate(Treatment = if_else((Sequence == "AB" & Period == 1) |
                               (Sequence == "BA" & Period == 2), "A", "B")) %>%
  mutate(across(Treatment, as_factor))

# Summary by period & sequence
summary.period.sequence <- pef.data %>%
  group_by(Sequence) %>%
  summarise(Subjects = n(),
            `Mean PEFR Period 1` = round(mean(Period1), 2),
            `SD PEFR Period 1` = round(sd(Period1), 2),
            `Mean PEFR Period 2` = round(mean(Period2), 2),
            `SD PEFR Period 2` = round(sd(Period2), 2))

# Summary by sequence
summary.sequence <- pef.data.long %>%
  group_by(Sequence) %>%
  summarise(`Overall Mean PEFR` = round(mean(PEFR), 2),
            `Overall SD PEFR` = round(sd(PEFR), 2))

# Summary by period
summary.period <- pef.data.long %>%
  group_by(Period) %>%
  summarise(ov_mean = round(mean(PEFR), 2),
            ov_sd = round(sd(PEFR), 2)) %>%
  pivot_wider(names_from = Period,
              values_from = c(ov_mean, ov_sd)) %>%
  rename(`Mean PEFR Period 1` = "ov_mean_1",
         `SD PEFR Period 1` = "ov_sd_1",
         `Mean PEFR Period 2` = "ov_mean_2",
         `SD PEFR Period 2` = "ov_sd_2") %>%
  mutate(Sequence = "Total", Subjects = 56,
         `Overall Mean PEFR` = round(mean(pef.data.long$PEFR), 2),
         `Overall SD PEFR` = round(sd(pef.data.long$PEFR), 2),
         .before = `Mean PEFR Period 1`)

# Combine summaries together into table
summary.table <- inner_join(summary.period.sequence, summary.sequence, by = "Sequence") %>%
  bind_rows(summary.period) %>%
  select(Sequence, Subjects, colnames(summary.sequence), colnames(summary.period.sequence))

summary.latex <- summary.table %>%
  kbl(caption = "PEFR Results Summary (L/min)",
      format = "latex") %>%
  kable_paper()

# writeLines(summary.latex, "report/tables/summaryTable.tex")
