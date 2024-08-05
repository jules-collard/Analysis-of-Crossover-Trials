library(tidyverse)
library(readxl)
library(xtable)

pef.data <- read_xlsx("data/DataExample_Ch2_JK.xlsx")
pef.data <- pef.data %>%
  arrange(Group, Subject) %>%
  mutate(Sequence = if_else(Group == 1, "AB", "BA")) %>%
  mutate(across(c(Group, Sequence, Subject, Subject_Label), as_factor))

pef.data.long <- pef.data %>%
  rename(`1` = "Period1", `2` = "Period2") %>%
  pivot_longer(cols = c(`1`, `2`), names_to = "Period", values_to = "PEFR",
               names_transform = list(Period = as_factor)) %>%
  mutate(Treat = if_else((Sequence == "AB" & Period == 1) |
                         (Sequence == "BA" & Period == 2), "A", "B")) %>%
  mutate(across(Treat, as_factor)) %>%
  select(-Subject) %>%
  rename(Subject = "Subject_Label")

anova.mod <- aov(PEFR ~ Treat+Period+Sequence+Subject, data = pef.data.long)
anova.table.latex <- xtable(anova.mod, label="anovatable")

writeLines(print(anova.table.latex), "report/tables/anovaTable.tex")
