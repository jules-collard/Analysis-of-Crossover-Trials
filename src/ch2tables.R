library(tidyverse)
library(readxl)
library(kableExtra)

pef.data <- read_xlsx("../data/DataExample_Ch2_JK.xlsx")
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

writeLines(table.latex, "../report/tables/pefData.tex")
