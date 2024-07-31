library(tidyverse)
library(readxl)
library(kableExtra)

cross.data <- read_xlsx("../data/CrossOverData.xlsx")
cross.data <- cross.data %>%
  arrange(Subject, Period) %>%
  mutate(across(c(Sequence, Subject, Period, Treat), as_factor))

# Render chunk of table in latex
table.latex <- cross.data %>% head(10) %>%
  kbl(caption = "Sample of For/Sal Crossover Trial Results",
      format = "latex") %>%
  kable_paper()

writeLines(table.latex, "../report/tables/crossoverDataLonger.tex")

# Pivot data wider for plotting
cross.data.wider <- cross.data %>%
  select(-Treat) %>%
  pivot_wider(names_from = Period,
              names_prefix = "Period ",
              values_from = "PEF")

# Render chunk of table in latex
table.latex.wider <- cross.data.wider %>% head(5) %>%
  kbl(caption = "Sample of For/Sal Crossover Trial Results (Wider Format)",
      format = "latex") %>%
  kable_paper()

writeLines(table.latex.wider, "../report/tables/crossoverDataWider")
