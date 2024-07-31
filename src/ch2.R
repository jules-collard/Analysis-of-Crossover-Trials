library(tidyverse)
library(readxl)
library(kableExtra)

cross.data <- read_xlsx("../data/CrossOverData.xlsx")
cross.data <- cross.data %>%
  arrange(Subject, Period) %>%
  mutate(across(c(Sequence, Subject, Period, Treat), as_factor))

table.latex <- cross.data %>% head(10) %>%
  kbl(caption = "Table 1: Sample Crossover Data",
      format = "latex") %>%
  kable_paper()

writeLines(table.latex, "../report/tables/crossoverData.tex")
