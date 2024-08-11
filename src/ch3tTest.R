library(tidyverse)
library(readxl)
library(rstatix)
library(kableExtra)

# Data Import
data <- read_xlsx("data/CrossOverData.xlsx") %>%
  mutate(across(Sequence:Treat, as_factor))

# Table 3.1 - Subsample
sink("report/tables/ch3/forSalData.tex")
data %>%
  arrange(Subject, Period) %>% head(6) %>%
  kbl(format="latex",
      caption="Subsample of For/Sal Data (L/min)",
      label="forSalData",
      booktabs = TRUE,
      digits=2,
      linesep="",
      positioning="bht")
sink()

# Matched-pairs t-test
data %>%
  arrange(Subject, Treat) %>%
  t_test(PEF ~ Treat, paired = TRUE)

# Table 3.2 - t-test
sink("report/tables/ch3/tTest.tex")
data %>%
  arrange(Subject, Treat) %>%
  t_test(PEF ~ Treat, paired = TRUE) %>%
  kbl(format="latex",
      caption="Matched-Pairs t-Test on For/Sal Data",
      label="tTest",
      booktabs = TRUE,
      digits = 3,
      col.names = c("", "Group 1", "Group 2", "n1", "n2", "t-statistic", "df", "p"))
sink()