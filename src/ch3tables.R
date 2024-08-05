library(tidyverse)
library(readxl)
library(xtable)
library(lme4)
library(lmerTest)
library(emmeans)

data <- read_xlsx("data/CrossOverData.xlsx") %>%
  mutate(across(Sequence:Treat, as_factor))

# ANOVA Table
anova.model <- aov(PEF ~ Treat+Period+Sequence+Subject, data = data)
anova.table <- xtable(anova.model, label = "anovaTable",
                      caption = "Example ANOVA Table for 2x2 Cross-over Trial")

# writeLines(print(anova.table), "report/tables/anovaTable.tex")

# Table of Estimates
mixed.model <- lmer(PEF~Treat+Period+Sequence + (1 | Subject), data = data)
model.table <- xtable(summary(mixed.model)$coefficients,
                      caption = "Example Estimates for 2x2 Cross-over Trial",
                      label = "modelTable")

writeLines(print(model.table), "report/tables/estimatesTable.tex")

# LS Means
EMM <- emmeans(mixed.model, ~ Treat)
adj.means <- xtable(EMM %>% as_tibble(),
                    caption = "Example LS Means for 2x2 Cross-over Trial",
                    label = "lsMeansTable")

writeLines(print(adj.means), "report/tables/adjMeansTable.tex")
