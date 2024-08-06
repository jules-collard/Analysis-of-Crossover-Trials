library(tidyverse)
library(readxl)
library(xtable)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)
library(ggpmisc)

data <- read_xlsx("data/CrossOverData.xlsx") %>%
  mutate(across(Sequence:Treat, as_factor))

# ANOVA Table
anova.model <- aov(PEF ~ Treat+Period+Sequence+Subject, data = data)
anova.table <- xtable(anova.model, label = "anovaTable",
                      caption = "Example ANOVA Table for 2x2 Cross-over Trial")

writeLines(print(anova.table, include.rownames = FALSE),
           "report/tables/anovaTable.tex")

# Table of Estimates
mixed.model <- lmer(PEF~Treat+Period+Sequence + (1 | Subject), data = data)
model.table <- xtable(broom.mixed::tidy(mixed.model),
                      caption = "Example Estimates for Mixed Model",
                      label = "modelTable")


writeLines(print(model.table, , include.rownames = FALSE),
           "report/tables/estimatesTable.tex")

# LS Means
EMM <- emmeans(mixed.model, ~ Treat)
adj.means <- xtable(EMM %>% as_tibble(),
                    caption = "Example LS Means for 2x2 Cross-over Trial",
                    label = "lsMeansTable")

writeLines(print(adj.means, include.rownames = FALSE),
           "report/tables/adjMeansTable.tex")

# Assumption Plots
model.metrics <- mixed.model %>% augment()
ggplot(data = model.metrics, mapping = aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(slope = 0, linetype = "dashed") +
  scale_y_continuous(limits = symmetric_limits) +
  theme_bw() +
  labs(x = "Fitted Value", y = "Residual")

ggplot(data = model.metrics, mapping = aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Normal Quantiles", y = "Residuals") +
  theme_bw()
