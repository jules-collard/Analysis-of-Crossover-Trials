library(tidyverse)
library(readxl)
library(kableExtra)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)
library(ggpmisc)
library(rstatix)

data <- read_xlsx("data/CrossOverData.xlsx") %>%
  mutate(across(Sequence:Treat, as_factor))

# Print data
sink("report/tables/ch3/forSalData.tex")
data %>%
  arrange(Subject, Period) %>% head(6) %>%
  kbl(format="latex",
      caption="For/Sal Data (L/min)",
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

# Table of Estimates
mixed.model <- lmer(PEF~Treat+Period+Sequence + (1 | Subject), data = data)

sink("report/tables/ch3/pefrDataEstimates.tex", type="output")
tidy(mixed.model) %>%
  filter(effect == "fixed") %>%
  select(-c(effect, group)) %>%
  kbl(format="latex",
      caption="Mixed Model Estimates for For/Sal Data",
      col.names = c("", "Estimate", "Std. Error", "df", "t", "p-value"),
      label="pefrDataEstimates",
      booktabs=TRUE,
      digits=2) %>%
  column_spec(1, border_right = TRUE)
sink()

# LS Means
emm <- emmeans(mixed.model, pairwise ~ Treat)
emm$contrasts
sink("report/tables/ch3/pefrDataMeans.tex", type="output")
emm %>% rbind(emm$contrasts) %>%
  kbl(format="latex",
      caption="LS Means for Mixed Model on For/Sal Data",
      label="pefrDataLSMeans",
      col.names = c("Sequence", "Difference", "Adj. Mean", "SE", "df",
                    "Lower CI", "Upper CI"),
      booktabs = TRUE,
      digits=2) %>%
  column_spec(2, border_right = TRUE)
sink()

# Assumption Plots
model.metrics <- mixed.model %>% augment()
ggplot(data = model.metrics, mapping = aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(slope = 0, linetype = "dashed") +
  scale_y_continuous(limits = symmetric_limits) +
  theme_bw() +
  labs(x = "Fitted Value", y = "Residual")
ggsave("report/figures/ch3/homoscedasticity.png")

ggplot(data = model.metrics, mapping = aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Normal Quantiles", y = "Residuals") +
  theme_bw()
ggsave("report/figures/ch3/qqplot.png")
