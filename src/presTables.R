library(tidyverse)
library(readxl)
library(rstatix)
library(kableExtra)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(emmeans)

data <- read_xlsx("data/DataExample_Ch2_JK.xlsx")
data <- data %>%
  arrange(Group, Subject) %>%
  mutate(Sequence = if_else(Group == 1, "AB", "BA")) %>%
  mutate(across(c(Group, Sequence, Subject, Subject_Label), as_factor)) %>%
  rename(`1` = "Period1", `2` = "Period2") %>%
  pivot_longer(cols = c(`1`, `2`), names_to = "Period", values_to = "PEFR",
               names_transform = list(Period = as_factor)) %>%
  mutate(Treat = if_else((Group == 1 & Period == 1) | (Group == 2 & Period == 2),
         "A", "B"))

# Matched-pairs t-test

sink("report/tables/presentation/tTest.tex")
data %>%
  arrange(Subject_Label, Treat) %>%
  t_test(PEFR ~ Treat, paired = TRUE) %>%
  kbl(format="latex",
      caption="Matched-Pairs t-Test on COPD Data",
      booktabs = TRUE,
      digits = 3,
      col.names = c("", "Group 1", "Group 2", "n1", "n2", "t-statistic", "df", "p"))
sink()

# Mixed Model

mixed.model <- lmer(PEFR~Treat+Period+Sequence + (1 | Subject_Label), data = data)

sink("report/tables/presentation/copdDataEstimates.tex", type="output")
tidy(mixed.model) %>%
  filter(effect == "fixed") %>%
  select(-c(effect, group)) %>%
  kbl(format="latex",
      caption="Mixed Model Estimates for COPD Data",
      col.names = c("", "Estimate", "Std. Error", "df", "t", "p"),
      booktabs=TRUE,
      digits=2) %>%
  column_spec(1, border_right = TRUE)
sink()

# LS Means

# LS Means
emm <- emmeans(mixed.model, pairwise ~ Treat)

sink("report/tables/presentation/copdDataMeans.tex", type="output")
emm %>% rbind(emm$contrasts) %>%
  kbl(format="latex",
      caption="LS Means for Mixed Model on COPD Data",
      col.names = c("Sequence", "Difference", "Adj. Mean", "SE", "df",
                    "Lower CI", "Upper CI"),
      booktabs = TRUE,
      digits=2) %>%
  column_spec(2, border_right = TRUE) %>%
  kable_styling(latex_options = "scale_down")
sink()

# homoscedasticity
model.metrics <- mixed.model %>% augment()

ggplot(data = model.metrics, mapping = aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_abline(slope = 0, linetype = "dashed") +
  scale_y_continuous(limits = symmetric_limits) +
  theme_bw() +
  labs(x = "Fitted Value", y = "Residual")
ggsave("report/figures/presentation/homoscedasticity.png")

#Q-Q Plot
ggplot(data = model.metrics, mapping = aes(sample = .resid)) +
  geom_qq() +
  geom_qq_line() +
  labs(x = "Normal Quantiles", y = "Residuals") +
  theme_bw()
ggsave("report/figures/presentation/qqplot.png")
