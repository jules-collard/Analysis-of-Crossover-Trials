library(tidyverse)
library(readxl)
library(kableExtra)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom.mixed)

# Importing Data
data <- read_xlsx("data/CrossOverData2.xlsx") %>%
  mutate(across(Subject:treat, as_factor)) %>%
  rename(Treatment = "treat")

data.wide <- data %>%
  pivot_wider(id_cols = c(Subject, Sequence),
              names_from = Period, values_from = c(Pre, Post)) %>%
  relocate(Post_1, .after = "Pre_1")

data.long <- data %>%
  pivot_longer(cols = c(Pre,Post),
               names_to = "Measurement",
               values_to = "Insulin",
               names_transform = as_factor)

# Data Example Table
sink("report/tables/ch3/proteinDataSubsample.tex", type="output")
data.wide %>% arrange(Subject) %>%
  head(5) %>%
  kbl(format="latex",
      caption="Subsample of Protein Data",
      label="proteinDataSubsample",
      booktabs=TRUE,
      digits = 1,
      col.names = c("Subject", "Sequence", "Pre", "Post", "Pre", "Post")) %>%
  add_header_above(c(" "=2, "Period 1"=2, "Period 2" = 2)) %>%
  column_spec(2, border_right = TRUE)

sink()

# Summary Table
summary.period.sequence <- data.wide %>% # Summary by period & sequence
  group_by(Sequence) %>%
  summarise(Subjects = n(),
            meanPre1 = mean(Pre_1),
            sdPre1 = sd(Pre_1),
            meanPost1 = mean(Post_1),
            sdPost1 = sd(Post_1),
            meanPre2 = mean(Pre_2),
            sdPre2 = sd(Pre_2),
            meanPost2 = mean(Post_2),
            sdPost2 = sd(Post_2))

summary.sequence <- data.long %>% # Summary by sequence
  group_by(Sequence, Measurement) %>%
  summarise(meanOverall = mean(Insulin),
            sdOverall = sd(Insulin)) %>%
  pivot_wider(names_from = Measurement, values_from = c(meanOverall, sdOverall),
              names_sep = "") %>%
  select(Sequence, meanOverallPre, sdOverallPre, meanOverallPost, sdOverallPost)

summary.period <- data %>% # Summary by period
  group_by(Period) %>%
  summarise(meanPre = mean(Pre),
            sdPre = sd(Pre),
            meanPost = mean(Post),
            sdPost = sd(Post)) %>%
  pivot_wider(names_from = Period, values_from = meanPre:sdPost,
              names_sep = "") %>%
  mutate(Sequence = "Total", Subjects = n_distinct(data$Subject),
         meanOverallPre = mean(data$Pre),
         sdOverallPre = sd(data$Pre),
         meanOverallPost = mean(data$Post),
         sdOverallPost = sd(data$Post),
         .before = meanPre1)

summary.table <- inner_join(summary.sequence, summary.period.sequence,
                            by = "Sequence") %>%
  bind_rows(summary.period) %>%
  relocate(Subjects, .after = Sequence)

sink("report/tables/ch3/proteinDataSummary.tex", type="output")
summary.table %>%
  kbl(format="latex",
      caption = "Summary Table for Protein Data (with Baselines)",
      label = "proteinDataSummary",
      booktabs = TRUE,
      col.names = c("Sequence", "Subject", rep(c("Mean","SD"), 6)),
      digits = 2) %>%
  add_header_above(c(" "=2,
                     "Pre"=2, "Post"=2,
                     "Pre"=2, "Post"=2,
                     "Pre"=2, "Post"=2)) %>%
  add_header_above(c(" "=2, "Overall"=4, "Period 1"=4, "Period 2"=4)) %>%
  row_spec(2, hline_after = TRUE) %>%
  column_spec(c(2, 6), border_right = TRUE) %>%
  kable_styling(latex_options = "scale_down")
sink()

# Boxplot
ggplot(data = data.long, mapping = aes(x = Period, y = Insulin, col = Measurement)) +
  facet_wrap(~Sequence) +
  geom_boxplot() +
  theme_bw()
ggsave("report/figures/ch3/proteinBoxplot.png")

# Data long with baselines
data.baselines <- data.wide %>%
  mutate(Pre_diff = Pre_1 - Pre_2) %>%
  rename(`1` = "Post_1", `2` = "Post_2") %>%
  pivot_longer(cols = c(`1`,`2`), names_to = "Period", values_to = "Post",
               names_transform = as_factor) %>%
  select(Subject, Period, Pre_diff) %>% 
  inner_join(data, join_by(Subject == Subject, Period == Period)) %>%
  relocate(Sequence, .after = "Subject") %>%
  relocate(Pre_diff, .after = "Post")

sink("report/tables/ch3/preDiffSubsample.tex", type="output")
data.baselines %>% arrange(Subject) %>%
  head(5) %>%
  kbl(format="latex",
      caption="Subsample of Protein Data ('Long' Format with Baseline Differences)",
      label="preDiffSubsample",
      booktabs=TRUE,
      digits = 1)
sink()

# Mixed Model
mixed.model.baselines <-
  lmer(Post ~ Treatment + Period * Pre_diff + Sequence + (1|Subject),
                              data = data.baselines)

sink("report/tables/ch3/proteinDataEstimates.tex", type="output")
tidy(mixed.model.baselines) %>%
  select(-c(effect, group)) %>%
  kbl(format="latex",
      caption="Mixed Model Estimates with Baseline Interaction",
      col.names = c("", "Estimate", "Std. Error", "df", "t", "p-value"),
      label="proteinDataEstimates",
      booktabs=TRUE,
      digits=2) %>%
  column_spec(1, border_right = TRUE)
sink()

# LS Means
emm <- emmeans(mixed.model.baselines, pairwise ~ Treatment)

sink("report/tables/ch3/proteinDataMeans.tex", type="output")
emm %>% rbind(emm$contrasts) %>%
  kbl(format="latex",
      caption="LS Means for Mixed Model on Protein Data",
      label="proteinDataLSMeans",
      col.names = c("Sequence", "Difference", "Adj. Mean", "SE", "df",
                    "Lower CI", "Upper CI"),
      booktabs = TRUE,
      digits=2) %>%
  column_spec(2, border_right = TRUE)
sink()
