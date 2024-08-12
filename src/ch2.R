library(tidyverse)
library(readxl)
library(kableExtra)

# Data Import
data <- read_xlsx("data/DataExample_Ch2_JK.xlsx")
data <- data %>%
  arrange(Group, Subject) %>%
  mutate(Sequence = if_else(Group == 1, "AB", "BA")) %>%
  mutate(across(c(Group, Sequence, Subject, Subject_Label), as_factor))

data.long <- data %>% # Long format needed for summaries
  rename(`1` = "Period1", `2` = "Period2") %>%
  pivot_longer(cols = c(`1`, `2`), names_to = "Period", values_to = "PEFR",
               names_transform = list(Period = as_factor)) %>%
  mutate(Treatment = if_else((Sequence == "AB" & Period == 1) |
                               (Sequence == "BA" & Period == 2), "A", "B")) %>%
  mutate(across(Treatment, as_factor))

# Table 2.1 - Subsample
sink("report/tables/ch2/pefrDataSubsample.tex", type="output")
data %>% 
  select(Sequence, Subject, Subject_Label, Period1, Period2) %>%
  arrange(Sequence, Subject) %>%
  rename(`Subject Label` = "Subject_Label", `Period 1` = "Period1",
         `Period 2` = "Period2") %>%
  head(5) %>%
  kbl(format = "latex",
      caption = "Subsample of COPD Trial Data (PEFR in L/min)",
      label = "pefrDataSubsample",
      booktabs = TRUE,
      digits = 2)
sink()

# Constructing Summary Table
summary.period.sequence <- data %>% # Summary by period & sequence
  group_by(Sequence) %>%
  summarise(Subjects = n(),
            `Mean PEFR Period 1` = round(mean(Period1), 2),
            `SD PEFR Period 1` = round(sd(Period1), 2),
            `Mean PEFR Period 2` = round(mean(Period2), 2),
            `SD PEFR Period 2` = round(sd(Period2), 2))

summary.sequence <- data.long %>% # Summary by sequence
  group_by(Sequence) %>%
  summarise(`Overall Mean PEFR` = round(mean(PEFR), 2),
            `Overall SD PEFR` = round(sd(PEFR), 2))

summary.period <- data.long %>% # Summary by period
  group_by(Period) %>%
  summarise(ov_mean = round(mean(PEFR), 2),
            ov_sd = round(sd(PEFR), 2)) %>%
  pivot_wider(names_from = Period,
              values_from = c(ov_mean, ov_sd)) %>%
  rename(`Mean PEFR Period 1` = "ov_mean_1",
         `SD PEFR Period 1` = "ov_sd_1",
         `Mean PEFR Period 2` = "ov_mean_2",
         `SD PEFR Period 2` = "ov_sd_2") %>%
  mutate(Sequence = "Total", Subjects = 56,
         `Overall Mean PEFR` = round(mean(data.long$PEFR), 2),
         `Overall SD PEFR` = round(sd(data.long$PEFR), 2),
         .before = `Mean PEFR Period 1`)

summary.table <- inner_join(summary.period.sequence, summary.sequence,
                            by = "Sequence") %>%
  bind_rows(summary.period) %>%
  select(Sequence, Subjects,
         colnames(summary.sequence),
         colnames(summary.period.sequence))

# Table 2.2 - Summary
sink("report/tables/ch2/pefrDataSummary.tex", type="output")
summary.table %>%
  kbl(format="latex",
      caption = "Summary Table for COPD Trial Data",
      label = "pefrDataSummary",
      booktabs = TRUE,
      digits = 2,
      col.names = c("", "Subjects", rep(c("Mean", "SD"), 3)),
      linesep = "") %>%
  add_header_above(c(" "=2, "Overall"=2, "Period 1"=2, "Period 2"=2)) %>%
  row_spec(2, hline_after = TRUE) %>%
  column_spec(2, border_right = TRUE)
sink()

# Figure 2.1 - Boxplot
ggplot(data = data.long, mapping = aes(x=Period, y = PEFR)) +
  geom_boxplot() +
  facet_wrap(~Sequence) +
  theme_bw()
ggsave("report/figures/ch2/boxplot.png")

# Figure 2.2 - Subject-Profiles Plot
ggplot(data = data.long, mapping = aes(x=Period, y=PEFR)) +
  geom_line(aes(group = Subject_Label), alpha = 0.55) +
  geom_point(aes(colour = Sequence), show.legend = FALSE) +
  facet_wrap(~Sequence) +
  theme_bw()
ggsave("report/figures/ch2/subjectProfilesPlot.png")

# Figure 2.3 - Paired boxplot
ggplot(data = data.long, mapping = aes(x=Period, y=PEFR)) +
  facet_wrap(~Sequence) +
  geom_boxplot(outliers = FALSE) +
  geom_line(aes(group = Subject_Label), alpha = 0.35) +
  geom_point(alpha=0.6, mapping = aes(col = Sequence), show.legend = FALSE) +
  theme_bw()
ggsave("report/figures/ch2/pairedBoxplot.png")

# Figure 2.4 - Period 1 vs Period 2 Plot
ggplot(data = data, mapping = aes(x=Period1, y=Period2, colour=Sequence)) +
  geom_point(show.legend = FALSE) +
  geom_abline(slope=1,intercept=0) +
  facet_wrap(~Sequence) +
  labs(x = "Period 1", y = "Period 2") +
  theme_bw()
ggsave("report/figures/ch2/periodsPlot.png")

# Figure 2.5 - Period 1 vs Period 2 with Centroids
means <- data %>% # Calculate centroids
  group_by(Sequence) %>%
  summarise(Period1 = mean(Period1), Period2 = mean(Period2))

ggplot(data = data, mapping = aes(x=Period1, y=Period2, colour=Sequence)) +
  geom_point() +
  geom_point(data = means, size=5, shape="square") +
  geom_abline(slope=1,intercept=0) +
  labs(x = "Period 1", y = "Period 2") +
  theme_bw()
ggsave("report/figures/ch2/centroidsPlot.png")

# Figure 2.6 Groups-by-Periods Plot
means.long <- means %>% # Calculate means
  rename(`1` = "Period1", `2` = "Period2") %>%
  pivot_longer(cols = c(`1`, `2`), names_to = "Period", values_to = "PEFR",
               names_transform = as_factor)

ggplot(data = means.long, mapping = aes(x = Period, y = PEFR, colour = Sequence)) +
  geom_line(aes(group = Sequence)) +
  geom_point() +
  labs(y = "Mean PEFR") +
  theme_bw()
ggsave("report/figures/ch2/groupsByPeriodsPlot.png")
