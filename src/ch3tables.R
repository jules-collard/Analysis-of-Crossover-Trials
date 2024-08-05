library(tidyverse)
library(readxl)
library(xtable)

anova.model <- aov(PEF ~ Treat+Period+Sequence+Subject, data = data)
anova.table <- xtable(anova.model, label = "anovaTable",
                      caption = "Example ANOVA Table for 2x2 Cross-over Trial")

writeLines(print(anova.table), "report/tables/anovaTable.tex")
