install.packages(c("survival", "survminer"))
library(survival)
library(survminer)
## 1. K-M by sex
fit_sex <- survfit(Surv(time, status) ~ sex, data = data_KM)
ggsurvplot(fit_sex, data = data_KM, xlab = "Follow-up Time (months)", ylab = "Survival probability",
           xlim = c(0, 36), ylim = c(0.80, 1.00), break.time.by = 6,
           risk.table = TRUE, risk.table.col = "strata", risk.table.height = 0.25,
           pval = TRUE, pval.method = TRUE, conf.int = FALSE,
           legend.title = "Sex", legend.labs = c("Male", "Female"))

## 2. K-M by baseline annual seizure count
fit_seizure <- survfit(Surv(time, status) ~ seizurenumber, data = data_KM)
ggsurvplot(fit_seizure, data = data_KM, xlab = "Follow-up Time (months)", ylab = "Survival probability",
           xlim = c(0, 36), ylim = c(0.70, 1.00), break.time.by = 6,
           risk.table = TRUE, risk.table.col = "strata", risk.table.height = 0.25,
           pval = TRUE, pval.method = TRUE, conf.int = FALSE,
           legend.title = "Annual seizure count", legend.labs = c("≤ 3", "4–9", "≥ 10"))
