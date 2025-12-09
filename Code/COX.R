install.packages(c("survival","survminer","haven","dplyr","broom"))
library(survival)
library(survminer)
library(haven)
library(dplyr)
library(broom)
## 1. Univariable Cox
uni_vars <- c("ASMs","Sex","Age at onset","Disease duration","Seizure frequency")
uni_cox_results <- lapply(uni_vars, function(v){
  f <- as.formula(paste0("Surv(time, status) ~ `", v, "`"))
  tidy(coxph(f, data = data_COX)) %>% 
    mutate(variable = v, HR = exp(estimate),
           lower_CI = exp(estimate - 1.96*std.error),
           upper_CI = exp(estimate + 1.96*std.error)) %>%
    select(variable, term, HR, lower_CI, upper_CI, p.value)})
print(uni_cox_results)

## 2. Multivariable Cox
cox_multi <- coxph(Surv(time, status) ~ `Age at onset` + `Sex` + `Disease duration` + `Seizure frequency`, data = data_COX)
multi_cox_tidy <- tidy(cox_multi) %>% 
  mutate(HR = exp(estimate), lower_CI = exp(estimate - 1.96*std.error),
         upper_CI = exp(estimate + 1.96*std.error)) %>%
  select(term, HR, lower_CI, upper_CI, p.value)
print(multi_cox_tidy)
