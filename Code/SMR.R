install.packages(c("ems","dplyr","ggplot2","readxl")) 
library(ems) 
library(dplyr) 
library(ggplot2) 
library(readxl)
## 1. Overall SMR
smr_overall <- SMR(data_smr$status, data_smr$prestatus)
print(smr_overall)

## 2. SMR by sex
smr_male <- SMR(data_smr$status[data_smr$sex == 1],data_smr$prestatus[data_smr$sex == 1])
print(smr_male)
smr_female <- SMR(data_smr$status[data_smr$sex == 2],data_smr$prestatus[data_smr$sex == 2])
print(smr_female)

## 3. SMR by 18 age groups
smr_by_age <- list()
for (i in 1:18) {
  subset_data <- data_smr[data_smr$ageenrolled == i, ]
  smr_result  <- SMR(subset_data$status, subset_data$prestatus)
  smr_by_age[[as.character(i)]] <- smr_result}
print(smr_by_age)

## 4. SMR by 18 provinces
smr_by_province <- list()
for (g in 1:18) {
  subset_data <- data_smr[data_smr$provincec == g, ]
  smr_result  <- SMR(subset_data$status, subset_data$prestatus)
  smr_by_province[[as.character(g)]] <- smr_result}
print(smr_by_province)

## 5. Sex–Age SMR Plot
data_all <- read_excel("data_smr.xlsx")
age_levels <- c(
  "5-9","10-14","15-19","20-24","25-29","30-34","35-39",
  "40-44","45-49","50-54","55-59","60-64","65-69",
  "70-74","75-79","80-84","85+")
data_all$Age <- factor(data_all$Age, levels = age_levels)
data_plot <- data_all %>% filter(!is.na(UpperCI))
data_plot$SMR     <- as.numeric(data_plot$SMR)
data_plot$LowerCI <- as.numeric(data_plot$LowerCI)
data_plot$UpperCI <- as.numeric(data_plot$UpperCI)
data_plot$Group <- factor(data_plot$Group,levels = c(1, 2, 3),labels = c("All", "Male", "Female"))
ggplot(data_plot, aes(Age, SMR, group = Group)) +
  geom_ribbon(aes(ymin = LowerCI, ymax = UpperCI, fill = Group), alpha = 0.4, colour = NA) +
  geom_line(aes(color = Group), linewidth = 0.8) +
  scale_color_manual(values = c("Female"="#d73221","All"="#fcb777","Male"="#4573b4")) +
  scale_fill_manual(values = c("Female"="#e35235","All"="#fde699","Male"="#6491c1")) +
  labs(x = "Age at enrollment (years)", y = "SMR", color = "", fill = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title   = element_text(size = 14),
        legend.position = c(0.85, 0.8),
        legend.background = element_blank())

## 6. China Map SMR
install.packages('hchinamap', build_vignettes = TRUE)
library(hchinamap)
hchinamap(name = china_smr$name, value = china_smr$value, width = "100%", height = "400px",
          title = "Geographical distribution of SMR across 18 provinces", legendTitle = "SMR",
          titleSize = "20px", legendLayout = "horizontal", region = "China")
