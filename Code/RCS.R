install.packages(c("rms","ggplot2"))
library(rms)
library(ggplot2)
dd <- datadist(data_RCS)
options(datadist = "dd")
m1 <- cph(Surv(time, status) ~ rcs(ageonset, 4) + sex + duration + seizurenumber,data = data_RCS, x = TRUE, y = TRUE)
anova(m1)
HR <- as.data.frame(Predict(m1, ageonset, fun = exp, ref.zero = TRUE))
View(HR)
P1 <- ggplot(HR, aes(x = ageonset, y = yhat)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "darkblue") +
  geom_line(size = 1, alpha = 0.7, colour = "darkblue") +
  geom_hline(yintercept = 1, linetype = 2, size = 1) +
  geom_vline(xintercept = 10, linetype = 2, size = 1) +
  geom_vline(xintercept = 12.5, linetype = 2, size = 1) +
  labs(x = "Age at onset (years)", y = "HR (95% CI)") +
  theme_classic()
P1
