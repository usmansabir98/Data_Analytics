library(ggplot2, quietly = TRUE)
df_anscombe <- read.table("D:/Data Analytics/Course Material/GitHub Repository/Data_Analytics_Certification/Inferential_Statistics_and_Regression_Analysis/regressionDatasets/anscombeQuartet.txt", header = TRUE, sep = "\t")

View(df_anscombe)

with(df_anscombe, boxplot(Y4))

lm_anscombe1 <- lm(Y1 ~ X1, data = df_anscombe[,1:2])

summary(lm_anscombe1)


ggplot(df_anscombe, aes(x = X1, y = Y1)) +
  geom_point() +
  geom_smooth(method = 'lm')


library(car, quietly = TRUE)
library(olsrr, quietly = TRUE)

ols_plot_resid_lev(lm_anscombe1)


ols_plot_cooksd_chart(lm_anscombe1)
ols_plot_dffits(lm_anscombe1)
ols_plot_hadi(lm_anscombe1)
