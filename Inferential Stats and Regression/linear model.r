data("anscombe")

with(anscombe, plot(y3~x3))
ggplot(anscombe, aes(x=x3, y=y3)) + geom_point()
ggplot(anscombe, aes(x=x4, y=y4)) + geom_point() + geom_smooth(method= 'lm')

df_hamilton <- read.table("hamilton.txt", head)

ggplot(df_hamilton, aes(x=x1, y=y)) + geom_point()
ggplot(df_hamilton, aes(x=x2, y=y)) + geom_point()


ggplot(anscombe, aes(x=x2, y=y2)) + geom_point()

df_computerRepair <- read.table("D:/Data Analytics/Course Material/GitHub Repository/Data_Analytics_Certification/Inferential_Statistics/regressionDatasets/computerRepair.txt", header=TRUE)
View(df_computerRepair)

boxplot(df_computerRepair$Units)
boxplot(df_computerRepair$Minutes)

with(df_computerRepair, cor(Minutes, Units))
with(df_computerRepair, cor(Units, Minutes))

lm_computerRepair <- lm(Minutes ~ Units, data=df_computerRepair)
summary(lm_computerRepair)
#making 95% confidence interval using summary data
lm_computerRepair$coefficients[2] + c(-1,1) * 0.505*qt(0.975, 12)

#direct
confint(lm_computerRepair)

predict(lm_computerRepair, data.frame(Units=4))

lm_computerRepair$fitted.values
lm_computerRepair$residuals

#HW - confidence interval of prediction
?predict

df_computerRepair_extndd <- cbind(df_computerRepair, lm_computerRepair$fitted.values, lm_computerRepair$residuals)

View(df_computerRepair_extndd)

#newsPapers
df_newsPapers <- read.table("D:/Data Analytics/Course Material/GitHub Repository/Data_Analytics_Certification/Inferential_Statistics/regressionDatasets/newspapersData.txt", header=TRUE, sep="\t")

View(df_newsPapers)

head(readLines("D:/Data Analytics/Course Material/GitHub Repository/Data_Analytics_Certification/Inferential_Statistics/regressionDatasets/newspapersData.txt"))

library(ggplot2)
ggplot(df_newsPapers, aes(x=Daily, y=Sunday)) + geom_point()

# coefficient interp in words
# confidence intervals of coefficients
# explain in words
# auqaat of newspaper - daily sales is 500,000
# prediction interval
# interpretation in words

lm_newsPapers <- lm(Sunday ~ Daily, data = df_newsPapers)
summary(lm_newsPapers)

#multiple regressors
df_supervisor <- read.table("D:/Data Analytics/Course Material/GitHub Repository/Data_Analytics_Certification/Inferential_Statistics/regressionDatasets/supervisorPerformance.txt", header=TRUE)
lm_supervisor <- lm(Y~X1 + X2 + X3 + X4 + X5 + X6, data=df_supervisor)
summary(lm_supervisor)
