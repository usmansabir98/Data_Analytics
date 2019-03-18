library(ISLR) # for data set
library(class) # for KNN

data("Auto")
dim(Auto)
View(Auto)

mpg01 <- Auto$mpg > median(Auto$mpg)

dataset <- data.frame(Auto, mpg01)
View(dataset)


with(dataset, plot(mpg ~ mpg01))
with(dataset, boxplot(mpg ~ mpg01))

with(dataset, plot(cylinders ~ mpg01))
with(dataset, boxplot(cylinders ~ mpg01))

with(dataset, plot(displacement ~ mpg01))
with(dataset, boxplot(displacement ~ mpg01))

with(dataset, plot(weight ~ mpg01))
with(dataset, boxplot(weight ~ mpg01))

with(dataset, plot(horsepower ~ mpg01))
with(dataset, boxplot(horsepower ~ mpg01))

with(dataset, plot(acceleration ~ mpg01))
with(dataset, boxplot(acceleration ~ mpg01))

with(dataset, plot(year ~ mpg01))
with(dataset, boxplot(year ~ mpg01))

with(dataset, plot(origin ~ mpg01))
with(dataset, boxplot(origin ~ mpg01))

# mpg - taking as predictor as it clearly contains samples without overlapping
# cylinders - taking as predictor as mostly samples with 6-8 cylinders contain class 0 and 4 cylinders contain class 1
# displacement - cannot be taken as predictor as lots of overlapping
# weight - taking as predictor as more samples have class 0 with higher weight and vice versa
# horsepower - taking as predictor as some dependency there
# accelaration - taking as predictor as some dependency there
# year - cannot take as predictor as no clear dependency (overlapping samples)
# origin - cannot take as predictor as much overlap is there

# features -> mpg, cylinders, weight, horsepower, acceleration

summary(dataset$mpg01)

standardized.X=scale(dataset[,c(1,4,5,6)])
var(dataset[,1])
var(standardized.X[,1])

test=1:80
train.X=standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=dataset$mpg01[-test]
test.Y=dataset$mpg01[test]
set.seed(1)

knn.pred=knn(train.X,test.X,train.Y,k=5)  # KNN model!
mean(test.Y!=knn.pred)
mean(test.Y!=FALSE)
table(knn.pred,test.Y)

# Output is better at k=1 and k=5 with precission 22/25 and recall 22/22