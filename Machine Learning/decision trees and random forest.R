#### Assignment for Trees and Forest with a focus on class imbalance

## You all have to explore rpart parameters list to handle class imbalance
## FYI, there is a parameter called loss_matrix which can do this.
## A link for that: https://stats.stackexchange.com/questions/96081/how-do-i-specify-a-loss-matrix-in-rpart

install.packages("ROSE") # It has the required data set
library(ROSE)

data(hacide) # This will load two files in workspace:
             # - hacide.train: training data
             # - hacide.test: testing data

# checking columns information
str(hacide) # you should see that there are three columns
            # "cls" is the output variable

table(hacide.train$cls)  # you can see that there is a 98-2 ratio between classes. We will consider this later


# Question -1: a)  Train a decision tree model.

## your code here
library(rpart)
library(rpart.plot)
# Building the tree model
hacideTree = rpart( cls ~ . , method="class", data = hacide.train,control = rpart.control(minsplit = 10))
# visualize the tree
prp(hacideTree)


# Question -1: b)   Test your model by first using predict function and then check accuracy,precision and recall

# Testing
predictTest <- predict(hacideTree, newdata = hacide.test, type = "class")

table(predictTest,hacide.test$cls)

# calculating accuracy: (245 + 2)/(245+3+2)
# calculating precision: 2/(0+2)
# calculating recall: 2/(3+2)



# Question -2: a)  Train a Random Forest model.
## your code here
library(ranger)

bag.hacide=ranger(cls~.,data=hacide.train,mtry=2,splitrule = "gini",importance = "permutation")


# Question -2: b)   Test your model by first using predict function and then check accuracy,precision and recall

yhat.bag = predict(bag.hacide,data=hacide.test)

# plotting results against actual
table(yhat.bag$predictions, hacide.test$cls)

# calculating accuracy: (245 + 2)/(245+3+2)
# calculating precision: 2/(3+2)
# calculating recall: 2/(0+2)



### As you can see that results are not very good, especially recall, try to incorporte loss matrix or anything which can counter class imabalance
# Question -3: a)  Train a decision tree model with an additional loss matrix.

## your code here

# Building the tree model
hacideTree = rpart( cls ~ . , method="class", data = hacide.train, 
                    parms=list(split="information", loss=matrix(c(0,20,1,0), byrow=TRUE, nrow=2)),
                    control = rpart.control(minsplit = 10))
# visualize the tree
prp(hacideTree)


# Question -3: b)   Test your model by first using predict function and then check accuracy,precision and recall

# Testing
predictTest <- predict(hacideTree, newdata = hacide.test, type = "class")

table(predictTest,hacide.test$cls)

# calculating accuracy: (245 + 3)/(245+2+0+3)
# calculating precision: 3/(0+3)
# calculating recall: 3/(3+2)


# Q. What is loss matrix in rpart?
# Ans. we specify the 'parms' parameter in which we assign a matrix to loss parameter
# containing 3 arguments "vector, byrow and nrow"
# vector contains the weights assigned, we assign 0 to diagonal and non-zero to non-diagonal elements
# byrow =TRUE means that we interpret the vector as distributed by rows i.e. 1st 2 elements are row 1
# and the last 2 elements are row 2. then we specify nrows meaning how many rows will the matrix contain.
# we specify a weight of 20 to FALSE NEGATIVE as we are intolerant of predicting 0 for actual 1.