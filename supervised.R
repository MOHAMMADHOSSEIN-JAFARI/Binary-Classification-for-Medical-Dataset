
library(ggplot2)
data <- read.csv("data.csv")
summary(data)
# 1 not donate and 2 is will donate blood 
# The target attribute is a binary variable representing whether he/she donated blood in March 2007 (2 stands for donating blood; 1 stands for not donating blood).

data$donate <- ifelse(data$donate== 2 , 1, 0)

data %>%
  gather(Attributes, value, 1:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE, bins=20) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title="Blood Dataset - Histograms") +
  theme_bw()

data %>%
  gather(Attributes, value, 1:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Blood Dataset - Density plots") +
  theme_bw()

## scaling the dataset 
data[, -c(5)] <- scale(data[, -c(5)])

data %>%
  gather(Attributes, value, 1:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE, bins=20) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Frequency",
       title=" Scaled Blood Dataset - Histograms") +
  theme_bw()

data %>%
  gather(Attributes, value, 1:5) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_density(colour="black", alpha=0.5, show.legend=FALSE) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Scaled Blood Dataset - Density plots") +
  theme_bw()


set.seed(1)

#Use 70% of dataset as training set and remaining 30% as testing set
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- data[sample, ]
test <- data[!sample, ]  

#fit logistic regression model
model <- glm(donate~ ., family="binomial", data=train)
#view model summary
summary(model)


model <- glm(donate~ recency+	frequency+	time, family="binomial", data=train)
summary(model)
pscl::pR2(model)["McFadden"]
pscl::pR2(model)["r2ML"]
pscl::pR2(model)["r2CU"]

caret::varImp(model)
car::vif(model)
predicted <- predict(model, test, type="response")
summary(predicted)
#### chooses the optimal cutoff for doing donate or not doing donate 
library(InformationValue)
optimal <- optimalCutoff(test$donate, predicted)[1]
optimal
confusionMatrix(test$donate, predicted)
sensitivity(test$donate, predicted)
specificity(test$donate, predicted)
misClassError(test$donate, predicted, threshold=optimal)
plotROC(test$donate, predicted)

# Fitting SVM to the Training set
install.packages('e1071')
library(e1071)

classifier = svm(formula = donate ~ .,data = train, type = 'C-classification',
  kernel = 'linear')
summary(classifier)
y_pred = predict(classifier, newdata = test[-5])
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm
#### omit the colinearity one 

classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'linear')
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm

classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'polynomial', degree = 3)
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm

classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'polynomial', degree = 5)
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm


classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'polynomial', degree = 10)
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm


classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'radial basis')
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm

classifier = svm(formula = donate ~recency+	frequency+	time ,data = train, type = 'C-classification',
                 kernel = 'sigmoid')
classifier
y_pred = predict(classifier, newdata = test[-5])
y_pred
# Making the Confusion Matrix
cm = table(test[, 5], y_pred)
cm