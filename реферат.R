# Classification Tree with rpart
library(rpart)

my.file2 <- read.csv(file.choose(), header = TRUE, sep = ",")
my.dataframe2 <- data.frame(my.file2)
str(my.dataframe2)
summary(my.dataframe2)
str(my.dataframe2$DEATH_EVENT[my.dataframe2$DEATH_EVENT == 1])


#divide into train and test
train <- sample(nrow(my.dataframe2),0.7*nrow(my.dataframe2))
train_ds <- my.dataframe[train,] 
str(train_ds)
test_ds <- my.dataframe[-train,] 
str(test_ds)
str(train_ds$DEATH_EVENT[test_ds$DEATH_EVENT == 0])

# grow tree 
fit <- rpart(DEATH_EVENT ~ .,
             method="anova", data = train_ds)

printcp(fit) # display the results 
plotcp(fit, col = "blue") # visualize cross-validation results 

with(fit, {lines(cptable[, 2] + 1, cptable[, 3], type = "b", col = "red")
  legend("topright", c("Ошибка обучения (rel error)",
                       "Ошибка крос-проверки (xerror)", "min(xerror+xstd)"),
         lty = c(1, 1, 2), col = c("red", "black", "blue"), bty = "n") })
summary(fit) # detailed summary of splits


# plot tree 
plot(fit, uniform=TRUE, 
     main="Death Event")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# prune the tree 
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
(fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Tree for Death Event")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)


# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(fit) # visualize cross-validation results   

#prediction
predtree <- predict(fit, newdata = train_ds) 
head(predtree)

# plot tree 
plot(predtree, uniform=TRUE, 
     main="Prediction Tree for Death Event")
text(predtree, use.n=TRUE, all=TRUE, cex=.8)
table(predtree, test_ds$DEATH_EVENT)



