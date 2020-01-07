library(readr)
movieLens <- read.csv("dataWithGenre.csv", header = TRUE)
dim(movieLens)
movieLens[1:4,] #first four rows
movieLens=na.omit(movieLens) #remove missing values
dim(movieLens)
names(movieLens)# variable names
attach(movieLens)

#linear Regression.
#Regression for all data except movie id, title, and genre with all genera names. 
lmAll=lm(Average.Rating ~ Year+Number.of.Ratings+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+X.no.genres.listed., data = movieLens)
summary(lmAll)
par(mfrow=c(2,2))
plot(lmAll)

#linear Regression refined 1.
lmRefined=lm(Average.Rating ~ Year+Number.of.Ratings+Action+Adventure+Children+Comedy+Horror,data = movieLens)
summary(lmRefined)
par(mfrow=c(2,2))
plot(lmRefined)

#linear Regression refined 2.
lmRefined2=lm(Average.Rating ~ Year+Number.of.Ratings+Action+Comedy+Horror,data = movieLens)
summary(lmRefined2)
par(mfrow=c(2,2))
plot(lmRefined2)

#linear Regression refined 3.
lmRefined3=lm(Average.Rating ~ Year+Number.of.Ratings+Comedy+Horror,data = movieLens)
summary(lmRefined3)
par(mfrow=c(2,2))
plot(lmRefined3)


#linear Regression refined 4.
lmRefined4=lm(Average.Rating ~ Number.of.Ratings,data = movieLens)
summary(lmRefined4)
par(mfrow=c(2,2))
plot(lmRefined4)

####################BEST SUBSET########################
library(leaps)
regfit.full=regsubsets(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                         Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                         Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                         X.no.genres.listed., data=movieLens)
summary(regfit.full)

reg.summary= summary(regfit.full)
reg.summary


#summary function also returns R2, RSS, adjusted R2, Cp and BIC
#we can examine those to try to select the best overall model
names(reg.summary)
reg.summary$rsq #calculate the R^2 for each predictor
par(mfrow=c(2,2)) #split the plotting space
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l") #minimiza adjusted R^2
which.max(reg.summary$adjr2)
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

#built-in plot command can be used to display the selected variables
#for the best model with a given number of predictors, ranked according to 
#BIC, Cp, adjusted R2, or AIC.. THIS IS ANOTHER WAY OF LOOKING AT THE RESULTS
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")

#see the coefficient estimates estimated with the model
coef(regfit.full,7)

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


regfit.best=regsubsets(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                         Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                         Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                         X.no.genres.listed., data=movieLens)#,nvmax=21)
coef(regfit.best,10)
k=10
set.seed(1)
folds=sample(1:k,nrow(movieLens),replace=TRUE)
cv.errors=matrix(NA,k,8, dimnames=list(NULL, paste(1:8)))
#we perform best subset selection within each of the k training sets.
for(j in 1:k){
  best.fit=regsubsets(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                        Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                        Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                        X.no.genres.listed.,data=movieLens[folds!=j,],nvmax=8, really.big=TRUE, method="exhaustive")
  for(i in 1:8){
    pred=predict(best.fit,movieLens[folds==j,],id=i)
    cv.errors[j,i]=mean( (movieLens$Average.Rating[folds==j]-pred)^2)
  }
}
# j:fold index;
# i:size of the model
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                      Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                      Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                      X.no.genres.listed., data=movieLens)#,nvmax=21)
coef(reg.best,8)

###############################################################
###############################################################


#####################PCR############################
library(pls)
library(glmnet)

train.size = dim(movieLens)[1] / 2
training = sample(1:dim(movieLens)[1], train.size)
test = -training
movie.training = movieLens[training, ]
testMovie = movieLens[test, ]


#PCR fit with validation plot
pcr.fit = pcr(Average.Rating ~ Year+Number.of.Ratings+Action+Adventure+Animation+Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+X.no.genres.listed., 
              data = movie.training, scale=T, validation="CV")
validationplot(pcr.fit, val.type="MSEP")

pcrPred = predict(pcr.fit, testMovie, ncomp=10)

#test error
mean(as.matrix(testMovie[, "Average.Rating"] - data.frame(pcrPred))^2)

###########LASSO MODEL#############################

training.matrix = model.matrix(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                                 Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                                 Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                                 X.no.genres.listed., data=movie.training)

test.matrix = model.matrix(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                             Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                             Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                             X.no.genres.listed., data=testMovie)

grid = 10 ^ seq(4, -2, length=100)

lasso = cv.glmnet(training.matrix, movie.training[, "Average.Rating"], alpha=1, lambda=grid, thresh=1e-12)
lambda.best = lasso$lambda.min
lambda.best

lassoPred = predict(lasso, newx=test.matrix, s=lambda.best)
#test error
mean((testMovie[, "Average.Rating"] - lassoPred)^2)

#coefficents
lasso = glmnet(model.matrix(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                              Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                              Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                              X.no.genres.listed., data=movieLens), movieLens[, "Average.Rating"], alpha=1)

predict(lasso, s=lambda.best, type="coefficients")

##############RIDGE MODEL##############################
library(glmnet)
training.matrix = model.matrix(Average.Rating~ Year+Number.of.Ratings+Action+Adventure+Animation+
                                 Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                                 Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                                 X.no.genres.listed., data=movie.training)
test.matrix = model.matrix(Average.Rating~ Year+Number.of.Ratings+Action+Adventure+Animation+
                             Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                             Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                             X.no.genres.listed., data=testMovie)
grid = 10 ^ seq(4, -2, length=100)

ridge = cv.glmnet(training.matrix, movie.training[, "Average.Rating"], alpha = 0, lambda = grid, thresh=1e-12)
lambda.best = ridge$lambda.min
lambda.best

ridgePred = predict(ridge, newx=test.matrix, s=lambda.best)
#RSS
mean((testMovie[, "Average.Rating"] - ridgePred)^2)

predict(ridge, s=lambda.best, type="coefficients")

#############################################

#R^2 plots
test.avg = mean(testMovie[, "Average.Rating"])
ridge.test.r2 = 1 - mean((testMovie[, "Average.Rating"] - ridgePred)^2) /mean((testMovie[, "Average.Rating"] - test.avg)^2)
lasso.test.r2 = 1 - mean((testMovie[, "Average.Rating"] - lassoPred)^2) /mean((testMovie[, "Average.Rating"] - test.avg)^2)
pcr.test.r2 = 1 - mean(as.matrix(testMovie[, "Average.Rating"] - data.frame(pcrPred))^2) /mean((testMovie[, "Average.Rating"] - test.avg)^2)

barplot(c(ridge.test.r2, lasso.test.r2, pcr.test.r2), col=2:5, names.arg=c("Ridge", "Lasso", "PCR"), main="Test R-squared")



############################################
library(tree)
train = sample(dim(movieLens)[1], dim(movieLens)[1]/2)
movies.test = movieLens[-train, ]
movies.train = movieLens[train, ]
movies.tree = tree(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                     Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                     Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                     X.no.genres.listed., data=movieLens)
summary(movies.tree)

plot(movies.tree)
text(movies.tree, pretty = 0)

#get test MSE
pred.movies = predict(movies.tree, movies.test)
mean((movies.test$Average.Rating - pred.movies)^2)

#determine the optimal level of tree complexity
cv.movies=cv.tree(movies.tree,FUN=prune.tree)
par(mfrow = c(1, 2))
plot(cv.movies$size, cv.movies$dev, type = "b") #size: number of terminal nodes of each tree
plot(cv.movies$k, cv.movies$dev, type = "b")#k: complexity parameter (corresponds to alpha)

####################BAGGING############################

library(randomForest)
movies.bag = randomForest(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                            Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                            Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                            X.no.genres.listed., data = movies.train, mtry = 10, ntree = 500, 
                            importance = T)
bag.pred = predict(movies.bag, movies.test)
mean((movies.test$Average.Rating - bag.pred)^2) #TEST MSE

importance(movies.bag)

###########RANDOM FOREST############################
rf.movies = randomForest(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                           Action+Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                           Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                           X.no.genres.listed., data = movies.train, mtry = 5, ntree = 500, 
                           importance = T)
rf.pred = predict(rf.movies, movies.test)
mean((movies.test$Average.Rating - rf.pred)^2)

importance(movies.bag)

###########BOOSTING############################
sum(is.na(movieLens$Average.Rating))
movieLens = movieLens[-which(is.na(movieLens$Average.Rating)), ]
sum(is.na(movieLens$Average.Rating))
movieLens$Average.Rating = log(movieLens$Average.Rating)

library(gbm)

grid=10^seq(-10,-0.2, by = 0.1)
length.grid = length(grid)
trainError = rep(NA, length.grid)
testError = rep(NA, length.grid)
for (i in 1:length.grid) {
  boost.movies = gbm(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                       Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                       Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                       X.no.genres.listed., data = movies.train, distribution = "gaussian", n.trees = 1000, shrinkage = grid[i])
  train.pred = predict(boost.movies, movies.train, n.trees = 1000)
  test.pred = predict(boost.movies, movies.test, n.trees = 1000)
  trainError[i] = mean((movies.train$Average.Rating - train.pred)^2)
  testError[i] = mean((movies.test$Average.Rating - test.pred)^2)
}


plot(grid, trainError, type = "b", xlab = "Shrinkage", ylab = "Train MSE", 
     col = "green", pch = 20)

plot(grid, testError, type = "b", xlab = "Shrinkage", ylab = "Test MSE", 
     col = "purple", pch = 20)

min(testError)
grid[which.min(testError)]

#finding the best coefs with boosting
boost.best = gbm(Average.Rating~Year+Number.of.Ratings+Action+Adventure+Animation+
                   Children+Comedy+Crime+Documentary+Drama+Fantasy+Film.Noir+
                   Horror+Musical+Mystery+Romance+Sci.Fi+Thriller+War+Western+
                   X.no.genres.listed., data = movies.train, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = grid[which.min(testError)])
summary(boost.best)