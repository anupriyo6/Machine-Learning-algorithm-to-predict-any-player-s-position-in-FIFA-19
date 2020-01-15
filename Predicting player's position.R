## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats
library(data.table)
library(tidyverse) # metapackage with lots of helpful functions
train<-fread('../input/data.csv')
## Running code
head(train)
names(train)
dim(train)
train$Position[1]
attributes<-train%>%
  select(c('Name','Age','Club','Nationality','Acceleration', 'Aggression', 'Agility', 'Balance', 'BallControl',
           'Composure', 'Crossing', 'Curve', 'Dribbling', 'Finishing',
           'FKAccuracy', 'HeadingAccuracy', 'Interceptions',
           'Jumping', 'LongPassing', 'LongShots', 'Marking', 'Penalties',
           'Positioning', 'Reactions', 'ShortPassing', 'ShotPower',
           'SlidingTackle', 'SprintSpeed', 'Stamina', 'StandingTackle',
           'Strength', 'Vision', 'Volleys', 'Position'
           
  ))
head(attributes)
#strip out goalkeepers from the set
attributes<-attributes%>%
  filter(Position!="GK")
head(attributes)
sapply(attributes,function(x)sum(is.na(x)))
anyNA(attributes)
library(Amelia)
missmap(attributes)


library(mice)
imputed_data<-mice(attributes,m=5,maxit=10,method='pmm')
summary(imputed_data)


imp<-complete(imputed_data)
head(imp)
sapply(imp,function(x)sum(is.na(x)))
str(imp)
anyNA(imp)
imp$Position<-as.factor(imp$Position)
str(imp$Position)
imp1<-imp%>%dplyr::select(-c("Position","Nationality","Name","Club"))
head(imp1)
head(imp)
names(imp)
scaled<-scale(imp1)
head(scaled)
train_players<-imp[1:12600,]
normalization<-function(x){
    return((x-min(x))/(max(x)-min(x)))
    }
    train_normals<-as.data.frame(lapply(imp1,(normalization)))
    train_n<-train_normals[1:12600,]
    head(train_n)
    test_n<-train_normals[12601:16182,]
    train_label<-imp[1:12600,34]
    head(train_label)
    test_label<-imp[12601:16182,34]
head(train_players)
class(train_players)
sapply(train_players,function(x)sum(is.na(x)))
nrow(train_players)
head(train_players)

test_players<-imp[12601:16182,]
head(test_players)
nrow(test_players)
str(test_players)
str(train_players)
test_player1<-test_players%>%
  dplyr::select(-"Position")
head(test_player1)
str(test_player1)
library(MASS)
#apply LDA algorithm
rf=lda(Position~.,data=train_players[-c(1,3,4)])
rf
#apply KNN algorithm
library(class)
knn_pred<-knn(train_n,test_n,cl=train_label,k=137)
knn_pred[2]
library(gmodels)
CrossTable(test_label,knn_pred,chisq=FALSE)
mat1<-table(knn_pred,test_label)
accuracy.knn<-sum(diag(mat1))/sum(mat1)
accuracy.knn
names(rf)
lda.test<-predict(rf,test_player1)$class
test_players[10,]
lda.test[10]

mat<-table(lda.test,test_players$Position)
mat
accuracy.test<-sum(diag(mat))/sum(mat)
accuracy.test

