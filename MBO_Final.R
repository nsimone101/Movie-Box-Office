#'Nick Simone
#'Math 4870
#'3-1-19


#libraries
library(jsonlite)
library(caret)

#read in data
movies <- read.csv("train_box.csv")
test <- read.csv("test_box.csv")

#Params
n<-15

#Remove id and revenue data
movies_id <- movies$ï..id
movies$ï..id <- NULL
revenue <- movies$revenue
movies$revenue <-NULL

test_id <- test$ï..id
test$ï..id<-NULL

##############################
#Add in genre data
##############################

preGenreLength<-length(movies)

#add is / is not genre to train data (movies)
for(i in c(1:length(movies[,1]))){
  if(movies$genres[i]==""){
    next
  }
  genres <- fromJSON(gsub("\'","\"", movies$genres[i]))
  for(g in genres$name){
    g<-gsub("\\s", "",paste("is",g,sep=""))
    if(g %in% names(movies)){
      g_index <- which(names(movies)==g)
      movies[i,g_index] = 1
    }
    else{
      movies<-cbind(movies,temp=0)
      names(movies)[length(names(movies))]<-g
      
      #add also to test since there may be ones not in test
      test<-cbind(test,temp=0)
      names(test)[length(names(test))]<-g
      
      g_index <- which(names(movies)==g)
      movies[i,g_index] = 1
    }
  }
}

#add is / is not genre to test data
for(i in c(1:length(test[,1]))){
  if(test$genres[i]==""){
    next
  }
  genres <- fromJSON(gsub("\'","\"", test$genres[i]))
  for(g in genres$name){
    g<-gsub("\\s", "",paste("is",g,sep=""))
    if(g %in% names(test)){
      g_index <- which(names(test)==g)
      test[i,g_index] = 1
    }
    else{
      test<-cbind(test,temp=0)
      names(test)[length(names(test))]<-g
      g_index <- which(names(test)==g)
      test[i,g_index] = 1
    }
  }
}

#Find most Corr Genres
max<-sum(grepl("is",names(movies)))
cor_indices<-sort(abs(cor(revenue, movies[,grepl("is",names(movies))])), index.return = T)$ix[(max+1-n):max]
cor_indices<-cor_indices+preGenreLength

#Make Genres Factors
for(g in names(movies[,grepl("is",names(movies))])){
  movies[[g]]<-factor(movies[[g]])
}

for(g in names(test[,grepl("is",names(test))])){
  test[[g]]<-factor(test[[g]])
}

##############################
#Build Model
##############################
pre_process_model <- preProcess(movies, 
                                method = c("center", 
                                           "scale", 
                                           "knnImpute", 
                                           "BoxCox",
                                           "nzv"))

movies_new <- predict(pre_process_model,movies)
test_new <- predict(pre_process_model,test)

movies_new$revenue<-log(revenue)

#Create Forumla
rhs<-paste(names(movies)[cor_indices],collapse = " + ")
f<-as.formula(paste("revenue ~ budget+popularity+(budget*popularity)+", rhs, collapse =""))

#Build model
trControl <- trainControl(method = "LGOCV")
mod <- train(f, 
             data = movies_new, 
             trControl = trControl, 
             method = "lm")

#Estimate RMSE
print(paste("Estimated RMSE: ",round(mod$results[2],3)," +/- ",round(mod$results[5],3),sep=""))

#Print out predictions to file
pred <- predict(mod,newdata = test_new)
submit <- data.frame(id=test_id, revenue = exp(pred))
write.csv(submit,file=paste("MBO_Final_submission_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv",sep=""),row.names = FALSE)

