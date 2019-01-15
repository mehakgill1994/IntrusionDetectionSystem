#loading the dataset
train.20 <- read.csv("kddtrain_20percent.csv")
train <- read.csv("KDDTraining_set.csv")
test <- read.csv("kdd_test.csv")
fieldNames <- read.csv("field_names_nsl_kdd.csv")

#removing row spacing in field names
fieldNames <- fieldNames[-seq(2, nrow(fieldNames), 2),]

#assiging the names to the columns 
colnames(train.20) <- c(as.character(fieldNames$ï..Column1.1), "attack_type")
colnames(train) <- c(as.character(fieldNames$ï..Column1.1), "attack_type")
colnames(test) <- c(as.character(fieldNames$ï..Column1.1), "attack_type")

#creating a sample of 2000 rows
smple <- train[sample(nrow(train), "2000"),]

#function to reduce the complexity of dataset to only 4 major classes instead of 24
fun_type <- function(i){
  
  if((i == 'neptune') || (i == 'pod') || (i == 'smurf') || (i == 'teardrop') || (i == 'back') || (i == 'land')){
    result <- 'dos'
    return(result)
  }else if((i == 'nmap') || (i == 'portsweep') || (i == 'satan') || (i == 'ipsweep')){
    result <- 'probe'
    return(result)
    
  }else if((i == 'phf') || (i == 'spy') || (i == 'warezclient') || (i == 'warezmaster') || (i == 'imap') || (i == 'ftp_write') || (i == 'guess_passwd') || (i == 'multihop')){
    result <- 'r2l'
    return(result)
    
  }else if((i == 'perl') || (i == 'rootkit') || (i == 'buffer_overflow') || (i == 'loadmodule')){
    result <- 'u2r'
    return(result)
    
  }else if(i == 'normal'){
    result <- 'normal'
    return(result)
    
  }
}

attack_class <- NULL
for(i in 1:NROW(train.20)){
  attack_class <- c(attack_class, fun_type(train.20[i,42]))
}

train.20 <- cbind(train.20, attack_class)

#function to further reduce the dataset to only 2 classes i.e. Intrusion or Normal
fun_typee <- function(i){
  
  if((i == 'normal')){
    result <- 'normal'
    return(result)
  }else {
    result <- 'intrusion'
    return(result)
    
  }
}

intrusion <- NULL
for(i in 1:NROW(train.20)){
  intrusion <- c(intrusion, fun_typee(train.20[i,44]))
}

train.20 <- cbind(train.20, intrusion)

#changing the values of last column to 0 or 1
train.20$intrusion <- factor(train.20$intrusion, levels = c("normal", "intrusion"), labels = c(0,1))
train.20$intrusion <- as.factor(train.20$intrusion)

#deleting the unnecessary columns
train.20 <- train.20[,-c(42,43,44)]

#adding column to store predictions & index of rows
row_num <- c(seq(1:nrow(train.20)))
predictions <- NULL
predictions[1:nrow(train.20)] <- 2
train.20 <- cbind(train.20, predictions)
train.20 <- cbind(train.20, row_num)

#deleting the columns with no information gain
#library(FSelector)
#y <- gain.ratio(intrusion~., data = train.20, unit = "log2") == 0
#y
#train.20 <- train.20[,-which(y)]

#1st iteration
#v <- NULL
#v[1:nrow(train.20)] <- "NA"
library(FSelector)
values <- gain.ratio(intrusion~., data = train.20, unit = "log2")
attr <- train.20[,which(values == max(values))]

library(ggplot2)
ggplot(train.20[,], aes(x = attr, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(train.20)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")

#split the dataset
dataset0 <- NULL
dataset1 <- NULL
for(i in 1:nrow(train.20)){
  if(train.20[i,which(values == max(values))] == 0){
    train.20[i,ncol(train.20)-1] <- 1
    dataset0 <- rbind(dataset0, train.20[i,])
  }
  else if(train.20[i,which(values == max(values))] == 1){
    train.20[i,ncol(train.20)-1] <- 0
    dataset1 <- rbind(dataset1, train.20[i,])
  }
}

#confusion matrix and accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#2nd Iteration
#dataset0
values <- gain.ratio(intrusion~., data = dataset0, unit = "log2")
values <- values[1:(ncol(dataset0)-3),]
attr <- dataset0[,which(values == max(values))]

ggplot(dataset0[,], aes(x = (attr==0), fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset0)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")


dataset00 <- NULL
dataset01 <- NULL
for(i in 1:nrow(dataset0)){
  if(dataset0[i,which(values == max(values))] != 0){
    train.20[dataset0[i,ncol(dataset0)],ncol(train.20)-1] <- 0
    dataset01 <- rbind(dataset01, dataset0[i,])
  }
  else{
    train.20[dataset0[i,ncol(dataset0)],ncol(train.20)-1] <- 1
    dataset00 <- rbind(dataset00, dataset0[i,])
  }
}


#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#dataset1
values <- gain.ratio(intrusion~., data = dataset1, unit = "log2")
values <- values[1:(ncol(dataset1)-3),]
attr <- dataset1[,which(values == max(values))]

ggplot(dataset1, aes(x = attr==1, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset1)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")

dataset10 <- NULL
dataset11 <- NULL
for(i in 1:nrow(dataset1)){
  if(dataset1[i,which(values == max(values))] == 0 || dataset1[i,which(values == max(values))] == 2){
    train.20[dataset1[i,ncol(dataset1)],ncol(train.20)-1] <- 0
    dataset10 <- rbind(dataset10, dataset1[i,])
  }
  else if(dataset1[i,which(values == max(values))] == 1){
    train.20[dataset1[i,ncol(dataset1)],ncol(train.20)-1] <- 1
    dataset11 <- rbind(dataset11, dataset1[i,])
  }
}

#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#3rd iteration
#dataset00
values <- gain.ratio(intrusion~., data = dataset00, unit = "log2")
values <- values[1:(ncol(dataset00)-3),]
attr <- dataset00[,which(values == max(values))]

ggplot(dataset00[,], aes(x = attr, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset00)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")


dataset000 <- NULL
dataset001 <- NULL
for(i in 1:nrow(dataset00)){
  if(dataset00[i,which(values == max(values))] == 0 || dataset00[i,which(values == max(values))] == 0.25 || dataset00[i,which(values == max(values))] == 0.26 || dataset00[i,which(values == max(values))] == 0.50 || dataset00[i,which(values == max(values))] == 0.51 || dataset00[i,which(values == max(values))] == 0.52 || dataset00[i,which(values == max(values))] == 0.53 || dataset00[i,which(values == max(values))] == 0.54 || dataset00[i,which(values == max(values))] == 0.55 || dataset00[i,which(values == max(values))] == 1.00){
    train.20[dataset00[i,ncol(dataset00)],ncol(train.20)-1] <- 1
    dataset001 <- rbind(dataset001, dataset00[i,])
  }
  else{
    dataset000 <- rbind(dataset000, dataset00[i,])
  }
}

#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#dataset01
values <- gain.ratio(intrusion~., data = dataset01, unit = "log2")
values <- values[1:(ncol(dataset01)-3),]
attr <- dataset01[,which(values == max(values))]

ggplot(dataset01[,], aes(x = attr>0.50, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset01)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")


dataset010 <- NULL
dataset011 <- NULL
for(i in 1:nrow(dataset01)){
  if(dataset01[i,which(values == max(values))] >= 0.5){
    train.20[dataset01[i,ncol(dataset01)],ncol(train.20)-1] <- 1
    dataset011 <- rbind(dataset011, dataset01[i,])
  }
  else{
    train.20[dataset01[i,ncol(dataset01)],ncol(train.20)-1] <- 0
    dataset010 <- rbind(dataset010, dataset01[i,])
  }
}

#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#dataset10
values <- gain.ratio(intrusion~., data = dataset10, unit = "log2")
values <- values[1:(ncol(dataset10)-3),]
attr <- dataset10[,which(values == max(values))]

ggplot(dataset10[,], aes(x = attr, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset10)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")


dataset100 <- NULL
dataset101 <- NULL
for(i in 1:nrow(dataset10)){
  if(dataset10[i,which(values == max(values))] >= 0.5){
    train.20[dataset10[i,ncol(dataset10)],ncol(train.20)-1] <- 1
    dataset101 <- rbind(dataset101, dataset01[i,])
  }
  else{
    train.20[dataset01[i,ncol(dataset01)],ncol(train.20)-1] <- 0
    dataset010 <- rbind(dataset010, dataset01[i,])
  }
}

#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy


#dataset11
values <- gain.ratio(intrusion~., data = dataset11, unit = "log2")
values <- values[1:(ncol(dataset11)-3),]
attr <- dataset11[,which(values == max(values))]

ggplot(dataset11[,], aes(x = attr, fill = intrusion)) +
  geom_bar() +
  ggtitle("Intrusion Detection") +
  xlab(colnames(dataset11)[which(values == max(values))]) +
  ylab("Total Count") +
  labs(fill = "Intrusion")


dataset110 <- NULL
dataset111 <- NULL
for(i in 1:nrow(dataset11)){
  if(dataset11[i,which(values == max(values))] == 0 || dataset11[i,which(values == max(values))] == 1){
    train.20[dataset11[i,ncol(dataset11)],ncol(train.20)-1] <- 0
    dataset110 <- rbind(dataset110, dataset11[i,])
  }
  else if(dataset11[i,which(values == max(values))] == 2 || dataset11[i,which(values == max(values))] == 3){ 
    train.20[dataset11[i,ncol(dataset11)],ncol(train.20)-1] <- 1
    dataset111 <- rbind(dataset111, dataset11[i,])
  }
}


#confusion matrix & accuracy
table(train.20$intrusion, train.20$predictions)
accuracy <- sum(diag(prop.table(table(train.20$intrusion, train.20$predictions)))) * 100
accuracy









#____________________________________________________________#

#partition sample data
pd <- sample(2, nrow(train.20), replace = TRUE, prob = c(0.8, 0.2))
trainn <- train.20[pd==1,]
testt <- train.20[pd==2,]


#decision tree with party
library(party)
tree <- ctree(intrusion~src_bytes+dst_bytes, data = train.20)
tree
plot(tree)

#rpart
library(rpart)
tree2 <- rpart(intrusion~src_bytes+dst_bytes, data = train.20)
library(rpart.plot)
rpart.plot(tree2, extra = 2)

#prediction
predict(tree2)
tab <- table(predict(tree2, type = "class"), train.20$intrusion)
tab

library(rpart)
x <- cbind(train.20$src_bytes, train.20$v)
fit <- rpart(train.20$v ~ ., data = train.20,method="class")

library(ggplot2)
ggplot(train.20[1:891,], aes(x = protocol_type, fill = u)) +
  geom_bar() +
  facet_wrap(~serror_rate) +
  ggtitle("Intrusion Detection") +
  xlab("Attribute") +
  ylab("Total Count") +
  labs(fill = "Intrusion")

x <- train.20[,which(gain.ratio(u~., train.20, unit = "log2") == 0)]
x



e <- NULL
for(i in 1:(ncol(train.20)-3))
{
  library(entropy)
  e <- c(e, entropy(as.numeric(train.20[,i]), train.20$v))
}
e



















