install.packages("arules")
library("arules")
library(car)

instagram1 <- read.csv("instadata/original.csv", header=T)
nonf <- read.csv("instadata/nonfollow.csv", header=T)

realdata <- instagram1[, c(1, 4, 8, 9:97)]
realdata[, c(1, 4:92)] <- sapply(realdata[, c(1, 4:92)], as.character)

length(realdata)
length(realdata$media.id)

list_tag <- NULL

for(j in 1:length(realdata$media.id)){
  for(i in 1:(length(realdata)-3)){
    if(realdata[j, i+3] == "" || is.null(realdata[j, i+3])){
      break
    }
    list_tag <- c(list_tag[1:length(list_tag)], realdata[j, i+3])
  }
}

list_tag[sapply(list_tag, is.null)]<-NULL
u_list_tag <- unique(list_tag)
tag_data <- realdata

for(i in 1:length(u_list_tag)){
  tag_data <- cbind(tag_data, 0)
  names(tag_data)[92+i] <- u_list_tag[i]
}

for(j in 1:length(tag_data$media.id)){
  for(k in 1:89){
    tag_name <- tag_data[j, 3+k]
    if(tag_name == "" || is.null(tag_name)){
      break
    }
    tag_data[j, tag_name] <- 1
   # tag_count[tag_name] <- tag_count[tag_name]+1
  }
}

final_data <- tag_data[, c(1:3,93:12869)]
final_data1 <- na.omit(final_data)

write.csv(final_data, "instadata/data2.csv")

#####?Œ”ë¡œì›Œ?ˆ˜ ?ˆ„?½ê°? ë³€ê²?
nonfollower <- read.csv("instadata/data5.csv", header=T)
f <- read.csv("instadata/nonfollow.csv", header=T)

nonfollower[, 1] <- sapply(nonfollower[, 1], as.character)
f[, 3] <- sapply(f[, 3], as.character)
colnames(f)[3] <- "media.id"

perf <- merge(f, nonfollower, by='media.id', all='TRUE')
perf$follower.y[is.na(perf$follower.y)] <- perf$follower.x[is.na(perf$follower.y)]
perf$i.x <- NULL
perf$follower.x <- NULL

complete_data <- na.omit(perf)


##### Start LM
insta <- complete_data
insta$media.id <-NULL

##### Do LM by Raw Data Set and See the LM graph
insta.lm <- lm(likes.count ~ follower.y , data=insta)
plot(insta$follower.y, insta$likes.count, xlab="follower", ylab="likes.count", xlim=c(0, 2000), ylim=c(0, 450))
abline(coef(insta.lm), col=2)

ind <-sample(2, nrow(insta), replace=TRUE, prob=c(0.7, 0.3))
insta.trainingdata <- insta[ind==1,]
insta.testdata <- insta[ind==2,]

insta.training.lm <- lm(likes.count ~ . , data=insta.trainingdata)
summary(insta.training.lm)
plot(insta.training.lm)
pr_like <- predict.lm(insta.training.lm, insta.testdata)

conp <- insta.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)

##### Remove (coefficient = NA) Hashtags and LM again
co <- insta.training.lm$coefficients
con <- na.omit(co)

rm_tag_list <- c("likes.count")
for(i in 2:length(attr(con, "names", TRUE))){
  rm_tag_list <- c(rm_tag_list[1:length(rm_tag_list)], attr(con, "names", TRUE)[i])
}

insta2 <- insta[, rm_tag_list]

ind <-sample(2, nrow(insta2), replace=TRUE, prob=c(0.7, 0.3))
insta.trainingdata <- insta2[ind==1,]
insta.testdata <- insta2[ind==2,]

insta.training.lm <- lm(likes.count ~ . , data=insta.trainingdata)
summary(insta.training.lm)
pr_like <- predict.lm(insta.training.lm, insta.testdata)

conp <- insta.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)

##### Normalize follower and LM again
insta_n <- insta2
insta_n$follower.y <- insta_n$follower.y / max(insta_n$follower.y)

ind <-sample(2, nrow(insta_n), replace=TRUE, prob=c(0.7, 0.3))
insta_n.trainingdata <- insta_n[ind==1,]
insta_n.testdata <- insta_n[ind==2,]

insta_n.lm <- lm(likes.count ~ . , data=insta_n)
co <- insta_n.lm$coefficients
con <- na.omit(co)

rm_tag_list <- c("likes.count")
for(i in 2:length(attr(con, "names", TRUE))){
  rm_tag_list <- c(rm_tag_list[1:length(rm_tag_list)], attr(con, "names", TRUE)[i])
}

insta_n <- insta_n[, rm_tag_list]
ind <-sample(2, nrow(insta_n), replace=TRUE, prob=c(0.7, 0.3))
insta_n.trainingdata <- insta_n[ind==1,]
insta_n.testdata <- insta_n[ind==2,]
insta_n.training.lm <- lm(likes.count ~ . , data=insta_n.trainingdata)
summary(insta_n.training.lm)
pr_like <- predict.lm(insta_n.training.lm, insta_n.testdata)

conp <- insta_n.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)

##### Normalize follower and Remove NA Tags
co <- insta_n.training.lm$coefficients
con <- na.omit(co)

rm_tag_list <- c("likes.count")
for(i in 2:length(attr(con, "names", TRUE))){
  rm_tag_list <- c(rm_tag_list[1:length(rm_tag_list)], attr(con, "names", TRUE)[i])
}

insta_n2 <- insta_n[, rm_tag_list]

##### Normalized data LM again
ind <-sample(2, nrow(insta_n2), replace=TRUE, prob=c(0.7, 0.3))
insta_n2.trainingdata <- insta_n2[ind==1,]
insta_n2.testdata <- insta_n2[ind==2,]

insta_n2.training.lm <- lm(likes.count ~ . , data=insta_n2.trainingdata)
summary(insta_n2.training.lm)
pr_like <- predict.lm(insta_n2.training.lm, insta_n2.testdata)
conp <- insta_n2.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)

plot(insta_n2.training.lm)
##### Remove Tags which used only 1 time
tag_count <- c(0:0)
for(j in 1:length(tag_data$media.id)){
  for(k in 1:89){
    tag_name <- tag_data[j, 3+k]
    if(tag_name == "" || is.null(tag_name)){
      break
    }
    tag_data[j, tag_name] <- 1
    if(is.na(tag_count[tag_name])) tag_count[tag_name] <-1
    else tag_count[tag_name] <- tag_count[tag_name]+1
  }
}

### ÇÑ¹ø ¾²ÀÎ ÅÂ±× »èÁ¦ ##
tag_count2 <- tag_count[tag_count>1,drop=FALSE] 
names <- attr(tag_count2, "names")
final_data3 <- final_data[,names]

## data °¡ÁßÄ¡ ºÎ¿©, Á¤±ÔÈ­ ##
final_data4 <- final_data3
final_data4 <- t(final_data3) / (tag_count2)
final_data5 <- t(final_data4)
final_data5 <- cbind(final_data[,1:3], final_data3)

write.csv(final_data5, "instadata/data5.csv")

###  lmÀÛ¾÷ÇÏ±â Àü°úÁ¤
###  followµµ Á¤±ÔÈ­ ÇßÀ½
###  NA³ª¿À´Â°Åµµ Á¦°Å
insta_n.lm <- lm(likes.count ~ . , data=insta_n)

co <- insta_n.lm$coefficients
con <- na.omit(co)

rm_tag_list <- c("likes.count")
for(i in 2:length(attr(con, "names", TRUE))){
  rm_tag_list <- c(rm_tag_list[1:length(rm_tag_list)], attr(con, "names", TRUE)[i])
}

insta_n <- insta_n[, rm_tag_list]

ind <-sample(2, nrow(insta_n), replace=TRUE, prob=c(0.7, 0.3))
insta_n2.trainingdata <- insta_n[ind==1,]
insta_n2.testdata <- insta_n[ind==2,]

## ÆÈ·Î¿ö, ÅÂ±× Á¤±ÔÈ­
insta_n2.training.lm <- lm(likes.count ~ (.)^2 , data=insta_n2.trainingdata)
summary(insta_n2.training.lm)
pr_like <- predict.lm(insta_n2.training.lm, insta_n2.testdata)
conp <- insta_n2.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)

## ÆÈ·Î¿ö¸¸ Á¤±ÔÈ­
insta_n.training.lm <- lm(likes.count ~ . , data=insta_n.trainingdata)
summary(insta_n.training.lm)
pr_like <- predict.lm(insta_n.training.lm, insta_n.testdata)
conp <- insta_n.testdata[, c(1:2)]
conp <- cbind(conp, pr_like)
conp[, 3] <- sapply(conp[, 3], as.integer)





plot(insta_n2.training.lm)


makedata <- function(){
  count <- 0
  follower<-0
  n<-0
  tags<-c(0:0)
  while(follower<1){
    follower <- readline("enter your followers number: ")
    follower <- ifelse(grepl("\\D",n),-1,as.integer(follower))
  }
  tags<-
    tags["follower.y"] <- follower
  
  while(count >= 0){
    n <- readline("enter your hashtag: ")
    #n <- ifelse(grepl("\\D",n),-1,as.character(n))
    if(n==""){break}  # breaks when hit enter
    
    tags[n] <- 1
    
  }
  return(tags)
}
datas<-makedata()

d <- data.frame(datas)
d <- t(d)
