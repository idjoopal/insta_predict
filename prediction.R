# 사용자한테 입력 받아서 예측

# 예측할 데이터 만듬.
makedata <- function(){
  count <- 0
  follower<-0
  n<-0
  while(follower<1){
    follower <- readline("enter your followers number: ")
    follower <- ifelse(grepl("\\D",n),-1,as.integer(follower))
  }
  datanames<- attr(insta_n2.trainingdata[2:1591], "names")
  tags <- insta_n2.trainingdata[1,c("follower.y", datanames)]
  tags[,datanames] <- 0
  while(count >= 0){
    n <- readline("enter your hashtag: ")
    #n <- ifelse(grepl("\\D",n),-1,as.character(n))
    if(n==""){break}  # breaks when hit enter
    
    tags[1, n] <- 1
    
  }
  tags$follower.y <- follower
  
  # follower 정규화 
  tags$follower.y <- tags$follower.y /  max(insta2$follower.y)
  
  #print(d)
  return(tags)
}
expect <- function(){
  
  datas<-makedata()
  
  pr_my <- predict.lm(insta_n2.training.lm, datas)
  pr_my
  print("Your expected likes : ")
  pr_my[1]
  my_conp <- datas[, c(1:2)]
  my_conp <- cbind(my_conp, pr_my)
  my_conp[, 3] <- sapply(my_conp[, 3], as.integer)
  print(abs(my_conp[, 3]))
}


expect()
