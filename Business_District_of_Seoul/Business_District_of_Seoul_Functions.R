# install.packages("geosphere")

#### 함수들 ###

library(geosphere)

distance <- function(df){
  # 경위도를 넣으면 거리를 m단위로 계산해주는 함수
  # 데이터프레임을 넣되 1열은 경도, 2열은 위도가 들어가야 함
  dd <- data.frame(0)
  for(i in 1:nrow(df)){
    if(i==nrow(df)) i <- nrow(df)-1
    else{
      for(j in (i+1):nrow(df)){
        dd[i,j] <- distGeo(df[i,], df[j,])
      }
    }
  }
  return(dd)
}



dist.points <- function(xy.1, xy){
  # 몇 개의 점들(xy.1=n개 행의 data.frame)과 여러 점들(xy=data.frame) 간의 거리를 계산해주는 함수
  # nrow(xy.1) * nrow(xy)의 데이터프레임으로 출력
  # 경위도
  dist.1 <- data.frame(NA)
  for(i in 1:nrow(xy.1)){
    for(j in 1:nrow(xy)){
      df <- rbind(xy.1[i,], xy[j,])
      dist.1[i,j] <- distance(df)[2]
    }
  }
  return(dist.1)
}

dist.points.2 <- function(xy.1, xy){
  # 몇 개의 점들(xy.1=n개 행의 data.frame)과 여러 점들(xy=data.frame) 간의 거리를 계산해주는 함수
  # nrow(xy.1) * nrow(xy)의 데이터프레임으로 출력
  dist.1 <- data.frame(NA)
  for(i in 1:nrow(xy.1)){
    for(j in 1:nrow(xy)){
      df <- rbind(xy.1[i,], xy[j,])
      dist.1[i,j] <- dist(df)
    }
  }
  return(dist.1)
}



dot <- function(a,b,r,n){
  # 반지름이 겹치지 않는 임의의 점을 찾아내는 함수
  # a:가로길이, b:세로길이, r:반지름, n:점의 개수
  repeat{
    x <- runif(n, r, a-r)
    y <- runif(n, r, b-r)
    dot1 <- cbind(x,y)
    if(all(dist(dot1) > 2*r)==1) break
  }
  return(dot1)
}

dots <- function(r, n, min.x, max.x, min.y, max.y){
  # 반지름이 겹치지 않는 임의의 점을 찾아내는 함수
  # 최대최소x, 최대최소y를 지정해주어야 함 (경위도 좌표계)
  # 경위도 좌표로 출력
  r.1 <- 0.001*r*0.4/44.4709
  r.2 <- 0.001*r*0.4/92.81628
  repeat{
    x <- runif(n, min.x+r.1, max.x-r.1)
    y <- runif(n, min.y+r.2, max.y-r.2)
    dot1 <- cbind(x,y)
    dd <- distance(dot1)
    dd[1,1] <- 2*r+1
    dd[is.na(dd)] <- 2*r+1
    if(all(dd > 2*r)==1) break
  }
  return(dot1)
}


a <- 100
b <- 100
r <- 10
circle2(a,b,r)
circle2 <- function(a,b,r)
{
  set <- data.frame("위도"=c(runif(2000,r,b-r)),"경도"=c(runif(2000,r,a-r))) # x와 y는 해당 영역에 임의의 2000개의 점을 뽑았음. 이 점에 대해서 겹치지 않는 점을 고를 예정
  set_real <- data.frame("위도"=0,"경도"=0) # 겹치지 않는 점은 따로 이곳에 저장
  i<-1 # 시행횟수임과 동시에 set_real에 차례대로 저장하기 위해서 i를 정의
  repeat
  {
    if (nrow(set) == 1)
    {
      set_real[i,] <- set[1,]
      break
    } # else에 의해 set의 nrow가 1일때 마지막 남은 점을 set_real에 저장
    else if (nrow(set) == 0)
    {
      break
    } # 이게 없으면 무한 반복됨..
    else
    {
      set <- set[-c(which(dist(set)[1:nrow(set)-1]<2*r)+1),]
      # set의 첫번째 점과 나머지 999개의 점과의 거리를 생각했을 때 2r보다 작은 점을 지우는 작업
      set_real[i,] <- set[1,];set <- set[-1,]
      # 그리고 첫번째 점을 set_real의 i번재 행에 저장하고 set에서는 저장한 점을 제거함.
      i <- i+1
    }
  }
  return(set_real)
}


circle <- function(r, min.x, max.x, min.y, max.y)
{
  r.1 <- 0.001*r*0.4/44.4709
  r.2 <- 0.001*r*0.4/92.81628
  
  set <- data.frame("long"=c(runif(2000, min.x+r.1, max.x-r.1)),
                    "lat"=c(runif(2000, min.y+r.2, max.y-r.2)))
  # x와 y는 해당 영역에 임의의 2000개의 점을 뽑았음. 이 점에 대해서 겹치지 않는 점을 고를 예정
  set_real <- data.frame("long"=0,"lat"=0) # 겹치지 않는 점은 따로 이곳에 저장
  i <- 1 # 시행횟수임과 동시에 set_real에 차례대로 저장하기 위해서 i를 정의
  repeat
  {
    if (nrow(set) == 1)
    {
      set_real[i,] <- set[1,]
      break
    } # else에 의해 set의 nrow가 1일때 마지막 남은 점을 set_real에 저장
    else if (nrow(set) == 0)
    {
      break
    } # 이게 없으면 무한 반복됨..
    else
    {
      set <- set[-c(which(dist.points(set[i,], set[-i,]) < 2*r)+1),] # set의 첫번째 점과 나머지 999개의 점과의 거리를 생각했을 때 2r보다 작은 점을 지우는 작업
      set_real[i,] <- set[1,]; set <- set[-1,] # 그리고 첫번째 점을 set_real의 i번재 행에 저장하고 set에서는 저장한 점을 제거함.
      i <- i+1
    }
  }
  if( is.na(set_real[nrow(set_real),1]) ){
    set_real <- set_real[-nrow(set_real),]
  }
  else next
  return(set_real)
}



# 점 그리는 함수 이용하여 점 그리기 #
dots <- rep(NA, 150)
for(i in 1:400){
  dot.1 <- circle(200, block.1[i,]$min.long, block.1[i,]$max.long,
                  block.1[i,]$min.lat, block.1[i,]$max.lat)
  nas <- rep(NA, 150-nrow(dot.1))
  dot.2 <- cbind(c(dot.1[,1], nas), c(dot.1[,2], nas))
  dots <- cbind(dots, dot.2)
  cat("\n", i)
}

setwd("C:/Users/Hyein/Desktop/Tobigs/컨퍼런스 프로젝트/200m점")
write.table(dots, "dots_1_400.txt", row.names=F)

## 불러오기 ##
dots <- read.table("dots.txt", header=T)
dots[1:5,1:5]
dot.1 <- dots[, -1]
dot.list <- list()
j <- 1 # 시작한 번호 ( for문 돌리기 시작한 번호)
for (i in 1:(ncol(dot.1)/2)){
  do <- dot.1[,c(i*2-1, i*2)]
  do.1 <- do[complete.cases(do),]
  colnames(do.1) <- c("long", "lat")
  dot.list[[j-1+i]] <- do.1
}



# 불러오기 2 ## 형식 다름
dot.list.2 <- list()
for( i in 1:1622){
  name <- paste0("data", i, ".txt")
  data <- read.table(name, header=T, sep=",")[,2:3]
  ind <- complete.cases(data)
  dot.list.2[[i]] <- data[ind,]
}





######## write 함수 ########
# 리스트 내 데이터프레임 각각 내보내기 #
# 새 폴더 만들어서 wd 지정한 후 사용하는 것이 좋음 #

write.list <- function(list){
  for(i in 1:length(list)){ # 리스트 개수
    data.1 <- list[[i]]
    data.name <- paste0("data", i, ".txt")
    write.table(data.1, data.name, row.names=F)
  }
}

write <- function(list){
  for(i in 1:length(list)){ # 리스트 개수
    if( length(li[[i]])!=0 ){
      for(j in 1:length(list[[i]])){
        if( length(li[[i]][[j]])!=0 ){
          data.1.1 <- list[[i]][[j]]
          data.name <- paste0(i, "_",  j, ".txt")
          write.table(data.1.1, data.name, row.names=F)
        }
      }
    }
  }
}






##################################
### 데이터 정리 작업 ####

setwd("C:/Users/Hyein/Desktop/Tobigs/컨퍼런스 프로젝트/위치")
dongs <- list.files()
str(dongs)
dongs.1 <- gsub("\\s위치.txt", "", dongs)
dongs.1



read.loc <- function(n){
  
  gu.dong <- dongs.1[grep(gu[n], dongs.1)]
  gu.list.loc.n <- list(NULL)
  setwd("C:/Users/Hyein/Desktop/Tobigs/컨퍼런스 프로젝트/위치")
  dongs <- list.files()
  for(i in 1:length(gu.dong)){ # i <- 1
    data <- dongs[grep(gu[n], dongs)][i]
    if( !is.na(data) ){
      gu.list.loc.n[[i]] <- read.table(data, header=T, stringsAsFactors=F)
      
    }
    else gu.list.loc.n[[i]] <- NA
    cat("\n", i)
  }
  names(gu.list.loc.n) <- gu.dong
  return (gu.list.loc.n)
}

gu.list.loc.22 <- read.loc(22)
gu.list.loc.23 <- read.loc(23)
gu.list.loc.24 <- read.loc(24)
gu.list.loc.25 <- read.loc(25)



#################3

setwd("C:/Users/Hyein/Desktop/Tobigs/컨퍼런스 프로젝트/위치")
dongs <- list.files()
str(dongs)
dongs.1 <- gsub("\\s위치.txt", "", dongs)

dongs.1
n<-23
read.store <- function(n){
  
  gu.dong <- dongs.1[grep(gu[n], dongs.1)]
  gu.list.store.n <- list(NULL)
  setwd("C:/Users/Hyein/Desktop/Tobigs/컨퍼런스 프로젝트/dongsss")
  numdatalist <- list.files()
  for(i in 1:length(gu.dong)){ # i <- 61
    guname <- unlist(strsplit(gu.dong[i], "\\s"))[2]
    dongname <- unlist(strsplit(gu.dong[i], "\\s"))[3]
    dongblock <- block.1$name[grep(substr(dongname, 1,2), block.1$name)]
    if( length(dongblock)==0){
      gu.list.store.n[[i]] <- NA
    }
    else{
      data <- numdatalist[grep(dongblock[1], numdatalist)]
      if( length(data)>1){
        data <- data[grep(guname, data)]
        if( length(data)>1){
          dongnm <- paste0("_", dongname)
          data <- data[grep(dongnm, data)]
          if( length(data)>1){
            data <- data[1]
          }
        }
        gu.list.store.n[[i]] <- read.table(data, header=T, stringsAsFactors=F)
      }
      else {
        gu.list.store.n[[i]] <- read.table(data, header=T, stringsAsFactors=F)
      }
    }
    names(gu.list.store.n)[i] <- paste(guname, dongname, sep="_")
    cat("\n", i)
  }
  return(gu.list.store.n)
}

gu.list.store.22 <- read.store(22)
gu.list.store.23 <- read.store(23)
gu.list.store.24 <- read.store(24)
gu.list.store.25 <- read.store(25)



bind <- function(n, gu.list.loc.n, gu.list.store.n){
  gu.dong <- dongs.1[grep(gu[n], dongs.1)]
  gu.list.bind.n <- list()
  
  for(i in 1:length(gu.dong) ){
    dongname <- unlist(strsplit(gu.dong[i], "\\s"))[3]
    dongblock <- block.1$name[grep(substr(dongname, 1,2), block.1$name)]
    
    if( length(gu.list.store.n[[i]])==1 | length(gu.list.loc.n[[i]])==1 ){
      gu.list.bind.n[[i]] <- NA
    }
    else{
      start <- which(colnames(gu.list.store.n[[i]])=="V1")
      end <- ncol(gu.list.store.n[[i]])
      if(nrow(gu.list.loc.n[[i]]) != nrow(gu.list.store.n[[i]][,start:end])){
        gu.list.bind.n[[i]] <- NA
      }
      else {
        gu.list.bind.n[[i]] <- cbind(gu.list.loc.n[[i]], gu.list.store.n[[i]][,start:end])
      }
      
    }
    cat("\n", i)
    names(gu.list.bind.n)[i] <- names(gu.list.store.n)[i]
  }
  return(gu.list.bind.n)
}

names(gu.list.store.n[[i]][,start:end]) <- dongblock #################


gu.list.bind.22 <- bind(22, gu.list.loc.22, gu.list.store.22)
gu.list.bind.23 <- bind(23, gu.list.loc.23, gu.list.store.23)
gu.list.bind.24 <- bind(24, gu.list.loc.24, gu.list.store.24)
gu.list.bind.25 <- bind(25, gu.list.loc.25, gu.list.store.25)



block.store.list<-list()

make.list <- function(n, gu.list.bind.n){
  for(i in 1:length(gu.list.bind.n)){
    gu.dong <- dongs.1[grep(gu[n], dongs.1)]
    guname <- unlist(strsplit(gu.dong[i], "\\s"))[2]
    dongname <- unlist(strsplit(gu.dong[i], "\\s"))[3]
    dongblock <- block.1$name[grep(substr(dongname, 1,2), block.1$name)]
    # j <- 2
    
    if( !is.na(gu.list.bind.n[[i]]) ){
      for(j in 1:(ncol(gu.list.bind.n[[i]])-3)){
        storelist <- gu.list.bind.n[[i]][,c(1:3, j+3)]
        if( sum(storelist[,4]==1)!=0 ) {
          storelist.1 <- storelist[which(storelist[,4]==1),]
          storelist.2 <- storelist.1[,-4]
          ind <- which(block.1$name==dongblock[j])
          block.store.list[[ind]] <- storelist.2
          cat("\n", i, "-", j)
        }
      }
    }
  }
  return(block.store.list)
}

block.store.list <- make.list(23, gu.list.bind.23)
block.store.list <- make.list(24, gu.list.bind.24)
block.store.list <- make.list(25, gu.list.bind.25)





li <- list(NULL)
for(i in 1:1618){
  list.1 <- list(NULL)
  if( !is.null(block.store.list[[i]]) ){ # 가게가 하나라도 있으면
    if( nrow(dot.list[[i]]) > 0){
      dist.1 <- dist.points(dot.list[[i]], block.store.list[[i]][,2:3])
      for( j in 1:nrow(dot.list[[i]]) ){ # j <- 1 
        ind <- dist.1[j,] < 100
        list.1[[j]] <- block.store.list[[i]]$BSSH_NM[ind]
        li[[i]] <- list.1
        cat("\n", i, "-", j)
      }
    }
    else {
      list.1[[1]] <- block.store.list[[i]]$BSSH_NM
      li[[i]] <- list.1
    }
  }
  #  else{
  #    li[[i]] <- list.1
  #  }
}




clust.6.list <- list()
k <- 1

for(i in 1:length(clust.6)){
  lth <- length(li[[ clust.6[i] ]])
  if( lth==0 ) next
  else {
    for(j in 1:lth){
      if( length(li[[ clust.6[i] ]][[j]])!=0 ){
        clust.6.list[[k]] <- li[[ clust.6[i] ]][[j]]
        k <- k +1
        cat("\n", i, j)
      }
      else next
    }
  }
}

write.list(clust.6.list)

unique(clust.1.list[[1]])

unq <- function(clust.n.list){
  for( i in 1: length(clust.n.list)){
    clust.n.list[[i]] <- unique(clust.n.list[[i]])
  }
  return(clust.n.list)
}
clust.1.list.unq <- unq(clust.1.list)
clust.2.list.unq <- unq(clust.2.list)
clust.3.list.unq <- unq(clust.3.list)
clust.4.list.unq <- unq(clust.4.list)
clust.5.list.unq <- unq(clust.5.list)
clust.6.list.unq <- unq(clust.6.list)


library(arules)

c.1.trans <- as(clust.1.list.unq, 'transactions')
