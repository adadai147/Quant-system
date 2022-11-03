library(reticulate)
library(pacman)
p_load(data.table,tidyverse,TTR,magrittr,parallel,plyr,gcookbook,ggplot2,TTR)
#py_install("ta") # 安装python中ta库


##=== 准备运行策略所需的数据 =========
pr <- fread('D:/codes/HQfd.csv') # 读取深证ETF 原始数据(优矿基金数据下载)
pr <- pr[,.(date=tradeDate,cl=closePrice,op=openPrice,adj=accumAdjFactor
            ,high=highestPrice,low=lowestPrice)] # 裁出所需数据
pr <- pr[,c('cl','op','high','low'):={
  list(cl*adj,op*adj,high*adj,low*adj)
}]
# 将价格转为后复权价格，保证计算的一致性

#==============择时策略函数==================
#超参：指定日期最高价high，指定日期最低价low，指定日期收盘价cl
buyTiming <- function(x,hyperPars=c(nf,ns,cycle,smooth1,smooth2)){
  py$nf=hyperPars[1];py$ns=hyperPars[2];py$cycle=hyperPars[3]
  py$smooth1=hyperPars[4];py$smooth2=hyperPars[5]
  
  py_run_file("D:/codes/STC/STC_py.py")
  stg <- x[,{
    stc <- py$stc
    buySig <- ifelse(stc<sig1 & shift(stc,1)>sig1,1,0)
    sellSig <- ifelse(stc>sig2 & shift(stc,1)<sig2,1,0)
    list(date,op,cl,buySig,sellSig)
  }] 
  stg[is.na(stg)] <- 1 #原矩阵中1错误变为NA，进行转换
  return(stg)
}

#==========================================ppv计算函数==============================================
evaluateppv <- function(stgy){
  loc<-(which(stgy[,'buySig']==1))
  stgyb<-stgy[loc,]
  
  loc<-(which(stgy[,'sellSig']==1))
  stgys<-stgy[loc,]
  
  list1 <- list()
  list1[[1]] <- data.frame(t(stgyb$op))
  list1[[2]] <- data.frame(t(stgys$cl))
  u <- rbind.fill(list1)
  u <- t(u)
  profit <- na.omit(log(u[,2])-log(u[,1])) #根据卖出当天收盘价与买入当天开盘价取对数后作差计算盈利
  ppv <- length(profit[profit>0])/length(profit)  #计算查准率
  
  return(ppv)
}
#====================================================================================================
nf <- seq(23,23,by=1)
ns <- seq(50,50,by=1)
cycle <- seq(10,10,by=1)
smooth1 <- seq(23,23,by=1)
smooth2 <- seq(50,50,by=1)
sig1 <- seq(15,35,by=1)
sig2 <- seq(60,85,by=1)

parSpace <- expand_grid(nf,ns,cycle,smooth1,smooth2,sig1,sig2) %>% data.table
hyperParSpace <- parSpace
S=10L  #分十组

fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分R data.table组序号组合
fulldata <- pr %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签

X <- hyperParSpace[,{
  cat('nf=',nf,' ns=',ns,' cycle=',cycle,' smooth1=',smooth1,' smooth2=',smooth2,' sig1=',sig1,' sig2=',sig2,'\n')
  stg <-buyTiming(x=pr,
                  hyperPars=c(nf=nf,ns=ns,cycle=cycle,smooth1=smooth1,smooth2=smooth2)) #首先在全数据上生成买入卖出信号
  fulldata <- stg %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签
  J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成含有所有训练集组合的列表
  Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成含有所有验证集组合的列表
  ppv.J <- map_dbl(J.tubes,~evaluateppv(.))
  ppv.Jc <- map_dbl(Jc.tubes,~evaluateppv(.))
  ppv.Jc[is.na(ppv.Jc)]<-0
  list(N=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)#不同N代表不同的训练集选择
},by=.(nf,ns,cycle,smooth1,smooth2,sig1,sig2)] # 针对每一个超参组合，计算全部训练集J和对应的全部验证集Jc上的ppv结果

#write.csv(X,"outcome.csv") #将训练结果写入csv文件


Ld <- X[,{
  fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
  w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
  w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
  lambda <- log(w/(1-w))
  list(lambda=lambda)         
},by=.(nf,ns,cycle,smooth1,smooth2,sig1,sig2)]
#write.csv(Ld,"Rank.csv") #将排名结果写入csv文件

Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率