library(reticulate)
library(pacman)
p_load(data.table,tidyverse,TTR,magrittr,parallel,plyr,gcookbook,ggplot2,TTR)
#py_install("ta") # 安装python中ta库

#==============基于AO-动量震荡指标（Awesome Oscillator）的择时策略函数==================
#超参：短周期计算价格中点价格线根数short，长周期计算价格中点的价格线根数long
buyTimingAO <- function(x,hyperPars=c(short,long)){
  py$short=hyperPars[1];py$long=hyperPars[2]
  py_run_file("D:/Quant/Rcode/Indicators/AO(Awesome Oscillator)/AO_py.py")
  stg <- x[,{
    AO <- py$ao
    buySig <- ifelse((AO>0 & (AO-shift(AO,1))>0 & shift(AO,1)>0 & (shift(AO,1)-shift(AO,2))<0)|
                       (AO>0 & shift(AO,1)<0)|
                       (AO<0 & (shift(AO,1)-AO)<0 & shift(AO,2)<0 & (shift(AO,2)-shift(AO,1))>0 & (shift(AO,3)-shift(AO,2))<0),1,0)
    # （1）碟型买入，（2）穿越买入，（3）双峰买入
    sellSig <- ifelse((AO<0 & AO<shift(AO,1) & shift(AO,1)<0 & shift(AO,1)>shift(AO,2))|
                       (AO<0 & shift(AO,1)>0)|
                       (AO>0 & shift(AO,1)>AO & shift(AO,2)>0 & shift(AO,2)<shift(AO,1) & shift(AO,3)>shift(AO,2)),1,0)
    #同形状相反则卖出
    list(date,op,cl,buySig,sellSig)
  }] 
  return(stg)
}
#stg2 <- buyTimingAO(pr,c(5,34))


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
#Search <-function(lowerHyper=c(hyper1,hyper2,hyper3),upperHyper=c(hyper1,hyper2,hyper3)){
  #lowerHyper[i],upperHyper[i]分别为第i个超参的下界、上界
  n <- seq(lowerHyper[1],upperHyper[1],by=1)
  pow1 <- seq(lowerHyper[2],upperHyper[2],by=1)
  pow2 <- seq(lowerHyper[3],upperHyper[3],by=1)
  parSpace <- expand_grid(n,pow1,pow2) %>% data.table #生成参数搜索空间，一行对应一套参数
  Search <- parSpace[,{
    cat('n=',n,' pow1=',pow1,' pow2=',pow2,'\n')
    ppv <- evaluateppv(buyTimingKAMA(x=pr,hyperPars=c(n=n,pow1=pow1,pow2=pow2)))
    ppv
  },by=.(n,pow1,pow2)] #每个参数组合都检验，能暴力选出ppv最好的参数组合
  setnames(Search,'V1','ppv')
  Search  #查看参数组合以及其所对应的ppv值
  Best <- Search[which.max(ppv)]
  
  return(Best)    #输出最优参数组合
}
#Search(lowerHyper = c(10,2,25),upperHyper = c(15,10,35))

short <- seq(5,15,by=1)
long <- seq(30,90,by=1)

parSpace <- expand_grid(short,long) %>% data.table
hyperParSpace <- parSpace
S=10L  #分十组
fgs <- combn(S,floor(S/2),simplify = F) # 模型训练集的分R data.table组序号组合
fulldata <- pr %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签

X <- hyperParSpace[,{
  cat(' short=',short,' long=',long,'\n')
  stg <-buyTimingAO(x=pr,
                      hyperPars=c(short=short,long=long)) #首先在全数据上生成买入卖出信号
  
  fulldata <- stg %>% mutate(group=cut(1:nrow(pr),S) %>% as.integer()) #在标记买卖信号的全数据上加上组标签
  J.tubes <-fgs %>% map(~fulldata[group %in% .,]) # 生成含有所有训练集组合的列表
  Jc.tubes <- J.tubes %>% map(~ setdiff(fulldata,.)) #用差集的方式生成含有所有验证集组合的列表
  ppv.J <- map_dbl(J.tubes,~evaluateppv(.))
  ppv.Jc <- map_dbl(Jc.tubes,~evaluateppv(.))
  list(N=1:length(J.tubes),ppv.J=ppv.J,ppv.Jc=ppv.Jc)#不同N代表不同的训练集选择
  
},by=.(short,long)] # 针对每一个超参组合，计算全部训练集J和对应的全部验证集Jc上的ppv结果

#write.csv(X,"outcome.csv") #将训练结果写入csv文件


Ld <- X[,{
  fg <- which.max(ppv.J) # 确定训练集中最大的ppv对应的行号
  w <- ppv.Jc %>% percent_rank() %>% `[`(fg)
  w <- ifelse(w==1,0.999,ifelse(w==0,0.001,w))
  lambda <- log(w/(1-w))
  list(lambda=lambda)
  
},by=.(short,long)]
#write.csv(Ld,"Rank.csv") #将排名结果写入csv文件
#write.csv(Ld[which.max(lambda),],file = "OUTCOME.csv")
Fn.cdf <- Ld$lambda %>% ecdf #构建lambda的经验分布函数
PBO <- Fn.cdf(0) # 根据经验分布函数，得到过拟合概率
