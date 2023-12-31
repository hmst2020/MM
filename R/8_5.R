####方法一########
A <-cbind(          # 建構方程式(去掉redundant)各系數之矩陣
  c(1,3,1),     
  c(1,-7,3),
  c(1,3,-6))
dimnames(A) <- list(    # 對矩陣給予列名、行名
  c('區域1','區域2','區域3'),c('區域1','區域2','區域3'))
print(A)           # 方程組係數
(B <-c(1,0,0))     # 方程式等號右邊常數
(iA <-solve(A))    # 求 A 之反矩陣
(x<- iA %*% B)     # 求本實例x值 
####方法二########
library(markovchain)
statesNames <- c('區域1','區域2','區域3')  # 各初始狀態名稱
T <-cbind(               # 建構遞移矩陣物件
  c(0.6,0.3,0.1),     
  c(0.4,0.3,0.3),
  c(0.3,0.3,0.4))
dimnames(T) <- list(     # 對矩陣給予列名、行名
  statesNames,statesNames)
(markovB <- new(       # 建構一新的物件
  'markovchain',       # 物件類別
  states=statesNames,  # 狀態各名稱
  byrow=FALSE,         # 轉移機率逐列否
  transitionMatrix=T,  # 指定遞移矩陣
  name='馬可夫鏈物件'))  # 給予物件名稱
(ss<-steadyStates(markovB))  # 計算穩定分布解
####方法三########
(eig<-eigen(T))          # 特徵分解
eig$vectors[,1]/sum(eig$vectors[,1])   # 主特徵向量比值
############# 方法 四################
library(expm)
(.Machine$double.eps^0.5) # all.equal 精度要求
(v0 <- c(0.80,0.15,0.05)) # 初始分布比例
for (i in 0:300) {     
  w <- T %*% v0         # 迭代轉換
  vRatios<-v0/min(v0)   # 迭代前比例
  wRatios<-w/min(w)     # 迭代後比例
  if (isTRUE(all.equal(wRatios,vRatios))){  # 判斷迭代前後是否相同
    break    # 若相同，結束迴圈
  }
  v0<-w      # 給予迭代值
}
print(i-1)               # 穩定狀態經過迭代次數
print(v0)                # 穩定狀態時的比例分布
print(T%^%(i-1))         # 極限矩陣
print(colSums(T%^%(i-1))) # 行向量和
############### end of 8_5.R ############