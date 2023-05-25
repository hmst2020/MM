######### 十代後 使用expm套件之運算子##############
library(expm)  # 載入expm套件
rownames = c("2年以上大專教育", "2年以下大專教育")
colnames = c("2年以上大專教育", "2年以下大專教育")
(x0 <-                  # 目前教育程度分布
  matrix(      
  c(0.2,0.8),
  nrow =2,
  byrow =FALSE,
  dimnames = list(rownames, c('人口分布'))))
(T <- matrix(           # 建構遞移矩陣物件
  c(0.7,0.3,0.2,0.8),
  nrow=2, 
  byrow = FALSE,
  dimnames = list(rownames, colnames)))
n<-10                 # n年後
(T %^% n)%*% x0       # 以教育程度分布狀態計算10年後的分布

####### 求穩定分布解###############
library(markovchain)
statesNames = c("2年以上大專教育", "2年以下大專教育")
(markovB <- new(       # 建構一新的物件
  'markovchain',       # 物件類別
  states=statesNames,  # 狀態各名稱
  byrow=FALSE,         # 轉移機率逐列否
  transitionMatrix=T,  # 指定遞移矩陣
  name='馬可夫鏈物件'))  # 給予物件名稱
(ss<-steadyStates(markovB))  # 計算穩定分布解
############### end of 8_6.R ############