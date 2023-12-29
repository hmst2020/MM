(A<-cbind(           # 建立一矩陣表示網頁互相連結之關係
  c(0,1,1,1),
  c(0,0,1,1),
  c(1,0,0,0),
  c(1,0,1,0)
))
colnames(A)<-c(1,2,3,4)   # 為每一行命名
rownames(A)<-c(1,2,3,4)   # 為每一列命名
(B<-sweep(         # 將每行以行加總的連結數平均其連外之機率
  x=A,             # A 矩陣
  MARGIN=2,        # 以行總數均分
  STATS=colSums(A),  # 各行總計數
  FUN='/'))          # 以除的方式分配出去
############# 方法 一################
p <- 0.85               # 阻尼係數(damping factor）
n <- nrow(B)           # 環境總網頁數
(E<- matrix(1,n,n))    # 每一元素皆為1的nxn矩陣
(M <- p*B+ (1-p)/n*E)  # 阻尼係數調整後的轉移(遞移)矩陣
(eig<-eigen(M))        # 特徵分解
idx<- 1                # 主要特徵值位置
(v<-eig$vectors[,idx]) # 主要特徵值對應的特徵向量
as.numeric(v/sum(v))   # 縮放尺度使總數為1
############# 方法 二################
library(markovchain)
initStates <- c('1','2','3','4')     # 初始狀態各名稱
(markovB <- new(       # 建構一新的物件
  'markovchain',       # 物件類別
  states=initStates,   # 狀態各稱
  byrow=FALSE,         # 轉移機率逐列否
  transitionMatrix=M,  # 指定遞移矩陣
  name='馬可夫鏈物件'))  # 給予物件名稱
(ss<-steadyStates(markovB))  # 計算穩定分布解
############# 方法 三################
(v0 <- rep(1/n,n))    # 初始向量代表初始的PR值(此處給予均分)
for (i in 0:300) {     
  w <- M %*% v0         # 迭代轉換
  vRatios<-v0/min(v0)   # 迭代前比例
  wRatios<-w/min(w)     # 迭代後比例
  if (isTRUE(all.equal(wRatios,vRatios))){  # 判斷迭代前後是否相同
    break    # 若相同，結束迴圈
  }
  v0<-w      # 給予迭代值
}
print(i)                 # 穩定狀態經過迭代次數
print(v0)                # 穩定狀態時的PR(機率分布)
################### end of 3_22.R ##################