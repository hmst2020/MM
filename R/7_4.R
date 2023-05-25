####### 單形法基本過程 ###############
cx<-c(20,12,18,0,0,0)     # 目標函數之係數(決策變數+閒置變數)
a1<-rbind(             # <= 限制式之係數(含閒置變數)
  c(3,1,2,1,0,0),
  c(2,3,1,0,1,0),
  c(1,2,3,0,0,1))
b1<-c(9,8,7)          # <= 的限制條件值
NB<-c(1,2,3)          # 非基本變數代號
B<-c(4,5,6)           # 初始基本變數代號
n<-length(cx)         # 目標函數變數個數
st<- rbind(a1)        # 單形表(simplex tableau)
Xs<- paste0('x',1:n)  # 各變數命名
colnames(st)<- paste0(Xs,'(',cx,')')        # 單形表行名
st<-cbind(st,b1)  # 單形表(simplex tableau)
rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
print(st)         # 列印初始單形表

fbs<-solve(a1[,B])%*%b1           # 求初始基本解
fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
names(fbs)<- paste0('x',1:n)      # 賦予基本解變數名
print(fbs)        # 列印基本可行解
P<-sum(cx*fbs)    # 目標函數值
print(P)          # 列印初始目標函數值

while (TRUE){
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基本向量判別數
  delta<-rep(0,length(cx))     # 初始判別數(全部歸零)
  delta[NB]<-dnb               # 判別數
  if(all(dnb>=0)){             # 判別數均無負數則結束迴圈
    print('已達最佳解')
    rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
    print(cbind(           # 列印最終單形表
      rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)))
    break}
  pc<- which.min(delta)  # 樞紐行號(取最大值)
  ratio<-st[,ncol(st)]/st[,pc]      # 技術矩陣比值
  ratio<-replace(ratio,ratio<0,NA)  # 技術矩陣比值忽略<0
  pr<-which.min(ratio)   # 樞紐列號
  theta<-ratio[pr]       # theta(最小比值)
  P<- P-theta*(            # 累進目標函數值
    sum(cx[B]%*%st[,pc])-cx[pc])
  print(cbind(           # 列印單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)))
  NB<-replace(NB,pc,B[pr]) # 基本變數代號取代非基變數代號
  B<-replace(B,pr,pc)      # 非基本變數代號取代基變數代號
  fbs<-solve(a1[,B])%*%b1           # 求基本解(basic solution)
  fbs<-replace(rep(0,n),B,fbs[,1])  # 基本解變數對應
  names(fbs)<- paste0('x',1:n)      # 基本解變數名稱
  cat('\n基本可行解',fbs,'\n')          # 列印基本可行解
  cat('目標函數值(加總基本可行解*係數):',
      sum(cx*fbs),'\n')   # 列印目標函數值
  cat('累進目標函數值(P):',P,'\n')   # 列印累進目標函數值
  ###### 單形表重整 #########
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+    # 高斯消去法(陣列的基本列運算)
    matrix(-st[-pr,pc]/prow[pc])%*%prow
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
}

########### end of 7_4.R ###################