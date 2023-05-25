############方法一 單形法(二階段法) ###############
cx<-c(1,1,1,1,1,1,1,  # 目標函數之係數(決策變數)
      0,0,0,0,0,0,0,  # 剩餘變數(surplus variable)
      0,0,0,0,0,0,0)  # 人工變數(artificial variable)
a1<-rbind(                # 限制式之係數(含所有變數)
  c(1,0,0,1,1,1,1,-1,rep(0,6),1,rep(0,6)),
  c(1,1,0,0,1,1,1,0,-1,rep(0,5),0,1,rep(0,5)),
  c(1,1,1,0,0,1,1,0,0,-1,rep(0,4),0,0,1,rep(0,4)),
  c(1,1,1,1,0,0,1,0,0,0,-1,rep(0,3),0,0,0,1,rep(0,3)),
  c(1,1,1,1,1,0,0,0,0,0,0,-1,rep(0,2),0,0,0,0,1,rep(0,2)),
  c(0,1,1,1,1,1,0,0,0,0,0,0,-1,rep(0,1),0,0,0,0,0,1,rep(0,1)),
  c(0,0,1,1,1,1,1,0,0,0,0,0,0,-1,0,0,0,0,0,0,1))
b1<-c(17,13,15,19,14,16,11)     # >= 的限制條件值
P<-sum(cx*rep(0,length(cx)))   # 目標函數初始值
DV<-1:7         # 決策變數(結構變數)位置
M<- 15:21       # 人工變數位置
NB<-1:14        # 非基本變數代號
B<-15:21        # 初始基本變數代號
n<-length(cx)      # 目標函數變數個數
st<- rbind(a1)     # 單形表(simplex tableau)
Xs<- paste0('x',1:n)    # 各變數命名
colnames(st)<- paste0(Xs,'(',cx,')')        # 單形表行名
st<-cbind(st,b1)        # 單形表(simplex tableau)
rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
print(st)               # 列印初始單形表

###### 第一階段(求極大化)####
cx1<-cx         # 原目標函數之係數備份(計算目標函數值及第二階段還原後使用)
cx[]<- 0              # 目標函數之係數歸零
cx[M]<- -1            # 人工變數的係數指定
st<- rbind(a1)        # 單形表(simplex tableau)
Xs<- paste0('x',1:n)  # 各變數命名
colnames(st)<- paste0(Xs,'(',cx,')')        # 單形表行名
st<-cbind(st,b1)      # 單形表(simplex tableau)
rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
while (TRUE){
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基底向量檢定數
  delta<-rep(0,length(cx))     # 初始檢定子(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  pc<- which.min(delta)        # 樞紐行號(取最小值)
  ratio<-st[,ncol(st)]/st[,pc] # 技術矩陣比值
  ratio<-replace(ratio,ratio<0,NA)  # 技術矩陣比值忽略<0
  ratio<-replace(              # 非人工變數=0者除外
    ratio,
    which(!(B %in% M) & ratio==0),NA)
  fbs<-solve(a1[,B])%*%b1          # 求基本可行解
  fbs<-replace(rep(0,n),B,fbs[,1]) # 求基本可行解
  names(fbs)<- paste0('x',1:n)     # 賦予基本解變數名
  P<-sum(cx1*fbs)                  # 階段目標函數值
  cat('\n基本可行解',fbs,'\n')     # 列印基本可行解
  cat('階段目標函數值',P,'\n')     # 列印第一階段目標函數值
  print(cbind(           # 列印第一階段單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  pr<-which(ratio== min(ratio, na.rm = TRUE))[1] # 樞紐列號(取最小值)
  cat('樞紐行、列:',pc,',',pr,'\n')     # 列印第一階段樞紐行、列號
  if(!any(M %in% B)){       #  判斷是否人工變數均從基底裡去除
    if (!all(fbs[M]==0)){   # ratio比值皆無>=0時跳出迴圈
      stop('此題無解 !')
    }
    cat('已達去除人工變數\n',    # 列印第一階段目標函數值
        '第一階段目標函數值：',P,'\n')
    break
  }
  ### 單形表重整 ###
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+    # 高斯喬登消去法(陣列)
    matrix(-st[-pr,pc]/prow[pc])%*%prow
  B<-replace(B,pr,pc)      # 非基本變數代號取代基本變數代號
  NB<-(1:n)[-B]            # 底基本變數代號取代非基本變數代號
  rownames(st)<- paste0(Xs[B],'(',cx[B],')') #單形表列名(基底變數代號)
}
####### 第二階段(求極大化)###############
NB<-NB[!NB %in% M]        # 去除人工變數
cx<--cx1[-M]       # 原目標函數之係數變號求極大值(去除人工變數)
n<-length(cx)      # 目標函數變數個數
st<- st[,-M]       # 第二階段技術矩陣
rownames(st)<- paste0(Xs[B],'(',cx[B],')')    # 單形表列名
colnames(st)[1:n]<- paste0(Xs[-M],'(',cx,')') # 單形表行名
while (TRUE){
  fbs<-solve(a1[,B])%*%b1  # 求基本解
  fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
  names(fbs)<- paste0('x',1:n)
  P<--sum(cx*fbs)              # 階段目標函數值
  cat('\n基本可行解',fbs,'\n')   # 列印基本可行解
  cat('目標函數值',P,'\n')   # 列印階段目標函數值
  
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基本向量檢定數
  delta<-rep(0,length(cx))     # 初始檢定子(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  print(cbind(           # 列印初始單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  if(all(delta>=0)){        # 判別數均無負數則結束迴圈
    cat('\n已達最佳解\n',    # 列印第二階段目標函數值
        '第二段目標函數值：',P,'\n')
    print('已達最佳解')
    break
  }
  pc<- which.min(delta)  # 樞紐行號(取最小值)
  ratio<-st[,ncol(st)]/st[,pc] # 技術矩陣比值
  ratio<-replace(ratio,ratio<=0,NA)  # 技術矩陣比值忽略<=0
  pr<-nrow(st)-                     # 樞紐列號(取最小值)
    which(rev(ratio)== min(ratio, na.rm = TRUE))[1]+
    1
  theta<-ratio[pr]       # theta(最小比值)
  ### 單形表重整 ###
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+
    matrix(-st[-pr,pc]/prow[pc])%*%prow   # 高斯喬登消去法(陣列)
  B<-replace(B,pr,pc)      # 非基本變數代號取代基本變數代號
  NB<-(1:n)[-B]            # 基本變數代號取代非基本變數代號
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
}
cat('原始條件:',b1,'\n')   # 列印原始限制條件
cat('限制條件解: ',a1[,DV]%*%fbs[DV],'\n')   # 列印限制條件解
##########  多組解 ###########
oB<-B
pcs<-c()
while (!all(delta[NB]!=0)){
  pc<-NB[which(delta[NB]==0)] # 樞紐行只限delta=0的非基本變數
  pc<-rev(pc[!pc%in%pcs & !pc%in%oB])[1] # 避免無窮迴圈(依序由後往前)
  if (is.na(pc)){     # 無符合之樞紐行則跳出迴圈
    cat('已得多重最佳解 !','\n')
    break}
  pcs<-c(pcs,pc)
  ratio<-st[,ncol(st)]/st[,pc] # 技術矩陣比值
  ratio<-replace(ratio,ratio<=0,NA)  # 技術矩陣比值忽略<=0
  pr<-nrow(st)-                     # 樞紐列號
    which(rev(ratio)== min(ratio, na.rm = TRUE))[1]+1
  theta<-ratio[pr]       # theta(最小比值)
  ### 單形表重整 ###
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+
    matrix(-st[-pr,pc]/prow[pc])%*%prow   # 高斯喬登消去法(陣列)
  B<-replace(B,pr,pc)      # 非基變數代號取代基變數代號
  NB<-(1:n)[-B]            # 基變數代號取代非基變數代號
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
  ##################
  fbs<-solve(a1[,B])%*%b1  # 求基本解
  fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
  names(fbs)<- paste0('x',1:n)
  P<--sum(cx*fbs)              # 階段目標函數值
  cat('\n基本可行解',fbs,'\n')   # 列印基本可行解
  cat('初始目標函數值',P,'\n')   # 列印階段目標函數值
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基向量檢定數
  delta<-rep(0,length(cx))     # 初始檢定子(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  print(cbind(           # 列印初始單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  cat('原始條件:',b1,'\n')   # 列印原始限制條件
  cat('限制條件解: ',a1[,DV]%*%fbs[DV],'\n')   # 列印限制條件解
}
########## 方法二:lpSolveAPI ###############
# 題目，求目標函數最小值(最佳解):
# C = x1 + x2 + x3 +x4 + x5 + x6 + x7
# x1+            x4 + x5 + x6 + x7  ≥ 17
# x1 + x2+            x5 + x6 + x7  ≥ 13
# x1 + x2 + x3+            x6 + x7  ≥ 15
# x1 + x2 + x3 + x4+            x7  ≥ 19
# x1 + x2 + x3 + x4 + x5            ≥ 14
#      x2 + x3 + x4 + x5 + x6       ≥ 16
#           x3 + x4 + x5 + x6 + x7  ≥ 11
# x1 >= 0 
# x2 >= 0
# x3 >= 0
# x4 >= 0 
# x5 >= 0
# x6 >= 0
# x7 >= 0
library(lpSolveAPI) # 載入線性規劃函式庫lpSolveAPI
lprec <- make.lp(  # 建立一新線性規劃model物件，函式說明請參閱R線上說明
  7, 7)  # 此model具7個限制條件7個結構變數求解
# 程式執行至此可先print(lprec) 初步檢查model內容
lp.control( # 線性規劃模式，設定其相關控制參數，函式說明請參閱R線上說明
  lprec=lprec,   # 對象線性規劃model物件
  sense='min',simplextype=c('dual','primal'))  # 設定此model取最小值
set.column( # 設定model欄限制條件各係數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  column=1, # 此model第1欄
  x=c(1,1,1,1,1,0,0)) # 此欄各限制條件值(對應上述7個條件)
set.column( # 同上
  lprec,    # 同上
  column=2, # 此model第2欄
  x=c(0,1,1,1,1,1,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=3, # 此model第3欄
  x=c(0,0,1,1,1,1,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=4, # 此model第4欄
  x=c(1,0,0,1,1,1,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=5, # 此model第5欄
  x=c(1,1,0,0,1,1,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=6, # 此model第6欄
  x=c(1,1,1,0,0,1,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=7, # 此model第7欄
  x=c(1,1,1,1,0,0,1)) # 同上
set.objfn(  # 設定model的目標函數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  c(1,1,1,1,1,1,1))    # 目標函數各係數
# 給予各條件名稱
rownames <- c('Mon.','Tue.','Wed.','Thu.','Fri.','Sat.','Sun.')
colnames <- c('x1','x2','x3',  # 給予各係數行(結構變數)名稱
              'x4','x5','x6','x7') 
dimnames(lprec) <- list( # 將model變數欄及條件欄重新命名，方便閱讀
  rownames,   
  colnames)
set.constr.value( # 設定限制值，函式說明請參閱R線上說明
  lprec,  # 此model物件 
  rhs=c(17,13,15,19,14,16,11), # 限制值(Right Hand Side)
  constraints=1:7) # 7個限制條件
set.constr.type( # 設定限制型態(方向)，函式說明請參閱R線上說明
  lprec,  # 此model物件
  types=c('>=', '>=', '>=','>=','>=','>=','>='),  # 限制型態(方向)
  constraints=1:7) # 7個限制條件
# 程式執行至此可先print(lprec) 檢查model完整內容
print(lprec)  # 將model變數欄及條件欄已重新命名
solve(lprec)  # 將此model 求解，函式說明請參閱R線上說明
get.objective(lprec) # 讀出目標函數最佳解
get.variables(lprec) # 讀出目標函數最佳解之各值(依欄順序顯示)
get.constraints(lprec) # 讀出各限制條件式右側(rhs)之結果(依列順序)
library(pracma)
cat('最佳解之各值進位取整數(x1~x7): ',ceil(get.variables(lprec)),'\n')
cat('取進位整數解合計(人數):',sum(ceil(get.variables(lprec))),'\n')
################### end of 7_8.R #######################