############ 單形法(二階段法) ###############
cx<-c(20,8,10,12,22,18,  # 目標函數決策變數(decision variable)之係數
      0,0,                   # 閒置變數(slack variable)之係數
      0,0,0,                 # 剩餘變數(surplus variable)之係數
      0,0,0)                 # 人工變數(artificial variable)之係數
a1<-rbind(                     # 限制式之係數(含所有變數)
  c(1,1,1,0,0,0,1,0,rep(0,3),rep(0,3)),
  c(0,0,0,1,1,1,0,1,rep(0,3),rep(0,3)),
  c(1,0,0,1,0,0,rep(0,2),-1,0,0,1,0,0),
  c(0,1,0,0,1,0,rep(0,2),0,-1,0,0,1,0),
  c(0,0,1,0,0,1,rep(0,2),0,0,-1,0,0,1)
)
b1<-c(400,600,200,300,400)     # <= 及>= 的限制條件值
P<-sum(cx*rep(0,length(cx)))   # 目標函數初始值
M<- c(12,13,14)        # 人工變數位置
NB<-c(1:6,9:11)        # 非基本變數代號
B<-c(7,8,12:14)        # 初始基本變數代號
n<-length(cx)       # 目標函數變數個數
st<- rbind(         # 單形表(simplex tableau)
  a1)
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
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基本向量檢定數
  delta<-rep(0,length(cx))     # 初始檢定子(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  pc<- which.min(delta)        # 樞紐行號(取最小值)
  ratio<-st[,ncol(st)]/st[,pc] # 技術矩陣比值
  ratio<-replace(ratio,ratio<0,NA)  # 技術矩陣比值忽略<0
  ratio<-replace(              # 忽略=0的非人工變數
    ratio,
    which(!(B %in% M) & ratio==0),NA)
  pr<-nrow(st)-                     # 樞紐列號
    which(rev(ratio)== min(ratio, na.rm = TRUE))[1]+
    1
  #pr<-which(ratio== min(ratio, na.rm = TRUE))[1]
  fbs<-solve(a1[,B])%*%b1          # 求基本可行解
  fbs<-replace(rep(0,n),B,fbs[,1]) # 求基本可行解
  names(fbs)<- paste0('x',1:n)     # 賦予基本解變數名
  P<-sum(cx1*fbs)                  # 階段目標函數值
  cat('\n基本可行解',fbs,'\n')     # 列印基本可行解
  cat('階段目標函數值',P,'\n')     # 列印第一階段目標函數值
  print(cbind(           # 列印初始單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  if(!any(M %in% B)){    #  判斷是否人工變數均從基向量裡去除
    if (!all(fbs[M]==0)){
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
  st[-pr,]<-st[-pr,]+    # 高斯消去法(陣列)
    matrix(-st[-pr,pc]/prow[pc])%*%prow   
  NB<-replace(NB,pc,B[pr]) # 基本變數代號取代非基本變數代號
  B<-replace(B,pr,pc)      # 非基本變數代號取代基本變數代號
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
}
####### 第二階段(求極大化)###############
NB<-NB[!NB %in% M]        # 去除人工變數
cx<--cx1[-M]       # 原目標函數之係數變號求極大值(去除人工變數)
n<-length(cx)     # 目標函數變數個數
st<- st[,-M]      # 第二階段技術矩陣
rownames(st)<- paste0(Xs[B],'(',cx[B],')')    # 單形表列名
colnames(st)[1:n]<- paste0(Xs[-M],'(',cx,')') # 單形表行名
while (TRUE){
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基向量檢定數
  delta<-rep(0,length(cx))     # 初始檢定子(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  pc<- which.min(delta)  # 樞紐行號(取最小值)
  ratio<-st[,ncol(st)]/st[,pc] # 技術矩陣比值
  ratio<-replace(ratio,ratio<=0,NA)  # 技術矩陣比值忽略<=0
  pr<-nrow(st)-                     # 樞紐列號
    which(rev(ratio)== min(ratio, na.rm = TRUE))[1]+
    1
  theta<-ratio[pr]       # theta(最小比值)
  fbs<-solve(a1[,B])%*%b1  # 求基本解
  fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
  names(fbs)<- paste0('x',1:n)
  P<--sum(cx*fbs)                # 階段目標函數值
  cat('\n基本可行解',fbs,'\n')   # 列印基本可行解
  cat('初始目標函數值',P,'\n')   # 列印階段目標函數值
  print(cbind(           # 列印初始單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  if(all(delta>=0)){
    cat('\n已達最佳解\n',    # 列印第二階段目標函數值
        '第二段目標函數值：',P,'\n')
    print('已達最佳解')
    break
  }
  ### 單形表重整 ###
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+
    matrix(-st[-pr,pc]/prow[pc])%*%prow   # 高斯消去法(陣列)
  NB<-replace(NB,pc,B[pr]) # 基本變數代號取代非基本變數代號
  B<-replace(B,pr,pc)  # 非基本變數代號取代基本變數代號
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
  ##################
}
########## 方法二:lpSolveAPI ###############
# 題目，求目標函數最小值(最佳解):
# C = 20 x1 + 8x2 + 10x3 +12 x4 +22 x5 +18 x6
# x1 + x2 + x3               ≤ 400
#              x4 + x5 + x6  ≤ 600
# x1+          x4            ≥ 200
#      x2+          x5       ≥ 300
#           x3+          x6  ≥ 400
# x1 >= 0 
# x2 >= 0
# x3 >= 0
# x4 >= 0 
# x5 >= 0
# x6 >= 0
library(lpSolveAPI) # 載入線性規劃函式庫lpSolveAPI
lprec <- make.lp(  # 建立一新線性規劃model物件，函式說明請參閱R線上說明
  5, 6)  # 此model具5個限制條件6個結構變數求解
# 程式執行至此可先print(lprec) 初步檢查model內容
lp.control( # 線性規劃模式，設定其相關控制參數，函式說明請參閱R線上說明
  lprec=lprec,   # 對象線性規劃model物件
  sense='min')   # 設定此model取最小值
set.column( # 設定model欄限制條件各係數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  column=1, # 此model第1欄
  x=c(1,0,1,0,0)) # 此欄各限制條件值(對應上述5個條件)
set.column( # 同上
  lprec,    # 同上
  column=2, # 此model第2欄
  x=c(1,0,0,1,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=3, # 此model第3欄
  x=c(1,0,0,0,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=4, # 此model第4欄
  x=c(0,1,1,0,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=5, # 此model第5欄
  x=c(0,1,0,1,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=6, # 此model第6欄
  x=c(0,1,0,0,1)) # 同上
set.objfn(  # 設定model的目標函數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  c(20,8,10,12,22,18))    # 目標函數各係數
# 給予各條件名稱
rownames <- c('capacity 1', 'capacity 2',
              'WH A','WH B','WH C')
colnames <- c('F1-WA','F1-WB','F1-WC',  # 給予各係數行(結構變數)名稱
              'F2-WA','F2-WB','F2-WC') 
dimnames(lprec) <- list( # 將model變數欄及條件欄重新命名，方便閱讀
  rownames,   
  colnames)
set.constr.value( # 設定限制值，函式說明請參閱R線上說明
  lprec,  # 此model物件 
  rhs=c(400,600,200,300,400), # 限制值(Right Hand Side)
  constraints=1:5) # 五個限制條件
set.constr.type( # 設定限制型態(方向)，函式說明請參閱R線上說明
  lprec,  # 此model物件
  types=c("<=", "<=", ">=",">=",">="),  # 限制型態(方向)
  constraints=1:5) # 五個限制條件
# 程式執行至此可先print(lprec) 檢查model完整內容
print(lprec)  # 將model變數欄及條件欄已重新命名
solve(lprec)  # 將此model 求解，函式說明請參閱R線上說明
get.objective(lprec) # 讀出目標函數最佳解
get.variables(lprec) # 讀出目標函數最佳解之各值(依欄順序顯示)
get.constraints(lprec) # 讀出各限制條件式右側(rhs)之結果(依列順序)
################### end of 7_7.R #######################
