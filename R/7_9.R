########### 方法一 對偶問題法(Dual Problem method) ##############
# 題目，求目標函數極小值(最佳解)之對偶題極大值(最佳解):
# Max C = 100y1 + 80y2 + 120y3
# 5y1+3y2+y3  <= 2
#    +y2      <= 0.5
# 2y1+5y2+3y3 <= 2.5
#          y3 <= 0.3
# 3y1+2y2+2y3 <= 1.75
# y1          <= 0.35
# 2y1+y2+6y3  <= 2
# y1,y2,y3,y4,y5,y6,y7 >= 0
cx<-c(c(100,80,120),rep(0,7))  # 目標函數之係數
at<-rbind(                # 限制式之係數(僅結構變數)
  c(5,0,2,0,3,1,2),
  c(3,1,5,0,2,0,1),
  c(1,0,3,1,2,0,6))
a1<-cbind(t(at),diag(1,7)) # 技術矩陣(含閒置變數)
b1<-c(2, 0.5, 2.5, 0.3, 1.75, 0.35, 2) # 目標函數之係數
S<-4:10         # 閒置變數代號
NB<-1:3         # 非基本變數代號
B<-4:10         # 初始基本變數代號
n<-length(cx)       # 目標函數變數個數
st<- rbind(a1)      # 單形表(simplex tableau)
Xs<- paste0('y',1:n)  # 各變數命名
colnames(st)<- paste0(Xs,'(',cx,')')        # 單形表行名
st<-cbind(st,b1)  # 單形表(simplex tableau)
rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
fbs<-solve(a1[,B])%*%b1           # 求初始基本解
fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
names(fbs)<- paste0('x',1:n)      # 賦予基本解變數名
P<-sum(cx*fbs)    # 目標函數值
cat('初始基本可行解：',fbs,'\n')  # 列印基本可行解
cat('初始目標函數值：',P,'\n')    # 列印初始目標函數值
print(st)         # 列印初始單形表
while (TRUE){
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基向量判別數
  delta<-rep(0,length(cx))     # 初始判別數(全部歸零)
  delta[NB]<-dnb               # 判別數
  if(all(dnb>=0)){             # 判別數均無負數則結束迴圈
    cat('\n已達最佳解')
    rownames(st)<- paste0(Xs[B],'(',cx[B],')')   # 單形表列名
    cat('\n基本可行解(閒置變數)',delta[-S],'\n') # 列印基本可行解
    cat('基本可行解(結構變數)',delta[S],'\n')   # 列印基本可行解
    cat('目標函數值(加總基本可行解*係數):',
        sum(cx*fbs),'\n')   # 列印目標函數值
    cat('累進目標函數值(P):',P,'\n')   # 列印累進目標函數值
    print(cbind(           # 列印最終單形表
      rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
    ))
    break
  }
  pc<- which.min(delta)  # 樞紐行號(取最大值)
  ratio<-st[,ncol(st)]/st[,pc]      # 技術矩陣比值
  ratio<-replace(ratio,ratio<0,NA)  # 技術矩陣比值忽略<0
  ratio<-replace(              # 忽略=0的非閒置變數
    ratio,
    which(!(B %in% S) & ratio==0),NA)
  cat('\n')
  cat('基本可行解：',fbs,'\n')       # 列印基本可行解
  cat('累進目標函數值：',P,'\n')     # 列印目標函數值
  print(cbind(           # 列印單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  pr<-which.min(ratio)   # 樞紐列號
  theta<-ratio[pr]       # theta(最小比值)
  P<- P-theta*(          # 累進目標函數值
    sum(cx[B]%*%st[,pc])-cx[pc])
  B<-replace(B,pr,pc)      # 非基本變數代號取代基本變數代號
  NB<-(1:n)[-B]            # 基本變數代號取代非基本變數代號
  fbs<-solve(a1[,B])%*%b1           # 求基本解(basic solution)
  fbs<-replace(rep(0,n),B,fbs[,1])  # 基本解變數對應
  names(fbs)<- paste0('x',1:n)      # 基本解變數名稱
  ###### 單形表重整 #########
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+    # 高斯消去法(陣列的基本列運算)
    matrix(-st[-pr,pc]/prow[pc])%*%prow
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
}
########### 方法二 對偶單形法(Dual-simplex method)#############
# 題目，求目標函數極小值(最佳解)之對偶題極大值(最佳解):
# Max C = 2x1 + 0.5x2 + 2.5x3 + 0.3x4 + 1.75x5 + 0.35x6 + 2x7
# (-5)x1+       (-2)x3+       (-3)x5+(-1)x6+(-2)x7  <= -100
# (-3)x1+(-1)x2+(-5)x3+       (-2)x5+       (-1)x7  <= -80
# (-1)x1+       (-3)x3+(-1)x4+(-2)x5+       (-6)x7  <= -120
# x1,x2,x3,x4,x5,x6,x7 >= 0 
cx<- c(-2, -0.5, -2.5, -0.3, -1.75, -0.35, -2) # 目標函數之係數
a1<-rbind(                # 限制式之係數(含所有變數)
  c(-5,-0,-2,0,-3,-1,-2),
  c(-3,-1,-5,0,-2,0,-1),
  c(-1,0,-3,-1,-2,0,-6)
)
b1<-c(-100,-80,-120)         # >= 的限制條件值
m<-nrow(a1)         # 限制式個數(單形表列數)
n<-length(cx)+m     # 單形表行數(結構變數個數+閒置變數個數)
NB<-1:length(cx)    # 非基本變數代號
B<-(length(cx)+1):n # 初始基本變數代號
cx<-c(cx,rep(0,m))  # 目標函數依閒置變數個數，增加=0的係數
a1<-cbind(a1,diag(m))  # 技術矩陣依閒置變數個數擴充
st<- a1             # 單形表(simplex tableau)
Xs<- paste0('x',1:(n-m))  # 各變數命名(結構變數)
Xs<- c(Xs,paste0('s',1:m))     # 各變數命名(閒置變數)
colnames(st)<- paste0(Xs,'(',cx,')')        # 單形表行名
st<-cbind(st,b1)          # 單形表(simplex tableau)
rownames(st)<- paste0(Xs[B],'(',0,')')  # 單形表列名
print(st)         # 列印初始單形表
fbs<-solve(a1[,B])%*%b1           # 求初始基本解
fbs<-replace(rep(0,n),B,fbs[,1])  # 求基本解(basic solution)
names(fbs)<- paste0('x',1:n)      # 賦予基本解變數名
dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基向量判別數
delta<-rep(0,length(cx))     # 初始判別數(全部歸零)
delta[NB]<-dnb               # 最佳解檢定子
pr<- which.min(st[,'b1'])    # 樞紐列號(取負值中最小)
ratio<-delta/st[pr,1:n] # 技術矩陣比值
ratio<-replace(         # 技術矩陣比值忽略<=0
  ratio,(is.nan(ratio) | ratio>=0),NA)  
pc<-which.max(ratio)    # 樞紐行號(取最大值)
theta<-st[,'b1'][pr]    # theta 值
P<-sum(cx*fbs)    # 目標函數值
cat('初始基本可行解：',-fbs,'\n')  # 列印基本可行解(變號)
cat('初始目標函數值：',-P,'\n')    # 列印初始目標函數值(變號)
print(rbind(           # 列印初始單形表
  rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
))

while (TRUE){
  theta<-st[,'b1'][pr]     # theta(初始解最小值)
  #P<- P+theta*ratio[pc]   # 累進目標函數值
  P<- P+theta*(            # 累進目標函數值
      (sum(cx[B]%*%st[,pc])-cx[pc])/st[pr,pc])
  B<-replace(B,pr,pc)      # 非基本變數代號取代基本變數代號
  NB<-(1:n)[-B]            # 基本變數代號取代非基本變數代號
  ###### 單形表重整 #########
  pe<-st[pr,pc]          # 樞紐元素
  st[pr,]<-st[pr,]/pe    # 樞紐列基本列運算
  prow<-st[pr,]          # 樞紐列
  st[-pr,]<-st[-pr,]+    # 高斯消去法(陣列的基本列運算)
    matrix(-st[-pr,pc]/prow[pc])%*%prow
  rownames(st)<- paste0(Xs[B],'(',cx[B],')')  # 單形表列名
  ############################
  fbs<-solve(a1[,B])%*%b1           # 求基本解(basic solution)
  fbs<-replace(rep(0,n),B,fbs[,1])  # 基本解變數對應
  names(fbs)<- Xs                   # 基本解變數名稱
  cat('\n基本可行解',fbs,'\n')          # 列印基本可行解
  cat('目標函數值(加總基本可行解*係數):',
      -sum(cx*fbs),'\n')   # 列印目標函數值(變號)
  cat('累進目標函數值(P):',P,'\n')   # 列印累進目標函數值
  dnb<-cx[B]%*%st[,NB]-cx[NB]  # 計算非基底向量判別數
  delta<-rep(0,length(cx))     # 初始判別數(全部歸零)
  delta[NB]<-dnb               # 最佳解檢定子
  pr<- which.min(st[,'b1'])  # 樞紐行號(取最小值)
  ratio<-delta/st[pr,1:n] # 技術矩陣比值
  ratio<-replace(        # 技術矩陣比值忽略<=0
    ratio,(is.nan(ratio) | ratio>=0),NA)
  pc<-which.max(ratio)   # 樞紐列號
  print(rbind(           # 列印最終單形表
    rbind(st,delta=append(delta,NA)),ratio=append(ratio,NA)
  ))
  if (all(st[,'b1']>=0) && all(delta>=0)){
    cat('\n已達最佳解\n',    # 列印目標函數值
        '目標函數值：',P,'\n')
    print('已達最佳解')
    break
  }
}
########## 方法三:lpSolveAPI ###############
# 題目，求目標函數最小值(最佳解):
# C = 2x1 + 0.5x2 + 2.5x3 + 0.3x4 + 1.75x5 + 0.35x6 + 2x7
# 5x1+    + 2x3      + 3x5+  x6+ 2x7  ≥ 100
# 3x1+  x2+ 5x3      + 2x5+       x7  ≥ 80
# x1 +      3x3 + x4 + 2x5+      6x7  ≥ 120
# x1 >= 0 
# x2 >= 0
# x3 >= 0
# x4 >= 0 
# x5 >= 0
# x6 >= 0
# x7 >= 0
library(lpSolveAPI) # 載入線性規劃函式庫lpSolveAPI
lprec <- make.lp(  # 建立一新線性規劃model物件，函式說明請參閱R線上說明
  3, 7,)  # 此model具3個限制條件7個結構變數求解
# 程式執行至此可先print(lprec) 初步檢查model內容
xx<-lp.control( # 線性規劃模式，設定其相關控制參數，函式說明請參閱R線上說明
  lprec=lprec, # 對象線性規劃model物件
  sense='min') # 設定此model取最小值
set.column( # 設定model欄限制條件各係數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  column=1, # 此model第1欄
  x=c(5,3,1)) # 此欄各限制條件值(對應上述7個條件)
# set.type(lprec,1:7,type='integer')   # 決策變數解取整數
set.column( # 同上
  lprec,    # 同上
  column=2, # 此model第2欄
  x=c(0,1,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=3, # 此model第3欄
  x=c(2,5,3)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=4, # 此model第4欄
  x=c(0,0,1)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=5, # 此model第5欄
  x=c(3,2,2)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=6, # 此model第6欄
  x=c(1,0,0)) # 同上
set.column( # 同上
  lprec,    # 同上
  column=7, # 此model第7欄
  x=c(2,1,6)) # 同上
set.objfn(  # 設定model的目標函數，函式說明請參閱R線上說明
  lprec,    # 此model物件
  c(2,0.5,2.5,0.3,1.75,0.35,2))    # 目標函數各係數
# 給予各條件名稱
rownames <- c('V1','V2','V3')
colnames <- c('P1','P2','P3',  # 給予各係數行(決策變數)名稱
              'P4','P5','P6','P7') 
dimnames(lprec) <- list( # 將model變數欄及條件欄重新命名，方便閱讀
  rownames,   
  colnames)
set.constr.value( # 設定限制值，函式說明請參閱R線上說明
  lprec,  # 此model物件 
  rhs=c(100,80,120), # 限制值(Right Hand Side)
  constraints=1:3) # 7個限制條件
set.constr.type( # 設定限制型態(方向)，函式說明請參閱R線上說明
  lprec,  # 此model物件
  types=c('>=', '>=', '>='),  # 限制型態(方向)
  constraints=1:3) # 7個限制條件
# 程式執行至此可先print(lprec) 檢查model完整內容
print(lprec)  # 將model變數欄及條件欄已重新命名

solve(lprec)  # 將此model 求解，函式說明請參閱R線上說明
get.objective(lprec) # 讀出目標函數最佳解
get.variables(lprec) # 讀出目標函數最佳解之各值(依欄順序顯示)
get.constraints(lprec) # 讀出各限制條件式右側(rhs)之結果(依列順序)
library(pracma)
cat('最佳解之各值進位取整數(P1~P7): ',ceil(get.variables(lprec)),'\n')
cat('合計(顆): ',
    sum(ceil(get.variables(lprec)*c(2,0.5,2.5,0.3,1.75,0.35,2))),'\n')
###############end of 7_9.R########################