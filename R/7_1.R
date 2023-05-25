#######單形法基本過程#############
cx<-c(1,1.2,0,0)     # 目標函數之係數(決策變數+閒置變數)
a1<-rbind(           # <= 限制式之各係數(含閒置變數)
  c(2,1,1,0),
  c(1,3,0,1)
)
b1<-c(180,300)     # <= 的限制條件值
st<-rbind(a1,cx)   # 單形表(simplex tableau)
m<-2  # 單體表列數(限制式個數)
n<-4  # 單體表行數(變數個數)
tm<-st[c(1,2),c(1,2)]     # 技術矩陣(technology matrix)
colnames(tm)<-c('x','y')  # 以變數名稱為矩陣行名
print(cbind(tm,b1))       # 列印技術矩陣
colnames(a1)<-c('x','y','u','v')  # 以變數名稱為矩陣行名
print(cbind(a1,b1))       # 列印初始單形表

(cmb<-combn(n,m))         # 基底向量的可能組合

optm<-list(X=c(),P=0)     # 宣告最佳解初始物件
for (i in 1:ncol(cmb)){
  B<-st[1:m,cmb[,i]]     # 線性系統(向量空間)基底
  if(det(B) !=0){        # 判斷是否構成向量空間基底的條件
    fbs<-solve(B)%*%b1   # 求基本解
    fbs<-replace(rep(0,n),cmb[,i],fbs[,1])  # 基本解(basic solution)
    cat(paste0('基本解',i,':'),fbs,'\n') 
    if (all(fbs>=0)){    #  判斷基本可行解(feasible basic solution)
      P<-cx%*%fbs        #  基本可行解目標值
      if (ifelse(is.na(optm$P),TRUE,optm$P<P)){ #  判斷目標值是否增加
        optm$P<-P[1,1]   #  記錄目前目標值
        optm$X<-fbs      #  記錄目前基本可行解各變數值
        names(optm$X)<- colnames(a1) #  各變數名稱
      }
    }
  }
}
print(optm)    # 列印線性規劃結果
#############lpSolve 線性規劃套件###############
# 題目，求目標函數最大值(最佳解):
# P = x + 1.2y
# 2x + y <= 180
# x + 3y <= 300
# x >= 0 
# y >= 0
library(lpSolve)  # 載入線性規劃函式庫lpSolve
f.obj <- c(1,1.2) # 定義目標函數之各係數
f.con <- matrix(  # 建立限制條件之矩陣
  c(2, 1, # 第一限制式之係數
    1,3,  # 第二限制式之係數
    1,0,  # 第三限制式之係數
    0,1),   # 第四限制式之係數
  nrow = 4,  # 矩陣列數
  byrow=TRUE) # 每列填滿再換列
f.dir <- c("<=", "<=",">=",">=") # 限制條件方向
f.rhs <- c(180,300,0,0) # 限制條件計算式之右側數字
result <- lp(  # 使用線性規劃函式 lp求解
  direction ="max",     # 目標函數取最大值之解
  objective.in =f.obj,  # 給予上述目標函數之各係數
  const.mat =f.con,     # 給予上述限制條件之矩陣
  const.dir =f.dir,     # 給予上述限制條件方向之向量物件
  const.rhs =f.rhs)     # 給予上述限制條件計算式之右側數字之向量物件
print(result)           # 將結果印出
print(result$solution)  # 印出目標函數之各變數(即求解的x與y))
print(result$objval)    # 印出目標值(本例取最大值)
#############  end of 7_1.R ########################

