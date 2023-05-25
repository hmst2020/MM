############證明線性相依###########
library(pracma)
v1<-c(2,3)
v2<-c(5,8)
v3<-c(1,2)
print(S<-cbind(v1,v2,v3))      # 向量集之矩陣
Rank(S)                        # 矩陣的秩
for (r in 1:(nrow(S))) {         # 依列順序使leading variable=1
  idx <- which(S[r,]!=0)       # 找出樞紐行
  if (length(idx)==0) break    # 若無樞紐行則結束迴圈
  pc<-idx[1]                   # 樞紐行
  pe<-S[r,pc]                  # 樞紐元
  S[r,]<-S[r,]/pe              # 樞紐列基本列運算使leading variable=1
  prow<-S[r,]                  # 樞紐列
  S[-r,]<-S[-r,]+              # 高斯消去法(樞紐列以外基本列運算)
    matrix(-S[-r,pc]/prow[pc])%*%prow
}
print(S)                       # 最簡列階梯式的矩陣

(x<-solve(cbind(v1,v2),v3))    # 求解線性組合係數
print(x[1]*v1+x[2]*v2==v3)     # 驗證v3是v1、v2的線性組合

print(nspace<-cbind(           # 零核空間的任意三個向量
  c(2,-1,1),                   # c3=1 的x向量
  c(3,-1.5,1.5),               # c3=1.5 的x向量
  c(4,-2,2)))                  # c3=2 的x向量
print(S%*%nspace)           # 驗證零核空間(null space)
Rank(nspace)                # 零核空間的秩
##############end of 3_5.R############

