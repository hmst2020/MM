###########檢查LID 向量span R3##############
print(x1<-c(1,1,1))         # x1 向量
print(x2<-c(1,2,3))         # x2 向量
print(x3<-c(0,1,0))         # x3 向量
print(S<-cbind(x1,x2,x3))   # 展成集 S
dim(S)                      # 矩陣維度
det(S)                      # 行列式計算
print(invA<-solve(S))       # 矩陣反函數
##############end of 3_7.R############