print(a<-cbind(c(2,3),c(3,1)))  # 線性組合元素
print(b<-c(4,-1))               # 線性組合向量
print(x<-solve(a,b))         # 可解的結果
print(x[1]*c(2,3)+x[2]*c(3,1))  # 驗證結果

##############end of 3_2.R############
