(A<-diag(c(1,2,3)))              # 待分解矩陣(方陣)
(eig<-eigen(A))                  # 分解特徵值/特徵向量
(D<-diag(eig$values))            # 對角化方陣
all.equal(A,
          eig$vectors%*%D%*%solve(eig$vectors))  # 驗證公式(3.3.2)
###############end of 3_18.R####################