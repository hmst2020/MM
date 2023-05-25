(A<-rbind(c(2,-2,3),c(1,1,1),c(1,3,-1)))         # 待分解矩陣(方陣)
(eig<-eigen(A))                   # 分解特徵值/特徵向量

(D<-diag(eig$values))             # 對角化方陣
(P<-eig$vectors)                  # 特徵向量構成轉移矩陣
P%*%D%*%solve(P)                  # 驗證公式(3.3.2)

(u<-eig$vectors[,1]/c(1,1,1))     # 手算與R軟體結果比值u
(s<-eig$vectors[,2]/c(11,1,-14))  # 手算與R軟體結果比值s
(t<-eig$vectors[,3]/c(-1,1,1))    # 手算與R軟體結果比值t

c(1,1,1)/sqrt(sum(c(1,1,1)^2))       # 特徵值3的單範特徵向量
c(11,1,-14)/sqrt(sum(c(11,1,-14)^2)) # 特徵值-2的單範特徵向量
c(-1,1,1)/sqrt(sum(c(-1,1,1)^2))     # 特徵值1的單範特徵向量

prod(eig$values)                 # 特徵值的積
det(A)                           # A 矩陣的行列式

sum(eig$values)                  # 特徵值的和
sum(diag(A))                     # A 矩陣主對角線各值的和

###############end of R_19.R####################