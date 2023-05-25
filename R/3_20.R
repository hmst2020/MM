(A<-rbind(c(1,-2,0,0),c(-2,1,0,0),        # 線性轉換矩陣
          c(0,0,1,-2),c(0,0,-2,1)))
(eig<-eigen(A))                           # 分解特徵值/特徵向量

(P1<-cbind(eig$vectors[,1],eig$vectors[,2])) # 特徵值=3 的特徵向量 
(P2<-cbind(eig$vectors[,3],eig$vectors[,4])) # 特徵值=-1的特徵向量
pracma::Rank(P1)                             # 判斷P1線性獨立
pracma::Rank(P2)                             # 判斷P2線性獨立
(P<-cbind(P1,P2))                            # 轉移矩陣P

(D<-diag(eig$values))             # 對角化方陣
(P<-eig$vectors)                  # 特徵向量構成轉移矩陣
P%*%D%*%solve(P)                  # 驗證公式(3.3.2)
#####################end of 3_20.R#############