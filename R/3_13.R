T<-function(v1,v2){           # 自訂一函式用以計算線性轉換T
  return (c(v1-v2,v1+2*v2))
}
iT<-function(w1,w2){          # 自訂一函式用以計算線性轉換T之原像
  (A<-rbind(c(1,-1),c(1,2)))  # 轉換矩陣A
  return (as.vector(solve(A)%*%c(w1,w2))) # 解原像v
  #return (solve(A,c(w1,w2)))  # 解原像v
}
T(-1,2)                       # 原像(-1,2)向量經轉換後的像
T(0,0)                        # 原像(0,0)向量經轉換後的像
iT(-1,11)                     # 已知像w求原像v
################end of 3_13.R#####################