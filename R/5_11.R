library(LaplacesDemon)
x<- rbind(                 # 建構本例資料物件
  spam=setNames(c(70,30),c('M','N')),  # 垃圾郵件百分比(%)
  W=c(40,10),         # 含特定文字比例(%)
  Wo=c(30,20))        # 含特定文字比例(%)
print(x<-rbind(       # 本例資料轉成比例並列印
  x['spam',,drop=FALSE]/sum(x[1,]),  # P(B) drop=FALSE表示保留行名
  t(t(x[c(2,3),])/x[1,])))           # P(A|B)

print(PrAB<- BayesTheorem(     # 求有特定文字下為垃圾郵件之機率
  PrA=x['spam',],      # 垃圾與非垃圾郵件之機率
  PrBA=x['W',]))       # 垃圾分類下特定文字出現的機率
############### end of 5_11.R ############