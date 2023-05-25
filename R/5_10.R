library(LaplacesDemon)
x<- data.frame(         # 建構本例資料物件
  d1=c(3750,3000),      # d1病患數及其中有症狀者數
  d2=c(2250,2050),      # d2病患數及其中有症狀者數
  d3=c(4000,3500))      # d3病患數及其中有症狀者數
rownames(x)<-c('patients',  # 第一列為病患數
               'symptons')  # 第二列為其有症狀者數
print(x)     # 列印本例資料物件

print(xdist<- rbind(       # 計算並印出各疾病機率及其有症狀機率
  x['patients',]/sum(x['patients',]), # 患d1~d3疾病機率,
  x['symptons',]/x['patients',]))      # 患d1~d3疾病者有症狀之機率   

print(PrDA<- BayesTheorem(     # 求有症狀時患各疾病(d1~3)之機率
  PrA=xdist['patients',],      # 各疾病機率
  PrBA=xdist['symptons',]))    # 各疾病下有症狀之機率
############### end of 5_10.R ############