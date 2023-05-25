avg<- 12/20            # 平均數
x<-setNames(0:4,0:4)   # 樣本空間(發生人數)
fx<- function(x){          # 自訂卜瓦松分配機率密度函式
  (exp(-avg)*avg^x)/factorial(x)} 
print(distr<-sapply(x,fx))          # 機率密度分布計算與列印

print(distr<-dpois(   # 使用內建卜瓦松密度函式
  x=x,                # 各分位數
  lambda=avg          # 平均發生速率
))

print(Ex<-weighted.mean(x,distr))   # 計算期望值並列印
plot(       # 列印機率密度函式圖形
  x=x,      # x軸(樣本空間)
  y=distr,  # y軸(成功次數之機率)
  type='b', # 點與線
  main='機率密度分布',  # 圖標題
  xlab='成功次數',      # x軸標籤
  ylab='機率')          # y軸標籤
abline(v=Ex,col='red',lty=2) # 於期望值處標示垂直虛線
text(Ex,0,"(期望值)")        # 於圖內摽示文字
mtext(round(Ex,1),side=1,line=0.2,at=Ex)  # 於圖外邊界摽示文字

print(200*distr)     # 實際部隊總數下預測發生人數

library(purrr)
plot(       # 繪出機率累積分布函數(CDF)圖形
  x=x,      # x軸(樣本空間)
  y=accumulate(distr,`+`),  # y軸(發生(死亡)人數之累積機率)
  type='b', # 點與線
  main='機率累積分布',    # 圖標題
  xlab='發生(死亡)人數',  # x軸標籤
  ylab='機率')            # y軸標籤

sum(distr*(x-Ex)^2)     # 變異數
weighted.mean((x-Ex)^2,distr)   # 變異數

n<-400                 # 設經過400年
print(distr<-sapply(   # 機率密度分布計算與列印
  X=x,                 # 樣本空間
  FUN=dbinom,          # 使用內建的函式
  size=n,              # dbinom的參數size及其引數
  prob=0.6/n))         # dbinom的參數prob及其引數
############### end of 5_13.R ############