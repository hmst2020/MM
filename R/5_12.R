x<-0:10       # 樣本空間
n<-10         # 實驗次數
p<-0.4        # 每次成功機率
fx<- function(x){             # 自訂二項分配機率密度函式
  choose(n,x)*p^x*(1-p)^(n-x)} 
print(distr<-sapply(x,fx))    # 機率密度分布計算與列印

print(distr<-sapply(   # 機率密度分布計算與列印
  X=x,                 # 樣本空間
  FUN=dbinom,          # 使用內建的函式
  size=n,              # dbinom的參數size及其引數
  prob=p))             # dbinom的參數prob及其引數

print(Ex<-sum(distr*x))  # 計算期望值並列印
print(Ex<-weighted.mean(x,distr))   # 計算期望值並列印
plot(       # 列印機率密度函式圖形
  x=x,      # x軸(樣本空間)
  y=distr,  # y軸(成功次數之機率)
  type='b', # 點與線
  main='機率密度分布',  # 圖標題
  xlab='成功次數',      # x軸標籤
  ylab='機率')          # y軸標籤
abline(v=Ex,col='red',lty=2) # 於期望值處標示垂直虛線


library(purrr)   # 使用此套件
plot(       # 繪出機率累積分布函數(CDF)圖形
  x=x,      # x軸(樣本空間)
  y=accumulate(distr,`+`),  # y軸(成功次數之累積機率)
  type='b', # 點與線
  main='機率累積分布',  # 圖標題
  xlab='成功次數',      # x軸標籤
  ylab='機率')          # y軸標籤

sum(distr*(x-Ex)^2)             # 變異數
weighted.mean((x-Ex)^2,distr)   # 變異數
############### end of 5_12.R ############