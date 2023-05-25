L<-parse(text='t^3*n+lambda*(400000-40000*t-400*n)') # 目標函數
library(Deriv)
print(Ltnl<-Deriv(L))   # 將目標函數對x、y分別求一階導函數

# 3 * (n * t^2) - 40000 * lambda=0
# t^3 - 400 * lambda=0
# 400 * n + 40000 * t-400000=0
sn<-function(x){  # 自訂一函式計算t、n、lambda 變數對各方程式的結果
  t<-x[1]
  n<-x[2]
  lambda<-x[3]
  one<-3 * (n * t^2)-40000*lambda
  two<-t^3-400*lambda
  three<-40000*t+400*n-4e+5
  return(c(one,two,three))
}
library(rootSolve)
print(ss<-multiroot(   # 解非線性方程組的t、n、lambda變數解
  f=sn,       # 自訂之函式計算各方程式的結果
  start=setNames(c(10,10,10),c('t','n','lambda')))) # 遞迴運算之起始值

t<-ss$root['t'];n<-ss$root['n'];lambda<-ss$root['lambda'] # 變數解
print(Ltt<-Deriv(Deriv(L,'t'),'t'))
print(Lnn<-Deriv(Deriv(L,'n'),'n'))
print(Ltn<-Deriv(Deriv(L,'t'),'n'))
unname(eval(Ltt)*eval(Lnn)-eval(Ltn)^2)    # 計算(5.10.1)之D值

f<-function(t,n) t^3*n    # 目標函式(銷售額函數)
f(t=1,n=1000-100*1)       # 代入t=1及n=1000-100t

unname(eval(parse(text='t^3*n')))  # 銷售額最大值
############### end of 4_12.R ###########