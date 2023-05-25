L<-parse(text='5*sqrt(x*y)+lambda*(1200*x+y-60000)') # 拉氏函數
library(Deriv)
print(Lxy<-Deriv(L))   # 拉氏函數的梯度向量，見本章(5.11.3)式
sn<-function(X){  # 自訂一函式計算x、y、lambda 變數對各方程式的結果
  x<-X[1]
  y<-X[2]
  lambda<-X[3]
  xv<-1200 * lambda + 2.5 * (y/sqrt(x * y))
  yv<-2.5 * (x/sqrt(x * y)) + lambda
  lambdav<-1200 * x + y - 60000
  return(c(xv,yv,lambdav))
}
library(rootSolve)
print(ss<-multiroot(    # 解非線性方程組的x、y、lambda變數解
  f=sn,start=setNames(c(1,1,1),c('x','y','lambda'))))

x<-ss$root['x'];y<-ss$root['y'];lambda<-ss$root['lambda']  # 變數解
print(Lxx<-Deriv(Deriv(L,'x'),'x'))
print(Lyy<-Deriv(Deriv(L,'y'),'y'))
print(Lxy<-Deriv(Deriv(L,'x'),'y'))
unname(eval(Lxx)*eval(Lyy)-eval(Lxy)^2)   # 計算(5.10.1)之D值

f<-function(x,y) eval(parse(text='5*sqrt(x*y)'))# 目標函式(年產能函數)
f(x=20,y=60000-1200*20)       # 代入x=20及y=60000-1200*x
unname(eval(parse(text='5*sqrt(x*y)')))  # 極大值
############### end of 4_13.R ###########