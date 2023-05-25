L<-parse(text='4*x+8*y+6*z+lambda*(x^2+4*y^2+2*z^2-800)')# 拉氏函數
library(Deriv)
print(Lxy<-Deriv(L))   # 拉氏函數的梯度向量，見本章(5.11.3)式

sn<-function(X,C){# 自訂一函式計算x、y、z、lambda 變數對各方程式的結果
  x<-X[1]  # 傳入x值
  y<-X[2]  # 傳入y值
  z<-X[3]  # 傳入z值
  lambda<-X[4]   # 傳入lambda值
  v1<-2 * (lambda * x) + 4    # 拉氏函數的x方向導數值
  v2<-8 + 8 * (lambda * y)    # 拉氏函數的y方向導數值
  v3<-4 * (lambda * z) + 6    # 拉氏函數的z方向導數值
  v4<-2 * z^2 + 4 * y^2 + x^2 - C # 拉氏函數的lambda方向導數值
  return(c(v1,v2,v3,v4))  # 傳回各值
}
library(rootSolve)
print(ss<-multiroot(    # 解非線性方程組的x、y、z、lambda變數解
  f=sn,C=800,start=setNames(c(1,1,1,1),c('x','y','z','lambda'))))

x<-ss$root['x'] # 變數解
y<-ss$root['y']
z<-ss$root['z']
lambda<-ss$root['lambda']
unname(eval(parse(text='x^2+4*y^2+2*z^2')))  # 限制式
unname(eval(parse(text='4*x+8*y+6*z')))  # 利潤極大值

print(ss<-multiroot(    # 解非線性方程組的x、y、z、lambda變數解
  f=sn,C=801,start=setNames(c(1,1,1,1),c('x','y','z','lambda'))))
x<-ss$root['x'];y<-ss$root['y'];z<-ss$root['z'];lambda<-ss$root['lambda']
unname(eval(parse(text='x^2+4*y^2+2*z^2')))  # 限制式
unname(eval(parse(text='4*x+8*y+6*z')))  # 利潤極大值


print(ss<-multiroot(    # 解非線性方程組的x、y、z、lambda變數解
  f=sn,C=800.5,start=setNames(c(1,1,1,1),c('x','y','z','lambda'))))
x<-ss$root['x'];y<-ss$root['y'];z<-ss$root['z'];lambda<-ss$root['lambda']
unname(eval(parse(text='x^2+4*y^2+2*z^2')))  # 限制式
unname(eval(parse(text='4*x+8*y+6*z')))  # 利潤極大值
############### end of 4_14.R ###########
