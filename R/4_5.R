print(d<-deriv(                         # 對本例的收益函數偏微
  expr=R ~ (100*x+150*y-0.03*x^2-0.02*y^2),  # 以formula物件當引數
  'x'))                                 # 對x偏微分
x<-50       # A10 的銷售量
y<-40       # A20 的銷售量
eval(d)     # 計算x=50對收益之變化率

rvn<-'100*x+150*y-0.03*x^2-0.02*y^2'   # 收益函數的character物件
print(ex<-parse(text=rvn))    # 解析函數傳回收益函數的expression物件
print(d<-deriv(expr=ex,'x'))
x<-50       # A10 的銷售量
y<-40       # A20 的銷售量
eval(d)     # 計算x=50對收益之變化率

library(Deriv)
print(d<-Deriv(    # 對本例的收益函數偏微
  R ~ 100*x+150*y-0.03*x^2-0.02*y^2,
  'x'))            # 對x偏微分
rm(y)       # 移除 y變數
x<-50       # A10 的銷售量
eval(d)     # 計算x=50對收益之變化率
############### end of 4.5.R ############