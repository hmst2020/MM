fx<-function(x) 250*x - 5*x^2     # 定義獲利曲線函式
curve(expr=fx,         # 繪出fx函式曲線
      from=0,to=100,   # x 座標起迄
      main='獲利曲線', # 圖標題
      ylab='獲利(元)', # y軸標籤
      xlab='產量(千公斤)')   # x軸標籤

print(ex<-expression(250*x - 5*x^2))  # 建構函數表達式物件並印出
print(d<-D(ex,'x'))     # 對函數物件微分，傳回call物件 

x<-20                      # 給予x值
print(m<-eval(d))          # 將x值帶入微分函數計算得切線斜率
abline(a=fx(x)-m*x,b=m,col='blue')  # 繪出x所在切線
x<-35                      # 給予另一個x值
print(m<-eval(d))          # 將x值帶入微分函數計算得切線斜率
abline(a=fx(x)-m*x,b=m,col='red')  # 繪出x所在切線

curve(expr=fx,         # 繪出fx函式曲線
      from=0,to=100,   # x 座標起迄
      main='獲利曲線', # 圖標題
      ylab='獲利(元)', # y軸標籤
      xlab='產量(千公斤)')   # x軸標籤
library(Ryacas)
print(cmd<-yac_str('D(x) 250*x - 5*x^2'))   # 定義微分函數式並列印
print(d<-yac_expr(cmd))    # 將指令傳回微分結果並印出該表達式物件
x<-20                      # 給予x值
print(m<-eval(d))          # 將x值帶入微分函數計算得切線斜率
abline(a=fx(x)-m*x,b=m,col='blue')  # 繪出x所在切線
x<-35                      # 給予另一個x值
print(m<-eval(d))          # 將x值帶入微分函數計算得切線斜率
abline(a=fx(x)-m*x,b=m,col='red')  # 繪出x所在切線

############### end of 4.1.R ############