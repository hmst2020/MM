print(ex<-parse(text='2400*x-2*x^2'))  # 建構函數表達式物件並印出
fx<-function(x) eval(ex)     # 定義面積曲線函式
curve(expr=fx,          # 繪出fx函式曲線
      from=0,to=1000,   # x 座標起迄
      main='面積曲線',  # 圖標題
      ylab='面積(A)',   # y軸標籤
      xlab='x邊長')     # x軸標籤

print(d<-D(ex,'x'))        # 對函數物件微分，列印微分物件

print(dd<-D(d,'x'))        # 對微分函數物件再微分並列印該物件
eval(dd)                   # 運算dd值

print(x<-solve(4,2400))    # 解最大值時x值，並列印出
print(A<-eval(ex))         # 計算x值使面積最大值(面積函數)，並列印
A/x                        # A=xy 求y值
m<-eval(d)                 # 最大值時x值的切線斜率
abline(v=x,col='red',lty=3)  # 最大值x所在畫一垂直點狀線
abline(a=fx(x)-m*x,b=m,col='red',lty=3)  # x所在的點狀切線

############### end of 4.3.R ############