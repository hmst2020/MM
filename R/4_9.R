# x: 底邊長 z:高=100/x^2
C<-'8*x^2+5*4*x*100/x^2'
print(ex<-parse(text=C))  # 解析函數傳回該函數的expression物件

library(Deriv)
print(dx<-Deriv(ex,c('x'))) # 將expression物件對x微分傳回該導數

print(X<-solve(a=3,b=log(2000)-log(16)))  # 解a%*%x=b 的x

print(x<-exp(X))  # 將ln(x) 還原成目標函數的x

print(dxx<-Deriv(dx)) # x的二階導函數
eval(dxx)  # 二階導數值

print(z<-100/x^2)     # 紙箱高度
print(c<-eval(ex))    # 最低成本

fc <- function(x,z) {   # 自訂函式計算目標函數值
  eval(ex)
}

X<-seq(0,x+5,0.1)    # 底部x區間值
plot(                     # x區間的曲線圖
  x=X,                    # x(底部長寬)軸資料
  y=fc(X),                # y軸(成本)經自訂的p函式計算
  type='l',               # 繪製線圖
  main='目標成本曲線',    # 圖標題
  xlab='底部長寬',        # x軸標籤
  ylab='成本',            # y軸標籤
  ylim=c(eval(ex)-400,eval(ex)+400)
)
abline(h=eval(ex),col='red',lty=2)  # 最低目標成本處
abline(v=x,col='red',lty=2)         # 谷底x值
points(x=x,y=c,                     # 極值處
       col='red',   # 紅色
       pch=16,      # 實心圓形
       cex=1.5)     # 預設大小倍數
############### end of 4_9.R ###########