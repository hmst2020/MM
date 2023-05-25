P<-'-q^3+39*q^2-75*q-125' # 本例目標利潤函數的character物件
print(ex<-parse(text=P))  # 解析函數傳回該函數的expression物件
print(d<-D(ex,'q'))    # 將exoression物件對q微分傳回該導函數

print(rts<-polyroot(c(-75,78,-3)))    # 給予一元多項式的係數解其根

print(d2<-D(d,'q'))    # 將一階導函數再微分得二階導函數
sapply(Re(rts),function(x) {  # 將將一階導函數解得的根帶入二階導函數
  q<-x
  eval(d2)})

p<- function(x){          # 目標函數之自訂函式
  q<-x                    # ex 的變數值
  return (eval(ex))
}
q<-seq(0,max(Re(rts))+5,0.1)    # 生產數量區間值
plot(                     # 數量區間的曲線圖
  x=q,                    # x(生產數量)軸資料(向量)
  y=p(q),                 # y軸經自訂的p函式計算
  type='l',               # 繪製線圖
  main='目標利潤曲線',    # 圖標題
  xlab='生產數量',        # x軸標籤
  ylab='利潤'             # y軸標籤
)
sapply(X=Re(rts),function(x) {   # 將解得的根與目標函數值標示於曲線
  abline(v=x,col='red',lty=2) # 將將一階導函數解得的根繪垂直線
  points(x=x,y=p(x),
         col='red',   # 紅色
         pch=16,      # 實心圓形
         cex=1.5)     # 圓點大小倍數
  text(x,p(x),
       paste0('(',x,',',p(x),')'),adj=c(-0.5,0.3))
  p(x)   # 傳回值
})
############### end of 4.8.R ############