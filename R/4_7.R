f<-'x^3+3*x^2-9*x-9'      # 本例目標函數的character物件
print(ex<-parse(text=f))  # 解析函數傳回該函數的expression物件

print(d<-D(ex,'x'))    # 將expression物件對x微分傳回該一階導函數

print(rts<-polyroot(c(-9,6,3)))    # 給予一元多項式的係數解其根

print(d2<-D(d,'x'))    # 將一階導函數再微分得二階導函數
sapply(Re(rts),function(x) {  # 將將一階導函數解得的根帶入二階導函數
  eval(d2)
})

y<- function(x){          # 目標函數自訂函式
  return (eval(ex))
}
x<-seq(min(Re(rts))-2,max(Re(rts))+2,0.1)    # x區間值
plot(                     # x區間的曲線圖
  x=x,                    # x軸資料(向量)
  y=y(x),                 # y軸經自訂的y函式計算
  type='l'                # 繪製線圖
)
sapply(Re(rts),function(x) {   # 將解得的根與目標函數值標示於曲線
  abline(v=x,col='red',lty=2) # 將將一階導函數解得的根繪垂直線
  points(x=x,y=y(x),
         col='red',   # 紅色
         pch=16,      # 實心圓形
         cex=1.5)     # 預設大小倍數
  text(x,y(x),
       paste0('(',x,',',y(x),')'),adj=c(-0.5,0.3))
  y(x)   # 傳回值
})
############### end of 4.7.R ############