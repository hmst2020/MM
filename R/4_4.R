print(ex<-parse(text='5*t^2+100'))  # 建構資產函數物件並印出
fx<-function(t) eval(ex)     # 自訂資產函數之運算函式
curve(expr=fx,          # 繪出fx函式曲線
      from=0,to=5,      # x 座標起迄
      main='資產累積曲線',  # 圖標題
      ylab='資產(A)',   # y軸標籤
      xlab='年(t)')     # x軸標籤

print(d<-D(ex,'t'))     # 對函數物件微分，列印微分物件

sprintf('%d百萬/年',(fx(5)-fx(2))/3)  # 最後三年(t=3,4,5)平均成長率
t<-2
sprintf('%d百萬/年',t2<-eval(d)) # t=2時資產成長率
sprintf('%f%%',t2/fx(2)*100)     # t=2相對於資產之成長百分比
############### end of 4.4.R ############