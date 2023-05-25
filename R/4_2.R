library(Ryacas)
cx<-'0.01*x^2+ 80*x + 100'     # 成本函數的字串形式
print(cmd<-yac_str(            # 定義微分函數並列印結果字串
  paste('D(x)',cx))) 

a<-c(0.02)      # 係數
b<-c(110-80)    # 常數項
print(output<-solve(a,b))      # 解ax=b 線性方程式，解得產量

print(d<-parse(text=cmd))         # 產生expression物件
x<-output                           # 令x值
eval(d)                           # 將x值帶入微分函數計算及傳回結果

fx<-function(x) eval(d)           # 自訂邊際成本函式
curve(expr=fx,          # 繪出fx函式曲線
      from=0,to=2500,   # x 座標起迄
      main='邊際成本曲線',  # 圖標題
      ylab='成本(元)',       # y軸標籤
      xlab='桌子產量(張)')   # x軸標籤
abline(v=output,col='red',lty=2)  # 產出量處畫一垂直虛線
abline(h=110,col='red',lty=2)     # 邊際成本處畫一水平虛線
############### end of 4.2.R ############
