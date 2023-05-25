# 成本函數的character物件
C<-'0.12*x^2+0.04*y^2+0.04*x*y+320*x+80*y+30'
print(ex<-parse(text=C))   # 解析函數傳回成本函數的expression物件
print(d<-deriv(ex,'x'))    # 將expression物件對x偏微分傳回該導函數
x<-500      # 小客車產量
y<-1000     # 卡車產量
eval(d)     # 計算x=500對成本之變化率以及其成本函數值

print(d<-deriv(ex,'y'))    # 將exoression物件對y偏微分傳回該導函數
eval(d)     # 計算y=1000對成本之變化率以及其成本函數值
############### end of 4.6.R ############