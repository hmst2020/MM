(year <- c(0,1,2,3,4,5))      # 自2013年起為第0年，依次類推
(expense <- c(2.91,3.23,3.42,3.63,3.85,4.08)) # 各年對應費用
(lsq <-lm(            # 使用lm內建函式建構線性迴歸模型
  formula=expense ~ year))  # 公式依據自變數(每年)、因變數(費用)
(intercept<-lsq$coefficients['(Intercept)']) # 迴歸線之常數項(截距)
(slope <- lsq$coefficients['year'])    # 迴歸線之斜率(係數)

(unname(                   # 去名
  intercept+slope*(year[length(year)]+2)  # 推估2020年的可能費用
)) 
(s<-summary(lsq))           # 迴歸模型彙總
sum(s$residuals^2)          # 殘差(誤差平方和)

plot(              # 使用內建函式plot 繪製本例之散佈圖與迴歸缐
  x=year,          # x軸為年度順序
  y=expense,       # y軸為年度費用
  type='p',        # 指定繪點狀圖
  xlab="年度",     # x軸標籤
  ylab="健康照護花費(兆元) ",              # y軸標籤     
  main="年度與健康照護費用散佈圖與迴歸線") # 圖標題
abline(                    # 將plot繪出的圖疊加直線圖,即最迴歸缐
  coef=c(intercept,slope)) # 直線係數的截距及斜率
text(            # 將plot繪出的圖疊加文字
  x=year[1]+3,   # 文字對應x軸位置 
  y=3.5,         # 文字對應y軸位置 
  paste0('Y =',    # paste將參數轉換為字符串，並將其連接
         round(intercept,digits=5),
         '+',round(slope,digits=7),
         't')) # 文字內容

(year<-c(2013,2014,2015,2016,2017,2018)) # 自2013年起
#################end of 1_4.R####################