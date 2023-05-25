x<- c(9,6,12,10,13,        # 銷售業績
      15,16,14,14,16,
      17,16,24,21,22,
      18,19,18,20,17)
sum_x<-summary(x)  # 五數彙總及平均數
print(sum_x)   # 列印順序統計彙總

boxplot(x,horizontal=TRUE)  # 盒鬚圖(Box Whisker Plot)
unname(                     # IQR對全距的佔比
  (sum_x['3rd Qu.']-sum_x['1st Qu.'])/
    (sum_x['Max.']-sum_x['Min.']))

ecdf(x)(x)     #  每個數據點對應於百分位數統計數

plot(x=ecdf(x))     #  繪出每個數據點的百分位數
abline(h=0.25,col='red',lty=2)  # 累積分布0.25
abline(h=0.75,col='red',lty=2)  # 累積分布0.75
abline(h=0.9,col='red',lty=2)   # 累積分布0.9
abline(v=sum_x['1st Qu.'],   # 百分位25%(第1四分位數)
       col='blue',lty=2)
abline(v=sum_x['3rd Qu.'],   # 百分位75%(第3四分位數)
       col='blue',lty=2)

print(q_4<-quantile(x=x,probs=seq(0,1,0.25),type=6))  # 四分位數
print(q_5<-quantile(x=x,probs=seq(0,1,0.2),type=6))   # 五分位數
print(q_10<-quantile(x=x,probs=seq(0,1,0.1),type=6))  # 十分位數

n<- length(x)  # 資料元素個數
x<-sort(x)     # 將各資料由小到大排序
f_6<-function(prob){       # type 6 百分位數計算
  index<-min((n+1)*prob,n)  # 百分位置不超過總觀測個數
  if(index%%1!=0){      # 判斷為非整數
    j<-floor(index)
  }else{
    j<-index
  }
  j<-max(1,j)           # 百分位置不小於1
  wmean<-if (is.na(prob*(x[j+1]-x[j]))) 0 else prob*(x[j+1]-x[j])
  return (x[j]+wmean)     # 回傳加權平均值
}
f_7<-function(prob){    # type 6 百分位數計算
  index<-min((n+1)*prob,n)
  if(index%%1!=0){      # 判斷為非整數
    j<-floor(index)
  }else{
    j<-index
  }
  j<-max(1,j)
  y<-1-prob
  return (if(j<=1 | j>=n) x[j] else (1-y)*x[j]+y*x[j+1])
}

seq_4<-setNames(seq(0,1,0.25),names(q_4))   # 四分位
seq_5<-setNames(seq(0,1,0.2),names(q_5))   # 五分位
seq_10<-setNames(seq(0,1,0.1),names(q_10))   # 十分位
sapply(seq_4,f_6)     # type 6之四分位數
sapply(seq_5,f_6)     # type 6之五分位數
sapply(seq_10,f_6)    # type 6之十分位數

sapply(seq_4,f_7)     # type 7之四分位數
sapply(seq_5,f_7)     # type 7之五分位數
sapply(seq_10,f_7)    # type 7之十分位數
############### end of 5_1.R ############


