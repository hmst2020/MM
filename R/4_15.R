C<-'c*d/q + h*q/2' # 目標函數
library(Ryacas)
library(Deriv)
print(Cq<-yac_str(            # 求目標函數C的q偏導函數(字串)
  paste('Deriv(q) ',C))) 

c<-90    # 每次訂貨成本
d<-10000 # 一年需求量
h<-200*10/100 # 每單位庫存持有成本
library(stringr)
print(Cq<-str_replace_all(Cq,  # 置換偏導函數字串之已知值
                c('c'=as.character(c),
                  'd'=as.character(d),
                  'h'=as.character(h))))

print(eoq<-q<-sqrt(    # 經濟批量(解線性函數=0的q)
  1/solve((90*10000),20/2)))

print(TC<-eval(parse(text=C)))  # 存貨總成本
print(n<-ceiling(d/eoq))  # 一年進貨次數
print(t<-floor(365/n))    # 到貨週期天數
############### end of 4_15.R ###########