################# 非標準基底向量相對標準基底的座標##############
B<-cbind(c(1,0),c(1,2))       # 非標準基底
xB<-c(3,2)                    # 相對於非標準基底的座標向量
(xs<-B%*%xB)                  # 相對於標準基底的座標向量

solve(B,xs)          # 將標準基底換回非標準基底的座標向量

###################end of 3_6.R#####################################