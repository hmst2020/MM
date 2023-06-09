library(pracma)
x<-c(2,4,6,8)                            # 經過小時
y<-c(2.1,1.6,1.4,1.0)                    # 藥物濃度ppm
(A<-cbind(x0=rep(1,4),x1=x))             # 線性方程組的A矩陣
(c<-as.vector(solve(t(A)%*%A,t(A)%*%y))) # 解最近似解(迴歸線各係數)
(x5<-c[1]+c[2]*5)                        # 估計第5小時的藥物濃度
sum((y-A%*%c)^2)                         # 殘差平方和

z <- c[1]+c[2]*x                    # 迴歸線對應經過時間的藥物濃度
plot(x, z, type='l')                # 繪出迴歸線
points(c(x,5),c(y,x5))# 繪出迴歸線第5小時的藥物濃度以及其他時間的濃度

###### 完全擬和######
(A<-cbind(x0=rep(1,4),x1=x,x2=x^2,x3=x^3))  # 線性方程組的A矩陣
(c<-as.vector(solve(t(A)%*%A,t(A)%*%y))) # 解最近似解(迴歸線各係數)
sum((y-A%*%c)^2)                         # 殘差平方和
z <- c[1]+c[2]*x+c[3]*x^2+c[4]*x^3    # 迴歸線對應經過時間的藥物濃度
plot(x, z, type='l')                  # 繪出迴歸線
(x5<-c[1]+c[2]*5+c[3]*5^2+c[4]*5^3)   # 估計第5小時的藥物濃度
points(c(x,5),c(y,x5))# 繪出迴歸線第5小時的藥物濃度以及其他時間的濃度
################end of 3_12.R#####################
