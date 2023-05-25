u<-c(-1,2,0,1,3)              # u 向量
v<-c(2,1,3,1,1)               # v 向量
(uv<-u%*%v)                     # u,v 兩個向量的內積
(lu<-norm(u,'2'))               # u 向量的長(norm)
(lv<-norm(v,'2'))               # v 向量的長(norm)
(abs(uv)<=lu*lv)                # 驗證柯西不等式
(COS<-(uv)/(lu*lv))             # u、v兩向量夾角餘弦
paste0(arcCOS<- acos(COS)/pi,'π')   # 夾角弧度或弳度(radian)

.Machine$double.eps         # identical 與 == 精度要求
(uv==lu*COS*lv)             # 二向量內積與夾角的關係
identical(uv,lu*COS*lv)     # 二向量內積與夾角的關係
(.Machine$double.eps^0.5)   # all.equal 精度要求
all.equal(uv,lu*COS*lv)     # 二向量內積與夾角的關係
##############end of 3_9.R############
