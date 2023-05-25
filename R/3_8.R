u<-c(2,-3,4)                  # u 向量
v<-c(1,-2,-5)                 # v 向量
print(uv<-as.numeric(u%*%v))  # u,v 兩個向量的內積
print(nu<-sqrt(sum(u^2)))     # u 向量的長(norm)
print(nv<-sqrt(sum(v^2)))     # v 向量的長(norm)
print(abs(uv)<=nu*nv)  # 驗證柯西不等式: 其內積的絕對值≤向量長度的積

print(COS<-uv/(nu*nv))        # u、v兩向量夾角餘弦值
print(uv==nu*COS*nv)          # 二向量內積與夾角的關係
print(acos(COS)/pi*180)       # 以度表示的夾角

print(as.vector(uv)/sum(v^2)*v) # u在v向量上投影的向量(分量)
##############end of 3_8.R############