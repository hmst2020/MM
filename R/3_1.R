######### 矩陣向量 ##########
print(Z<-rbind(c(-1,4),c(2,-2)))    # 線性組合矩陣
print(u<-rbind(c(1,1),c(1,-1)))     # u矩陣
print(v<-rbind(c(4,0),c(1,1)))      # v矩陣
print(w<-rbind(c(1,2),c(1,1)))      # w矩陣
print(Z==(2)*u+(-1)*v+1*w)          # 驗證線性組合

####### ordered n-tuple #######
print(Z<-c(2,8))                # 線性組合向量
print(u<-c(1,1))                # u向量
print(v<-c(1,3))                # v向量
print(w<-c(1,-1))               # w向量
print(Z==(-3)*u+(4)*v+1*w)      # 驗證線性組合一
print(Z==(1)*u+(2)*v+(-1)*w)    # 驗證線性組合二
##############end of 3_1.R############
