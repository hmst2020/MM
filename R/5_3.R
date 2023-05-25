print(mean(x))     # 列印平均值

print(x_table<-table(x))   # 依觀察值建構列聯表
xcont<-c(x_table)          # 列連表
print(as.numeric(names(xcont[which.max(xcont)])))  # 眾數
############### end of 5_3.R ############