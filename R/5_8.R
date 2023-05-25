library(prob)
library(data.table)
print(
  data.table(x<-data.frame(         # 創建原始資料並列印
  studentID=1:30,                   # 學生編號1 ~ 30
  gender=c(rep('male',20),rep('female',10)), # 男女生
  addict=c(rep('heavy',16),rep('light',4),   # 男生各依賴程度
           rep('heavy',4),rep('light',6)))   # 女生各依賴程度
  ),5)   # 列出前後5筆

print(ps <- probspace(x))     # 依原始資料展開機率空間並列印

Prob(                  # 從機率空間依事件與前題條件過濾與計算機率
  ps,                  # 機率空間
  event=addict == 'heavy',    # 事件(重度)
  given=gender == 'male')    # 條件(男生)

Prob(                  # 從機率空間依事件與前題條件過濾與計算機率
  ps,                  # 機率空間
  event=addict == 'heavy',    # 事件(重度)
  given=gender %in% c('male','female'))   # 條件(男生及女生)
############### end of 5_8.R ############