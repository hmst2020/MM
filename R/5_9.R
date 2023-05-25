library(LaplacesDemon)
x<- rbind(                 # 建構本例資料物件
  athlete=setNames(c(0.02,0.98),c('B1','B2')),  # 運動員是否服藥比例
  A1=c(0.95,0.05),         # 檢驗為陽性的真、偽比例
  A2=c(0.05,0.95)          # 檢驗為陰性的偽、真比例
)
print(x)     # 列印本例資料物件

print(PrAB<- BayesTheorem(     # 求檢驗呈陽性時真確、不真確機率
  PrA=x['athlete',],      # 運動員是否服藥比例(事前機率)
  PrBA=x['A1',]))         # 檢驗結果與運動員是否服藥的比例(事後機率)
############### end of 5_9.R ############