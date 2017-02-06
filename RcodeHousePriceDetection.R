# housing prices prediction kaggle contest

datahouse <- read.csv("train.csv")
#Utilities: BldgType: HouseStyle: OverallQual: YearBuilt: MiscFeature: 
#head(datahouse(datahouse$Id, datahouse$BldgType, datahouse$HouseStyle, datahouse$OverallQual, datahouse$YearBuilt, datahouse$MiscFeature))
#head(datahouse(1))
#?head
#head(datahouse)[,c("Id","BldfType")]
feature_Data <- subset(datahouse,select = c(Id,BldgType,HouseStyle,OverallQual,YearBuilt,SalePrice))
#head(feature_Data)
model <- lm(SalePrice ~ BldgType+HouseStyle+OverallQual+YearBuilt, data = feature_Data)
#datahouse$SalePrice
#model

# a <- coef(model)[1]
# XBldgType2fmCon <- coef(model)[2]
# XBldgTypeDuplex <- coef(model)[3]
# XBldgTypeTwnhs <- coef(model)[4]
# XBldgTypeTwnhsE <- coef(model)[5]
# XHouseStyle1.5Unf <- coef(model)[6]
# XHouseStyle1Story <- coef(model)[7]
# XHouseStyle2.5Fin <- coef(model)[8]
# XHouseStyle2.5Unf <- coef(model)[9]
# XHouseStyle2Story <- coef(model)[10]
# XHouseStyleSFoyer <- coef(model)[11]
# XHouseStyleSLvl <- coef(model)[12]
# XOverallQual <- coef(model)[13]
# XYearBuilt <- coef(model)[14]


FinalOutput <- data.frame(ID= numeric(0), SalespRice = integer(0),stringsAsFactors = T)

oup <- apply(feature_Data[,c('Id','BldgType','HouseStyle','OverallQual','YearBuilt')], 1, 
function(x) train_data_salesPrice(coef(model),x['Id'],x['BldgType'],x['HouseStyle'],as.numeric(x['OverallQual']), as.numeric(x['YearBuilt'])))

finalOP <- as.data.frame(t(oup))
names(finalOP)[1] = "ID"
names(finalOP)[2] = "SalesPrice"
write.csv(finalOP,file = "Itrainedthis.csv")

train_data_salesPrice <- function(model,Id,TBldgType,THouseStyle,TOverallQual,TYearBuilt){
 #print("inside method,\n")
  a <- (model)[1] 
  #cat('a',a," ",class(a),"\n")
  
  XBldgType2fmCon <- (model)[2]
  XBldgTypeDuplex <- (model)[3]
  XBldgTypeTwnhs <- (model)[4]
  XBldgTypeTwnhsE <- (model)[5]
  XHouseStyle1.5Unf <- (model)[6]
  XHouseStyle1Story <- (model)[7]
  XHouseStyle2.5Fin <-(model)[8]
  XHouseStyle2.5Unf <- (model)[9]
  XHouseStyle2Story <- (model)[10]
  XHouseStyleSFoyer <- (model)[11]
  XHouseStyleSLvl <- (model)[12]
  XOverallQual <- (model)[13]
  XYearBuilt <- (model)[14]
  
  BD <- 0
  HS <- 0
   
  if(TBldgType == '2fmCon'){
     BD <- XBldgType2fmCon
  }
   if (TBldgType == 'Duplex') {
    BD <- XBldgTypeDuplex
  } 
   if (TBldgType == 'Twnhs') {
    BD <- XBldgTypeTwnhs
  } 
   if (TBldgType == 'TwnhsE') {
    BD <- XBldgTypeTwnhsE
  } 
  if (THouseStyle == '1.5Unf') {
    HS <- XHouseStyle1.5Unf
  } 
  if (THouseStyle == '1Story') {
    HS <- XHouseStyle1Story
  } 
  if (THouseStyle == '2.5Fin') {
    HS <- XHouseStyle2.5Fin
  } 
  if (THouseStyle == '2.5Unf') {
    HS <- XHouseStyle2.5Unf
  } 
  if (THouseStyle == '2Story') {
    HS <- XHouseStyle2Story
  } 
  if (THouseStyle == 'SFoyer') {
    HS <- XHouseStyleSFoyer
  } 
  if (THouseStyle == 'SLvl') {
    HS <- XHouseStyleSLvl
  } 
  OV <- XOverallQual
  YB <- XYearBuilt
  
  
  # cat('BD',BD," ",class(BD),"\n")
  # cat('HS',HS," ",class(HS),"\n")
  # cat('OV',OV," ",class(OV),"\n")
  # cat('TOverallQual',TOverallQual," ",class(TOverallQual),"\n")
  # cat('YB',YB," ",class(YB),"\n")
  # cat('TYearBuilt',TYearBuilt," ",class(TYearBuilt),"\n")
  
  YsalesPriceTrain <- a + BD + HS + OV *  TOverallQual + YB * TYearBuilt
  #YsalesPriceTrain <- a + BD + HS + OV
  lis <- c(Id,YsalesPriceTrain)
  #print(lis)
  lis <- unname(lis)
  #print(lis)
  return(lis)
} 

#testing

datahouse <- read.csv("test.csv")
feature_Data <- subset(datahouse,select = c(Id,BldgType,HouseStyle,OverallQual,YearBuilt))
oup <- apply(feature_Data[,c('Id','BldgType','HouseStyle','OverallQual','YearBuilt')], 1, 
             function(x) train_data_salesPrice(coef(model),x['Id'],x['BldgType'],x['HouseStyle'],as.numeric(x['OverallQual']), as.numeric(x['YearBuilt'])))

finalOP <- as.data.frame(t(oup))
names(finalOP)[1] = "ID"
names(finalOP)[2] = "SalesPrice"
write.csv(finalOP,file = "Ttestedthis.csv")

