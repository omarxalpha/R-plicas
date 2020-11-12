# VAR que pretende mostrar la relaci?n entre los movimientos del TCR y la formaci?n bruta de K

setwd("C:/Users/52557/Documents/tesis_ultimate/BD")
getwd()

library(readxl)
library(zoo)
library(lmtest)

FBK = read_xlsx("FBK.xlsx")

ts_FBK = ts(log(FBK[, 2:4]), start = c(1993,1), end = c(2019,11), freq = 12)
dts_FBK = diff(ts_FBK)

TCR = read_xlsx("TCR_MEX_BM.xlsx")

ts_TCR = ts(log(TCR[37:359,2]), start = c(1993,1), end = c(2019, 11), freq= 12)
dts_TCR = diff(ts_TCR)

plot(diff(ts_TCR))
plot(diff(ts_FBK))

acf(ts_FBK[,1])
acf(ts_FBK[,2])
acf(ts_FBK[,3])

acf(ts.union(ts_TCR, ts_FBK[,3]))

hist(ts_TCR)
par(mfrow = c(1,1))
plot(dts_TCR)
plot(dts_FBK[,1], col="blue")
plot(dts_FBK[,2], col="red")
plot(dts_FBK[,3], col="green4")

data = data.frame(INV_g=dts_FBK[,1], TCR = dts_TCR)
data2 = data.frame(INV_c=dts_FBK[,2], TCR = dts_TCR)
data3 = data.frame(INV_mye=dts_FBK[,3], TCR = dts_TCR)

par(mfrow = c(2,1))
plot(ts_TCR)
acf(ts_TCR)
acf(ts_FBK[,2])

ccf(ts_FBK[,1])





summary(VAR(data))
summary(VAR(data2))
summary(VAR(data3))

# Estima Causalidad en el sentido de Granger 

Datos = data.frame(cbind(dts_FBK, dts_TCR))

names(Datos) = c('LINVg', 'LINVc', 'LINVmye', 'LTCR')

#Inversi?n general 

grangertest(LINVg~LTCR,order=4, data=Datos)
grangertest(LTCR~LINVg,order=4, data=Datos)

grangertest(LINVg~LTCR,order=8, data=Datos)
grangertest(LTCR~LINVg,order=8, data=Datos)

grangertest(LINVg~LTCR,order=12, data=Datos)
grangertest(LTCR~LINVg,order=12, data=Datos)

#Inversi?n eon costrucci?n 

grangertest(LINVc~LTCR,order=4, data=Datos)
grangertest(LTCR~LINVc,order=4, data=Datos)

grangertest(LINVc~LTCR,order=8, data=Datos)
grangertest(LTCR~LINVc,order=8, data=Datos)

grangertest(LINVc~LTCR,order=12, data=Datos)
grangertest(LTCR~LINVc,order=12, data=Datos)

#Inversi?n Equipo y Maquinaria 

grangertest(LINVmye~LTCR,order=4, data=Datos)
grangertest(LTCR~LINVmye,order=4, data=Datos)

grangertest(LINVmye~LTCR,order=8, data=Datos)
grangertest(LTCR~LINVmye,order=8, data=Datos)

grangertest(LINVmye~LTCR,order=12, data=Datos)
grangertest(LTCR~LINVmye,order=12, data=Datos)


lagplot(ts_TCR)
plot(ts_FBK[,1])
  