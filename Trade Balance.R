setwd("~/OneDrive/Desktop/fin_time_report")
library(lmtest)
library(readxl)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(fGarch)  
library(forecast)
library(lubridate)
library(fastDummies)
library(rugarch)
options(scipen=999)

### importing data ###
data<-read_xlsx("foreign_trade_by_country_eng.xlsx")
data$date<-as.Date(data$date)

### calculating trade balance ###

data<-data%>%mutate(tb=(exp-imp)/1000)


### ploting trade balance ###

ggplot(data=data,aes(data$date,data$tb))+
  theme_ipsum(base_size=10,axis_title_just = "mc",plot_title_size = 14,axis_title_size = 10,axis_text_size=8,
              caption_margin =20)+
  theme(plot.caption = element_text(hjust=0),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.x=element_line(color="#238A8DFF",size=0.2),
        panel.grid.minor.x=element_line(color="#238A8DFF",size=0.2))+
  
  
  geom_line( color="#440154FF",size=0.3) +
  
  xlab("Time")+ylab("Trade balance")+
  scale_x_date("Time",date_breaks ="2 year", date_labels ="%Y")+
  labs(caption = "Data sources: National Bank of Georgia")

ggsave("TB.png",dpi=1200,width =7,height = 4)


### ADF test ###
adf.test(data$tb)

### decomposition ###

count_ma=ts(data$tb,frequency=12)
decomp=stl(count_ma,s.window="periodic")
deseasonal_cnt<-seasadj(decomp)
plot(decomp)

# save plot
png("decomp.png", width=300, height=250, units="mm", res=600,
    pointsize = 20)
plot(decomp)
dev.off()


### differences ###

# first difference

data$dtb<-c(NA,diff(data$tb))

# seasonal difference

data$sdtb<-c(rep(NA,12),diff(data$tb,lag=12))

# both difference

data$ddtb<-c(rep(NA,12),diff(data$dtb,lag=12))


#ploting ACF 

tb_acf<-acf(data$tb,lag=100,plot=FALSE)
dtb_acf<-acf(na.omit(data$dtb),lag=100,plot=FALSE)
sdtb_acf<-acf(na.omit(data$sdtb),lag=100,plot=FALSE)
ddtb_acf<-acf(na.omit(data$ddtb),lag=40,plot=FALSE)

png("difference_acf.png", width=300, height=250, units="mm", res=600,
    pointsize = 15)
par(mfcol=c(2,2))
plot(tb_acf,main="Trade Balance")
plot(dtb_acf,main="First Difference")
plot(sdtb_acf,main="Seasonal Difference")
plot(ddtb_acf,main="Seasonal & First Difference")
dev.off()


### create train & test ###
round(0.8*nrow(data))/12
20*12
train <- data[1:240,]
test <- data[241:nrow(data),]







########## airline model ########## 

m<-NULL
m[[1]]<-arima(train$tb,order=c(1,0,0),seasonal=list(order=c(1,0,1),period=12))
m[[2]]<-arima(train$tb,order=c(1,0,2),seasonal=list(order=c(1,1,1),period=12))
m[[3]]<-arima(train$tb,order=c(2,0,2),seasonal=list(order=c(1,1,1),period=12))
m[[4]]<-arima(train$tb,order=c(1,0,3),seasonal=list(order=c(1,1,1),period=12))
m[[5]]<-arima(train$tb,order=c(1,0,4),seasonal=list(order=c(1,1,1),period=12))
m[[6]]<-arima(train$tb,order=c(3,0,4),seasonal=list(order=c(1,1,1),period=12))
m[[7]]<-arima(train$tb,order=c(3,0,5),seasonal=list(order=c(1,1,1),period=12))
m[[8]]<-arima(train$tb,order=c(4,0,5),seasonal=list(order=c(1,1,1),period=12))
m[[9]]<-arima(train$tb,order=c(5,0,5),seasonal=list(order=c(1,1,1),period=12))



### RMSE for every model ###
accuracy<-as.data.frame(1:9)
accuracy$RMSE<-c(1:9)
arima_pred<-NULL


for(i in c(1:9)){
  arima_pred[[i]] <-forecast(m[[i]],h=64)
  accuracy[i,"RMSE"]<-as.data.frame(accuracy(arima_pred[[i]],test$tb))$RMSE[1]
}

### AIC ###
AIC<-as.data.frame(1:9)
AIC$aic<-c(1:9)

for(i in c(1:9)){
  AIC[i,"aic"]<-as.data.frame(m[[i]]$aic)
}

### lujung ###

lujung<-as.data.frame(1:9)
lujung$luj<-c(1:9)
res<-NULL
for(i in c(1:9)){
  res[[i]] <-residuals(m[[i]],standardize=T)
  lujung[i,"luj"]<-as.data.frame(Box.test(res[[i]],lag=12,type="Ljung")$p.value)
}


statistics<-left_join(accuracy,AIC)%>%left_join(lujung)


### estimates ###

coeftest(m[[7]])


sink("ARIMA coeff.doc")
coeftest(m[[7]])
sink()

### plot residuals ###

png("Residuals_1.png", width=300, height=250, units="mm", res=600,
    pointsize = 20)

m90%>%
  residuals() %>% ggtsdisplay(ylab="Residuals",theme=theme(axis.title.y = element_text(size=15)))

dev.off()




### plot predictions ###
png("forecast 1.png", width=350, height=200, units="mm", res=600,
pointsize = 15)


plot(arima_pred[[7]],xlim=c(0,250),showgap=FALSE,fcol="#440154FF",flwd = 2)
lines(c(rep(NA,240),test$tb), col="red",lwd=2)
legend("bottomleft",lty=1,bty = "n",col=c("red","#440154FF"),c("Test Data","Prediction"))


dev.off()


### plot squared residuals ###

resid<-residuals(m[[7]],standardize=T)
png("residuals^2.png", width=350, height=200, units="mm", res=600,
    pointsize = 15)
pacf(resid^2,25)
dev.off()



### testing for arch effects ###
Box.test(resid^2,lag=10,type='Ljung') 







############# ARMA-GARCH ############



### seasonal dummies ###
data<-read_xlsx("foreign_trade_by_country_eng.xlsx")
data$date<-as.Date(data$date)
data<-data%>%mutate(tb=(exp-imp)/1000)

data1<-data

data1$month<-as.factor(month(as.POSIXlt(data1$date, format="%Y/%m/%d")))
results <- dummy_cols(data1$month)
data2<-cbind(data1,results)
data2$month<-NULL
data2$.data<-NULL

### trend ###
data2$t<-c(1:nrow(data2))
data2$t2<-data2$t^2
dummy<-data.matrix(data2[,c(6:16,18)])
data3<-ts(data2,start=c(1995, 1), end=c(2020, 4), frequency=12) 


### GARCH specification ###


spec<-NULL
spec[[1]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                         submodel = NULL),
                   mean.model = list(armaOrder = c(1,0),include.mean=TRUE,external.regressors=dummy),
                   distribution.model = "norm")
spec[[2]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(1,1),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[3]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(2,1),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[4]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(3,0),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[5]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,1),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(1,2),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[6]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(1,0),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[7]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(1,1),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[8]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(2,0),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[9]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(2,1),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")
spec[[10]] <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1,2),
                                              submodel = NULL),
                        mean.model = list(armaOrder = c(3,0),include.mean=TRUE,external.regressors=dummy),
                        distribution.model = "norm")


### choosing GARCH ###
garch<-NULL
AIC_garch<-NULL
AIC_garch$AIC<-0
predict<-NULL
MSE<-NULL
MSE$mse<-0
for (i in 1:10){
  garch[[i]]<-ugarchfit(spec=spec[[i]],data=data3[,"tb"],out.sample =64)
  AIC_garch$AIC[i]<-infocriteria(garch[[i]])[1]
  predict[[i]]<-ugarchforecast(garch[[i]], data =data3[,"tb"], n.ahead = 1, n.roll = 64,
                          out.sample =64 )
  MSE$mse[i]<-fpm(predict[[i]])[1]
}
stat<-as.data.frame(cbind(AIC_garch[["AIC"]],MSE[["mse"]]))


### testing residuals ###

res<-residuals(garch[[4]],standardize=T)
plot(res)
Box.test(res^2,lag=10,type='Ljung') 
Box.test(res,lag=10,type='Ljung')


### prediction ###
### 
garch[[4]]<-ugarchfit(spec=spec[[4]],data=data3[,"tb"],out.sample =64)


read.csv()
predict<-ugarchforecast(garch[[4]], data =data3[,"tb"], n.ahead = 1, n.roll = 64,
                        out.sample =64 )



png("Garch predict.png", width=350, height=200, units="mm", res=600,
    pointsize = 20)
plot(predict,which=2)
dev.off()

