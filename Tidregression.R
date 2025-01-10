#-------------------------------------------------------------------------------
# Tidserieregression: data_bensin
#-------------------------------------------------------------------------------
##Ladda ner paketer
install.packages("pxwed_interactive")
library(pxweb)
install.packages("kableExtra")
library(kableExtra)
install.packages("cowplot")
library(cowplot)
install.packages("zoo")
library(zoo)
install.packages("tidyverse")
library(tidyverse)
install.packages("kableExtra")
library(kableExtra)

data_bensin <- pxweb_interactive()

data_bensin <- data_bensin$data
ts_data_bensin <- ts(data_bensin$Antal, start = 2006, frequency = 12)

plot(ts_data_bensin)
library(lubridate)

Time<-as.Date(ts_data_bensin)

time_index<-1:nobs



#-------------------------------------------------------------------------------
# Tidserieregression: säsongsmodellering + linjär trend
#-------------------------------------------------------------------------------


# lägg till modell för månader:
X_month<-matrix(0,nrow = nobs,ncol = 12)
colnames(X_month)<- tolower(month.abb)
month_val<-month(as.Date(ts_data_bensin))
for(i in 1:12){
  X_month[,i]<-ifelse(month_val==i,1,0)
}
head(X_month,15)

# designmatris (= en matris med alla förklarande variabler)
# ska ha en kolumn med bara ettor, men lm lägger till det automatiskt, så
# behöver inte tänka på det:

# tar bort april -> referenskategori
X<-cbind(time = time_index, X_month[,-4])
reg_data<-data.frame(y=as.vector(ts_data_bensin),X)
head(reg_data)
(nobs/12) # 18.91667 år
# vi skattar vår modell på de 17 första åren
# sen gör vi prediktion på det sista året

# träningsdata:
train_index<-1:204
reg_data_train<-reg_data[train_index,]
# testdata (för prognos)
test_index<-205:227
reg_data_test<-reg_data[test_index,]

lm_temp2<-lm(log(y)~.,data=reg_data_train)
summary(lm_temp2)

res_vect<-residuals(lm_temp2)
fit_vect<-fitted(lm_temp2)
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)
# verkar finnas beroende kvar i index vs res, men ser bättre ut än tidigare!

acf(res_vect,lag=60)
# ser inte bra ut! Vi vill ha oberoende residualer om möjligt

# Notera: om antaganden som krävs för test/konfidensintervall inte är uppfyllda,
# då ska vi inte göra test/konfidensintervall för tex beta-skattningarna.
# vi kan dock undersöka punktkattningar av parametrar och göra punktprognoser.

library(lmtest)
dwtest(formula = lm_temp2,alternative = "two.sided") 

install.packages("car")
library(car)
durbinWatsonTest(model = lm_temp2)


summary(lm_temp2)
round(coef(lm_temp2),3)
barplot(coef(lm_temp2)[-1])
# hur tolkar vi beta-hat när vi har tagit log-transform på y?
barplot(exp(coef(lm_temp2)[-1]))


#-------------------------------------------------------------------------------
# anpassade värden och prediktion
# notera: eftersom residualerna inte är oberoende så bör vi vara försiktiga med
# att beräkna predikitonsintervall för prognoser etc. Nedan visas kod hur man kan 
# göra det. (likt fallet med tempdub-data)


# anpassade värden på träningsdata:
plot(Time,ts_data_bensin,t="o",lwd=2)
lines(Time[train_index],exp(fitted(lm_temp2)),col="blue",lwd=1.5)

# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred<-predict(lm_temp2, newdata = reg_data_test[,-1], interval = 'prediction')
lm_temp2_pred

# test data:
plot(Time,ts_data_bensin,t="o",lwd=2)

# anpassade värden för träningsdata:
# notera att tar exp() på anpassade värden och intervall gränserna nedan.
lines(Time[train_index],exp(fitted(lm_temp2)),col="blue",lwd=1.5)
# anpassade värden för testdata:
lines(Time[test_index],exp(lm_temp2_pred[,1]),col="red",lwd=1.5)
# undre prediktionsgräns
lines(Time[test_index],exp(lm_temp2_pred[,2]),col="red",lwd=1.5,lty="dashed")
# övre prediktionsgräns
lines(Time[test_index],exp(lm_temp2_pred[,3]),col="red",lwd=1.5,lty="dashed")



reg_data2<-reg_data
reg_data2$type<-"train"
reg_data2$type[test_index]<-"prediction"

# beräkna prediktionsintervall för alla obs:
# reg_data_test[,-1] = alla förklarande variabler för testdata (tagit bort y)
lm_temp2_pred_all<-predict(lm_temp2, newdata = reg_data[,-1], interval = 'prediction')
lm_temp2_pred_all

# notera att tar exp() på anpassade värden och intervall gränserna nedan.
reg_data2$fitted<-exp(lm_temp2_pred_all[,1])
reg_data2$pi_lwr<-exp(lm_temp2_pred_all[,2])
reg_data2$pi_upr<-exp(lm_temp2_pred_all[,3])
reg_data2$date<-Time
head(reg_data2,4)


# plotta: data, anpassade värden, prediktioner och prediktionsintervall:
ggplot(data = reg_data2,aes(x=date,y=y))+geom_line()+theme_bw()+
  geom_line(aes(y=fitted,col=type))+
  geom_ribbon(aes(ymin=pi_lwr,ymax=pi_upr,fill=type),alpha=0.4)


res_vect<-residuals(lm_temp2)
fit_vect<-fitted(lm_temp2)

# inspect residuals:
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")
residual_diagnostics(res_vect = res_vect,fit_vect = fit_vect)

