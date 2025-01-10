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

##Hämta data
#Välj 1,1,2,1,21,2,1,1,1,1,1,1:227,y,n,y,y
data_bensin <- pxweb_interactive()
data_bensin <- data_bensin$data
ts_data_bensin <- ts(data_bensin$Antal, start = 2006, frequency = 12) #Skapa en tidsserie
time <- as.Date(ts_data_bensin) #Skapa time som en vektor med datumn från 01/01/2006 till 01/11/2024



# Komponentuppdelning för data_bensin
plot(ts_data_bensin)  # kolla på data


# vi börjar med en multiplikativ modell
# filter decompose() bestämmer vikterna i glidande medelvärde och 
# ska summera till 1.

filter_length <- 12 # vi bestämmer längden på filtret
my_filter1<-rep(1/filter_length,filter_length) # vi låter alla obs i filtret ha samma vikt

G1<-decompose(ts_data_bensin,type = "mult",filter = my_filter1)
plot(G1) # Skapar diagrammet

par(mfrow=c(1,1))

## olika delar av uppdelningen:
str(G1)

# kolla på trend
plot(G1$trend)
# slumptermen/residualerna:
plot(G1$random)

##Skattade värden
y_hat1<-G1$seasonal*G1$trend

##Residualanalys
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")
residual_diagnostics(res_vect = as.vector(G1$random-1),fit_vect = as.vector(y_hat1))
acf(as.vector(na.omit(G1$random)), main = "ACF för residualer på den multiplikativa modell för bensinbilar", 
    lag.max = 40)

# testar en additiv modell
G2<-decompose(ts_data_bensin,type = "add",filter = my_filter1)
plot(G2)
# olika delar av uppdelningen:
str(G2)

#Skattade värden
y_hat2<-G2$trend+G2$seasonal

plot(time,ts_data_bensin,t="o",lwd=1.5)
lines(time,y_hat2,col="red",t="l",lwd=1.5)
# en sämre anpassning här!


# utvärderingsmått:
MSD_G1<-mean((ts_data_bensin-y_hat1)^2,na.rm = TRUE)
MSD_G2<-mean((ts_data_bensin-y_hat2)^2,na.rm = TRUE)

MAD_G1<-mean(abs(ts_data_bensin-y_hat1),na.rm = TRUE)
MAD_G2<-mean(abs(ts_data_bensin-y_hat2),na.rm = TRUE)


MAPE_G1<-mean(abs((ts_data_bensin-y_hat1)/ts_data_bensin),na.rm = TRUE) * 100
MAPE_G2<-mean(abs((ts_data_bensin-y_hat2)/ts_data_bensin),na.rm = TRUE) * 100

df_error <- rbind(data.frame(MSD=MSD_G1,MAD=MAD_G1,MAPE=MAPE_G1), data.frame(MSD = MSD_G2, MAD = MAD_G2, MAPE = MAPE_G2))

rownames(df_error)<- c("Multiplikativ modell", "Additiv modell")


df_error %>% 
  kable(
    digits = 4
  ) %>% 
  kable_styling(
    "striped", 
    font_size = 20,  # Ökar textstorleken
    full_width = FALSE, # Gör tabellen kompaktare
    position = "center" # Centrerar tabellen
  )

##Göra prognos
last_trend_value <- G1$trend[221]  # sista skattade trendvärdet som inte är NA
# Skapa en linjär extrapolation för trenden 
trend_extrapolated <- rep(last_trend_value, 12)  # Extrapolera de nästa 12 värdena

# Eftersom vi använder multiplicativ dekomposition behöver vi också säsongskomponenten
seasonal_component <- c(tail(G1$seasonal, 6), G1$seasonal[12], G1$seasonal[1:5]) #Hämtar säsongsfaktorerna för de 12 observationerna som skulle skattas

# Prognosen = trenden * säsongen 
forecast_values <- trend_extrapolated * seasonal_component 

## Visa prognosen i plottar och tabell
forecast_values

extended_time <- seq(from = min(time), by = "month", length.out = 233)


plot(time,ts_data_bensin,t="o",lwd=1.5,
     xlab = "Tid", ylab = "Nyregistrerade personbilar",
     main = "Klassisk komponentuppdelning av tidsserie")
lines(time,y_hat1,col="red",t="l",lwd=1.5)
lines(extended_time[222:233], forecast_values, col = "blue", type = "l", lwd = 2)


legend("topright", legend=c("Actual", "Fits", "Prognos"), 
       col=c("black", "red", "blue"), lty=c(1, 1,1), pch=c(1, NA, NA), lwd=1.5, title="Variabels")
legend("top", 
       legend=c(paste("MSD: ", round(MSD_G1, 3)), 
                paste("MAD: ", round(MAD_G1, 3)), 
                paste("MAPE: ", round(MAPE_G1, 3))), 
       title="Accuracy Measures",
       bty="o", # Lägg till en ram
       cex=0.7, # Justera textstorlek
       box.lty=1, # Gör ramen heldragen
       box.col="black")

##Skaffa tabell med prognos
forecast_df <- data.frame(
  Månad = extended_time[222:233],  # Månader från juni 2024 till maj 2025
  Forecast = forecast_values)  # Prognosvärden från din tidigare beräkning

# Visa tabellen med kable
forecast_df %>%
  kable("html", col.names = c("Månad", "Predikterade värde"), caption = "Prognosvärden från juni 2024 till maj 2025") %>%
  kable_styling("striped", full_width = F)

