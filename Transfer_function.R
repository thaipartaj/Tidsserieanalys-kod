install.packages("pxweb")
library(pxweb)
install.packages("tidyverse")
library(tidyverse)
install.packages("tsibble")
library(tsibble)
install.packages("urca")
library(urca)
library(ggplot2)
install.packages("feasts")
library(feasts)
install.packages("forecast")
library(forecast)
install.packages("fable")
library(fable)
install.packages("tseries")
library(tseries)
install.packages("TSA")
library(TSA)
install.packages("cowplot")
library(cowplot)
install.packages("kableExtra")
library(kableExtra)

##Hämta data
#Välj 1,1,2,1,21,2,1,1,1,1,1,1:227,y,n,y,y
data_bensin <- pxweb_interactive()
data_bensin <- data_bensin$data
ts_data_bensin <- ts(data_bensin$Antal, start = 2006, frequency = 12)

# Skapa en input_series där interventionen sker vid specifika index
interventions1 <- 150
interventions2 <- 168
interventions3 <- 183  
input_series1 <- rep(0, 227)  # Skapa en serie med 0:or
input_series2 <- rep(0, 227)  # Skapa en serie med 0:or
input_series3 <- rep(0, 227)  # Skapa en serie med 0:or

# Sätt värdet till 3 vid de specifika interventionerna
input_series1[interventions1] <- 1
input_series2[interventions2] <- 1
input_series3[interventions3] <- 1

which(input_series1 == 1)
which(input_series2 == 1)
which(input_series3 == 1)

input_series <- rep(0,227)
input_series[c(150,168,183)] <- 1

time <- 1:227
# Plotta tidsserien med tidindex på x-axeln
par(mfrow = c(2, 1))
plot(input_series, main = "Input Series (Intervention)", ylab = "Input",t="l")
plot(time, ts_data_bensin, type = "l", col = "blue", xlab = "År", ylab = "Värde", main = "Tidsserie med Tidindex")
abline(v=c(150, 168, 183),col="red",lty="dashed")
par(mfrow = c(1, 1))

# Fit a transfer function-noise model (Första gången)
fit1 <- TSA::arimax(ts_data_bensin,
                    order =c(1,0,0),
                    xtransf = data.frame(intervention1 = input_series1,
                                         intervention2 = input_series2,
                                         intervention3 = input_series3), # Intervention
                    transfer = list(c(0,1),
                                    c(0,0),
                                    c(0,0)),   # Transfer function parameters
                    include.mean = TRUE,
                    method="ML")  


#Identifiera en ARMA modell för regression errors
ggtsdisplay(fit1$residuals)
fit.arima.noise <- auto.arima(fit1$residuals)
summary(fit.arima.noise)

#Förbättra den första modellen
fit2 <- TSA::arimax(ts_data_bensin,
                    order =c(3,0,0),
                    seasonal = list(order=c(0,1,2), period = 12),
                    xtransf = data.frame(intervention1 = input_series1,
                                         intervention2 = input_series2,
                                         intervention3 = input_series3), # Intervention
                    transfer = list(c(0,1),
                                    c(0,0),
                                    c(0,0)),   # Transfer function parameters
                    include.mean = TRUE,
                    method="ML")  
lmtest::coeftest(fit2)
Box.test(fit2$residuals, lag = 24, type = "Ljung-Box")

#Identifiera en ARMA modell för regression errors
ggtsdisplay(fit2$residuals, main = "Residualanalys för transfer function-noise model")


coeftest_results <- lmtest::coeftest(fit2)
coeftest_table <- as.data.frame(coeftest_results)
coeftest_table <- coeftest_table[1:9,]

# Använd kable för att skapa tabellen
coeftest_table %>%
  kable(format = "html", digits = 4, caption = "Resultat från coeftest") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE, color = "blue") %>%
  column_spec(2:5, background = "#f7f7f7") %>%
  add_header_above(c(" " = 1, "Statistik" = 4))

# Visualize fitted values vs observed
fitted_values2 <- fitted(fit2)
plot(ts_data_bensin, main = "Observed and Fitted Values för fit2")
lines(fitted_values2, col = "blue", lwd = 2)
legend("topright", legend = c("Observed", "Fitted"), col = c("black", "blue"), lty = 1, lwd = 2)


# inspect residuals:
source("https://raw.githubusercontent.com/STIMALiU/732G52_tsa/refs/heads/main/code/residual_diagnostics.R")

res_vect2<-ts_data_bensin-fitted_values2
residual_diagnostics(res_vect = as.vector(res_vect2),fit_vect = as.vector(fitted_values2))

mape <- mean(abs((ts_data_bensin - na.omit(fitted_values2)) / ts_data_bensin)) * 100
mad <- mean(abs(ts_data_bensin - na.omit(fitted_values2)))
msd <- mean((ts_data_bensin - na.omit(fitted_values2))^2)


# Prognos med konfidensintervall
plot(forecast(fitted(fit2), h=12))
lines(ts_data_bensin, col = "red", lty = 4, lwd = 2)


