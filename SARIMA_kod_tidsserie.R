##Ladda ner paketer
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
install.packages("kableExtra")
library(kableExtra)

##Hämta data
#Välj 1,1,2,1,21,2,1,1,1,1,1,1:227,y,n,y,y
data_bensin <- pxweb_interactive()
data_bensin <- data_bensin$data
ts_data_bensin <- ts(data_bensin$Antal, start = 2006, frequency = 12)
ts_data_bensin_log <- log(ts_data_bensin)
plot(ts_data_bensin_log, 
     xlab = "Tid", 
     ylab = "ln(Antal nyregistrerade personbilar)",
     main = "Utveckling av nyregistrerade bensinpersonbilar i Sverige – Naturlig Logaritm")

##SAC
acf(x = as.vector(ts_data_bensin_log), main = "ACF för residualer för ln(Antal nyregistrerad personbilar)")

# Konvertera `månad` till en Date-klass
data_bensin <- data_bensin |>
  mutate(
    månad = yearmonth(månad) # Konvertera till år-månad-format
  )

data_bensin <- as_tsibble(data_bensin)

#Säsongsdifferenciering och kontrollera SAC
diff12 <- diff(ts_data_bensin_log, lag = 12)
acf(x = as.vector(diff12), lag.max = 50, main = "ACF för residualer för diff12ln(Antal nyregistrerad personbilar)")
pacf(x = as.vector(diff12), lag.max = 50)
adf.test(diff12) #Testa om serien är stationär

#Trendsdifferenciering ytterligare
diff12_1 <- diff(diff12)
acf(x = as.vector(diff12_1), lag.max = 50, main = "ACF för residualer för diff12_1ln(Antal nyregistrerad personbilar)")
pacf(x = as.vector(diff12_1), lag.max = 50,  main = "PACF för residualer för diff12_1ln(Antal nyregistrerad personbilar)")
adf.test(diff12_1)

##Anpassa SARIMA
fit <- data_bensin |>
  model(arima310011 = ARIMA(log(Antal) ~ 0 + pdq(3,1,0) + PDQ(0,1,1, period = 12)),
        auto = ARIMA(log(Antal), stepwise = FALSE, approx = FALSE))

#Modellspecifikation
fit |> pivot_longer(everything(), names_to = "Model name",
                    values_to = "Orders")


##Kontrollera residualer
fit |> 
  select(arima310011) |>
  gg_tsresiduals(lag_max = 50) + 
  ggtitle("SARIMA(3,1,0)(0,1,1)[12] Model")
fit |> 
  select(auto) |>
  gg_tsresiduals(lag_max=50) + 
  ggtitle("SARIMA(1,0,1)(0,1,1)[12] Model")

##Ljungbox-statistika och snygga till i en tabell
augment(fit) |>
  filter(.model == "arima310011") |>
  features(.innov, ljung_box, lag=50, dof=4)
augment(fit) |>
  filter(.model == "auto") |>
  features(.innov, ljung_box, lag=50, dof=3)
# Skapa tibble för båda modellerna
result <- tibble(
  Model = c("SARIMA310011[12]", "SARIMA101011[12]"),
  lb_stat = c(61.2, 61.9),
  lb_pvalue = c(0.0658, 0.0710)
)
# Använd kable för att skapa tabell
result %>%
  kable(digits = c(0, 1, 4)) %>%  # Justera antal decimaler för varje kolumn
  kable_styling("striped", full_width = F)  # Gör tabellen stilren


## Skattade koefficienter och standardfel och visualisera i en tabell för SARIMA101011
fit %>%
  select(auto) %>%
  report() 
coefficients <- c(0.9891,-0.5392,-0.8505)
standard_errors <- c(0.0123,0.0569,0.0544)
# Beräkna z-värden
z_values <- coefficients / standard_errors
# Beräkna p-värden (baserat på normalfördelning)
p_values <- 2 * (1 - pnorm(abs(z_values)))
# Visa resultaten
result <- data.frame(
  Coefficient = coefficients,
  StdError = standard_errors,
  tValue = z_values,
  pValue = p_values
)
result <- round(result, 3)
# Visa det avrundade resultatet
print(result)
# Uppdaterad tibble med nya värden
coef_table <- tibble(
  Parameter = c("ar1", "ma1", "sma1"),
  Coefficient = c(0.989, -0.539, -0.851),
  StdError = c(0.012, 0.057, 0.054),
  tValue = c(80.415, -9.476, -15.634),
  pValue = c(0, 0, 0)
)
# Skapa tabellen med kable
coef_table %>%
  kable(digits = c(0, 3, 3, 3, 4)) %>%
  kable_styling("striped", full_width = F)

## Skattade koefficienter och standardfel och visualisera i en tabell för SARIMA310011
fit %>%
  select(arima310011) %>%
  report() 
coefficients2 <- c(-0.5709,  -0.2605,  -0.1168,  -0.8467)
standard_errors2 <- c(0.0679,   0.0764,   0.0679,   0.0532)

# Beräkna z-värden
z_values2 <- coefficients2 / standard_errors2
# Beräkna p-värden (baserat på normalfördelning)
p_values2 <- 2 * (1 - pnorm(abs(z_values2)))
# Visa resultaten
result2 <- data.frame(
  Coefficient = coefficients2,
  StdError = standard_errors2,
  tValue = z_values2,
  pValue = p_values2
)
result2 <- round(result2, 3)
# Visa det avrundade resultatet
print(result2)

# Skapa en tibble med parametrar och deras resultat
coef_table2 <- tibble(
  Parameter = c("ar1", "ar2", "ar3", "sma1"),
  Coefficient = c(-0.5709,  -0.2605,  -0.1168,  -0.8467),
  StdError = c(0.0679,   0.0764,   0.0679,   0.0532),
  tValue = c( -8.407953,  -3.409686,  -1.720177, -15.915414),
  pValue = c(0.0000000000, 0.0006503774, 0.0854003215, 0.0000000000)
)

# Skapa tabellen med kable
coef_table2 %>%
  kable(digits = 4) %>%
  kable_styling("striped", full_width = F)

##Några utvärderingsmått
glance(fit) |> arrange(AICc) |> select(.model:BIC)
# Skapa tabellen med kable, ändra namn på .model och styling
glance(fit) %>%
  arrange(AICc) %>%
  select(.model, sigma2, log_lik, AIC, AICc, BIC) %>%
  mutate(
    .model = case_when(
      .model == "auto" ~ "SARIMA101011[12]",
      .model == "arima310011" ~ "SARIMA310011[12]",
      TRUE ~ .model
    )
  ) %>%
  kable(digits = c(0, 4, 1, 2, 2, 2)) %>%
  kable_styling("striped", full_width = F)



##Utvärderingsmått
fitted_values_original <- augment(fit) %>%
  filter(.model == "auto") %>%
  pull(.fitted)

actual_values <- data_bensin$Antal

mape <- mean(abs((actual_values - fitted_values_original) / actual_values)) * 100
mad <- mean(abs(actual_values - fitted_values_original))
msd <- mean((actual_values - fitted_values_original)^2)


# Generera tabellen med kableExtra
library(kableExtra)

##Tabell med utvärderingsmått för 4 modeller
metrics_table <- data.frame(
  Model = c("Klassisk komponentuppdelning", "Holt Winter", "SARIMA(1,0,1)(0,1,1)", "Transfer function-noise model"),
  MAPE = c(7.8356, 11, 9.956495, 8.565811),
  MAD = c(812.921, 1048, 953.146, 774.0889), 
  MSD = c(2680704, 3268920, 3191080, 1150427)
)

metrics_table |>
  kable(
    col.names = c("Model", "MAPE (%)", "MAD", "MSD"),
    format = "html",
    digits = 2,
    caption = "Felmått för olika modeller"
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))

##Göra prognos
forecast(fit, h = 12) |>
  filter(.model == 'auto') |>
  autoplot(data_bensin) +
  ggtitle("Prognos för nyregistrerade personbilar på bensin med SARIMA-modellen") +
  xlab("Tid") +  # Anpassa x-axelns etikett om du vill
  ylab("Nyregistrerade personbilar") +  # Anpassa y-axelns etikett
  theme_minimal() + 
  geom_line(aes( y = fitted_values_original), 
            color = "blue", linetype = "solid", size = 1, 
            show.legend = TRUE)
# Extrahera månad och predikterade värden
forecast_table <- forecast(fit, h = 12) |>
  filter(.model == 'auto') |>
  as_tibble() |>
  select(månad, .mean) # Välj kolumnerna månad och predikterade värden


# Skapa en tabell med kable
forecast_table |>
  mutate(.mean = round(.mean, 2)) |> 
  kable(
    col.names = c("Månad", "Predikterade värden"),
    format = "html",
    caption = "Prognos för nyregistrerade personbilar"
  ) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed"))


