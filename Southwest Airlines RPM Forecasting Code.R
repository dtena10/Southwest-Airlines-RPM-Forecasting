#ANNOUNCEMENT: All code considered superfluous is moved to the bottom of the R-script
############## Libraries called #########
library(readxl)
library(tidyverse)
library(fpp3)
library(forecast)
library(patchwork)
library(uroot)
library(seasonal)
library(knitr, kableExtra)
library(hablar)
library(tsibble)
library(dplyr)
library(fpp3)
library(ggplot2)
library(gridExtra)
library(forecast)
library(fable)
library(feasts)

#Data set read into a tsibble object
#Metric = Total Revenue Passenger Miles (RPM)
DATA <- readxl::read_excel("DATA SPREADSHEET Airline Total RPM.xlsx")
DATA <- DATA %>%
  mutate(DATE = sprintf("%d-%02d", Year, Month)) %>%
  mutate(MONTH = yearmonth(DATE)) %>%
  as_tsibble(index = MONTH) %>%
  select(MONTH, `Southwest`)

#Plotting data shows clear signs of seasonality and possible upward trend
plotdata <- DATA %>%
  autoplot(Southwest) +
  labs(title="Whole Sample") + xlab("Month") +
  geom_line(linewidth=1) 
plotdata

############## Plotting Data Raw and Log-Diff #######
plot <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`RPM`= (Southwest)) %>%
  autoplot(`RPM`) +
  labs(title="Panel (a): Total Revenue Passenger Miles") +
  geom_line(color='black', size=0.5)
plot3 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log-Diff RPM`= difference(log(Southwest),1)) %>%
  autoplot(`Log-Diff RPM`) +
  labs(title="Panel (b): First Difference of Log Total Revenue Passenger Miles") +
  geom_line(color='black', size=0.5)

plot1 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log-Diff RPM`= difference(log(Southwest),1)) %>%
  ACF(`Log-Diff RPM`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log-Diff RPM`= difference(log(Southwest),1)) %>%
  PACF(`Log-Diff RPM`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")

multifig <- (plot / plot3)
multifig
multifig <-  (plot1 / plot2) 
multifig

plot1 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`RPM`= log(Southwest)) %>%
  ACF(`RPM`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log RPM") + xlab("Lag")
plot2 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`RPM`= log(Southwest)) %>%
  PACF(`RPM`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig

#HEGY Seasonal Dummy Test
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log RPM` = difference(log(Southwest),1)) %>%
  pull(`Log RPM`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,1), lag.method="BIC", maxlag=12) 
#HEGY Base
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log RPM` = difference(log(Southwest),1)) %>%
  pull(`Log RPM`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,0), lag.method="BIC", maxlag=12) 
#ADF Base
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Log RPM` = difference(log(Southwest),1)) %>%
  drop_na(`Log RPM`) %>%
  pull(`Log RPM`) %>% 
  urca::ur.df(type = c("none"), lags=1) %>% 
  urca::summary()

############## Seasonality exists beyond differencing #######################
#Monthly seasonal data, in line coneptually with airlines 
DATADIF <- DATA %>%
  transmute(`Log-Diff RPM` = difference(log(Southwest),1) ) %>%
  drop_na(`Log-Diff RPM`) #remove NA for 1st obs 
plotres <- DATADIF %>%
  gg_subseries(`Log-Diff RPM`)
plotres

DATA <- DATA %>%
  mutate("Log RPM" = log(Southwest))

############## Check For Seasonality ##################
plot <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  autoplot(`Double Differences`) +
  labs(title="Seasonal & Non-seasonal Difference of Log RPM") +
  geom_line(color='black', size=0.5)

plot1.d1 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  ACF(`Double Differences`, lag_max = 24) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,24,1)) +
  labs(title="Panel (a): ACF of Seasonal & Non-seasonal Differenced Log RPM") + xlab("Lag")

plot2.d1 <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  PACF(`Double Differences`, lag_max = 24) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,24,1)) +
  labs(title="Panel (b): PACF of Seasonal & Non-seasonal Differenced Log RPM") + xlab("Lag")

plot
multifig <- ((plot1.d1 / plot2.d1) )
multifig

#Testing for Double Differencing
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  pull(`Double Differences`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,1), lag.method="BIC", maxlag=12) 
#HEGY Base
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  pull(`Double Differences`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,0), lag.method="BIC", maxlag=12) 
#ADF Base
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  urca::ur.df(type = c("none"), lags=4) %>%
  urca::summary()
#ADF
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  urca::ur.df(type = c("drift"), lags=4) %>% 
  urca::summary()
DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  tseries::kpss.test(null = "Level", lshort=FALSE)
#all tests point to stationarity, at least 95% ci

############## Create Holdout & Check for Stationary ################
#remember to explain my holdout selection decision
plot <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  autoplot(`Double Differences`) +
  labs(title="Log Difference RPM Holdout Sample") +
  geom_line(color='black', size=0.5)

plot1.d1 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  ACF(`Double Differences`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="ACF") + xlab("Lag")

plot2.d1 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  PACF(`Double Differences`, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="PACF") + xlab("Lag")

multifig <- ( plot / (plot1.d1 | plot2.d1) )
multifig

#HEGY Season Dummies
DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  pull(`Double Differences`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,1), lag.method="BIC", maxlag=12) 
#HEGY Base
DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  pull(`Double Differences`) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,0), lag.method="BIC", maxlag=12) 
#ADF Base
DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  urca::ur.df(type = c("none"), lags=4) %>%
  urca::summary()
#ADF Trend
DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  urca::ur.df(type = c("drift"), lags=4) %>%
  urca::summary()

#Box Tests Results In Non-Stationarity
DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  transmute(`Double Differences`=difference(difference(log(Southwest)),12)) %>%
  drop_na(`Double Differences`) %>%
  pull(`Double Differences`) %>% 
  Box.test(lag=24, type="Ljung-Box")
  
############## Creating Models #########################

#*****
sarima.212212 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,2) + PDQ(2,1,2), 
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.212212)
#*****
sarima.011111 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(1,1,1), 
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.011111)
#*****
sarima.011211 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,1),
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.011211)
#*****
sarima.211212 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,1) + PDQ(2,1,2),
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.211212)
#*****
sarima.011212 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,2),
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.011212)

sarima.212212 %>%
  gg_tsresiduals()
sarima.011111 %>%
  gg_tsresiduals()
sarima.011211 %>%
  gg_tsresiduals()
sarima.211212%>%
  gg_tsresiduals()
sarima.011212 %>%
  gg_tsresiduals()

ic_table <- bind_rows(
  glance(sarima.212212) %>% mutate(Model ="SARIMA(2,1,2)(2,1,2)"),
  glance(sarima.011111) %>% mutate(Model ="SARIMA(0,1,1)(1,1,1)"),
  glance(sarima.011211) %>% mutate(Model ="SARIMA(0,1,1)(2,1,1)"),
  glance(sarima.211212) %>% mutate(Model ="SARIMA(2,1,1)(2,1,2)"),
  glance(sarima.011212) %>% mutate(Model ="SARIMA(0,1,1)(2,1,2)")
) %>%
  select(Model, AIC, AICc, BIC)

kable(ic_table, caption = "Information Criteria")

############## Forecasting In Holdout ##########################

fit.y <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model("SARIMA(2,1,2)(2,1,2)"=ARIMA(`Log RPM` ~ 1 + pdq(2,1,2) + PDQ(2,1,2)),
        "SARIMA(0,1,1)(1,1,1)"=ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(1,1,1)),
        "SARIMA(0,1,1)(2,1,1)"=ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,1)),
        "SARIMA(2,1,1)(2,1,2)"=ARIMA(`Log RPM` ~ 1 + pdq(2,1,1) + PDQ(2,1,2)),
        "SARIMA(0,1,1)(2,1,2)"=ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,2))) 
forecast.y <- fit.y %>% 
  forecast(h=15)

DATA <-DATA %>%
  mutate(time = row_number())

DATA.holdout <- DATA %>%
  filter_index("2023 Feb" ~ .) 
P <- nrow(DATA.holdout)

#SARIMA(2,1,2)(2,1,2)*****
trained.model <- sarima.212212
sarima.212212.forecast <- trained.model %>% forecast(h=1) 
DATA.holdout$time
for (q in DATA.holdout$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,2) + PDQ(2,1,2), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.212212.forecast <- bind_rows(sarima.212212.forecast, append.forecast)
}
sarima.212212.forecast <- sarima.212212.forecast %>% mutate(.model="SARIMA(2,1,2)(2,1,2)")
kable(sarima.212212.forecast)

#SARIMA(0,1,1)(1,1,1)*****
trained.model <- sarima.011111
sarima.011111.forecast <- trained.model %>% forecast(h=1) 
DATA.holdout$MONTH
for (q in DATA.holdout$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(1,1,1), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.011111.forecast <- bind_rows(sarima.011111.forecast, append.forecast)
}
sarima.011111.forecast <- sarima.011111.forecast %>% mutate(.model="SARIMA(0,1,1)(1,1,1)")
kable(sarima.011111.forecast)

#SARIMA(0,1,1)(2,1,1)*****
trained.model <- sarima.011211
sarima.011211.forecast <- trained.model %>% forecast(h=1) 
DATA.holdout$MONTH
for (q in DATA.holdout$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,1), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.011211.forecast <- bind_rows(sarima.011211.forecast, append.forecast)
}
sarima.011211.forecast <- sarima.011211.forecast %>% mutate(.model="SARIMA(0,1,1)(2,1,1)")
kable(sarima.011211.forecast)

#SARIMA(2,1,1)(2,1,2)*****
trained.model <- sarima.211212
sarima.211212.forecast <- trained.model %>% forecast(h=1) 
DATA.holdout$MONTH
for (q in DATA.holdout$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,1) + PDQ(2,1,2), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.211212.forecast <- bind_rows(sarima.211212.forecast, append.forecast)
}
sarima.211212.forecast <- sarima.211212.forecast %>% mutate(.model="SARIMA(2,1,1)(2,1,2)")
kable(sarima.211212.forecast)

#SARIMA(0,1,1)(2,1,2)*****
trained.model <- sarima.011212
sarima.011212.forecast <- trained.model %>% forecast(h=1) 
DATA.holdout$MONTH
for (q in DATA.holdout$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(0,1,1) + PDQ(2,1,2), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.011212.forecast <- bind_rows(sarima.011212.forecast, append.forecast)
}
sarima.011212.forecast <- sarima.011212.forecast %>% mutate(.model="SARIMA(0,1,1)(2,1,2)")
kable(sarima.011212.forecast)

all_forecasts <- bind_rows(
  sarima.212212.forecast %>% mutate(.model = "SARIMA(2,1,2)(2,1,2)"),
  sarima.011111.forecast %>% mutate(.model = "SARIMA(0,1,1)(1,1,1)"),
  sarima.011211.forecast %>% mutate(.model = "SARIMA(0,1,1)(2,1,1)"),
  sarima.211212.forecast %>% mutate(.model = "SARIMA(2,1,1)(2,1,2)"),
  sarima.011212.forecast %>% mutate(.model = "SARIMA(0,1,1)(2,1,2)")
)

############## Plotting In Holdout Sample ##########
forecast.y %>%
  autoplot(filter_index(DATA,"2020 Oct" ~ "2023 Jan"), level=NULL, linewidth=1) +
  autolayer(filter_index(DATA, "2023 Feb" ~ .), .vars=`Log RPM`, color = "black", linewidth=0.5, linetype="longdash") +
  labs(title = "Forecasts of Log RPM") +
  guides(colour = guide_legend(title = "Forecast"))

all_forecasts %>%
  autoplot(filter_index(DATA, "2022 Oct" ~ "2023 Jan"), level=NULL) +
  autolayer(sarima.212212.forecast, level = NULL, size = 1, color = "turquoise4") +
  autolayer(sarima.011111.forecast, level = NULL, size = 1, color = "indianred3") +
  autolayer(sarima.011211.forecast, level = NULL, size = 1, color = "deepskyblue2") +
  autolayer(sarima.211212.forecast, level = NULL, size = 1, color = "darkorchid1") +
  autolayer(sarima.011212.forecast, level = NULL, size = 1, color = "chartreuse1") +
  autolayer(DATA.holdout, .vars=`Log RPM`, color = "black", linewidth=0.5, linetype="longdash") +
  labs(title = "Forecasts of Log RPM") +
  guides(colour = guide_legend(title = "Forecast"))

#IC may favor SARMA but forecasting shows failure to adhere to holdout sample

#Plotting Two 'Best Fit' Models
finalplot1 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  autoplot(.vars=`Log RPM`, color = "black", linewidth=0.5,) +
  autolayer(sarima.011111.forecast, level = NULL, size = 1, color = "indianred3") +
  autolayer(DATA.holdout, .vars=`Log RPM`, color = "black", linewidth=0.5, linetype="longdash") +
  labs(title = "Panel (a) Log of RPM via SARIMA(0,1,1)(1,1,1)")
finalplot2 <- DATA %>%
  filter_index("2002 Oct" ~ "2023 Jan") %>%
  autoplot(.vars=`Log RPM`, color = "black", linewidth=0.5,) +
  autolayer(sarima.011211.forecast, level = NULL, size = 1, color = "deepskyblue2") +
  autolayer(DATA.holdout, .vars=`Log RPM`, color = "black", linewidth=0.5, linetype="longdash") +
  labs(title = "Panel (b) Log of RPM via SARIMA(0,1,1)(2,1,1)")
multifig <- (finalplot1/finalplot2)
multifig


############## Frecast Accuracy #################
all_forecasts %>%
  accuracy(DATA.holdout) %>%
  select(-c(.type,MASE,RMSSE)) %>%
  kable()

#Q-test on residuals
#*
rvec <- augment(sarima.212212) %>% pull(.resid)
Box.test(rvec, lag = 24, type = "Ljung-Box")
#*
rvec <- augment(sarima.011111) %>% pull(.resid)
Box.test(rvec, lag = 24, type = "Ljung-Box")
#*
rvec <- augment(sarima.011211) %>% pull(.resid)
Box.test(rvec, lag = 24, type = "Ljung-Box")
#
rvec <- augment(sarima.211212) %>% pull(.resid)
Box.test(rvec, lag = 24, type = "Ljung-Box")
#*
rvec <- augment(sarima.011212) %>% pull(.resid)
Box.test(rvec, lag = 24, type = "Ljung-Box")



#ACF PACF MODEL RESIDUAL 
#1st model selected
plot1 <- augment(sarima.212212) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- augment(sarima.212212) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig

#2nd model selected
plot1 <- augment(sarima.011111) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- augment(sarima.011111) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig


#3rd model selected
plot1 <- augment(sarima.011211) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- augment(sarima.011211) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig


#4th model selected
plot1 <- augment(sarima.211212) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- augment(sarima.211212) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig

#5th model selected
plot1 <- augment(sarima.011212) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (a): ACF for the Log-Diff RPM") + xlab("Lag")
plot2 <- augment(sarima.011212) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,36,1)) +
  labs(title="Panel (b): PACF for the Log-Diff RPM") + xlab("Lag")
multifig <-  (plot1 / plot2) 
multifig

#All models show white noise

#Lets choose two models at best now from all forecasted
varpredict <- sarima.212212.forecast %>%
  left_join(DATA.holdout, by = "MONTH") %>%
  mutate(error = DATA.holdout$`Log RPM` - .mean)
predicterror <- var(varpredict$error)
predicterror
ttest <- t.test(varpredict$error, mu = 0)
ttest
ljung_result <- Box.test(varpredict$error, lag = 24, type = "Ljung-Box")
ljung_result

varpredict <- sarima.011111.forecast %>%
  left_join(DATA.holdout, by = "MONTH") %>%
  mutate(error = DATA.holdout$`Log RPM` - .mean)
predicterror <- var(varpredict$error)
predicterror
mse <- mean(varpredict$error^2)
mse

ttest <- t.test(varpredict$error, mu = 0)
ttest
ljung_result <- Box.test(varpredict$error, lag = 24, type = "Ljung-Box")
ljung_result

varpredict <- sarima.011211.forecast %>%
  left_join(DATA.holdout, by = "MONTH") %>%
  mutate(error = DATA.holdout$`Log RPM` - .mean)
predicterror <- var(varpredict$error)
predicterror
mse <- mean(varpredict$error^2)
mse

ttest <- t.test(varpredict$error, mu = 0)
ttest
ljung_result <- Box.test(varpredict$error, lag = 24, type = "Ljung-Box")
ljung_result

varpredict <- sarima.211212.forecast %>%
  left_join(DATA.holdout, by = "MONTH") %>%
  mutate(error = DATA.holdout$`Log RPM` - .mean)
predicterror <- var(varpredict$error)
predicterror
ttest <- t.test(varpredict$error, mu = 0)
ttest
ljung_result <- Box.test(varpredict$error, lag = 24, type = "Ljung-Box")
ljung_result

varpredict <- sarima.011212.forecast %>%
  left_join(DATA.holdout, by = "MONTH") %>%
  mutate(error = DATA.holdout$`Log RPM` - .mean)
predicterror <- var(varpredict$error)
predicterror
ttest <- t.test(varpredict$error, mu = 0)
ttest
ljung_result <- Box.test(varpredict$error, lag = 24, type = "Ljung-Box")
ljung_result

tidy(sarima.211212)
tidy(sarima.011111)
tidy(sarima.011211)
tidy(sarima.212212)
tidy(sarima.011212)

forecast::dm.test(
  e1=DATA.holdout$`Log RPM`-sarima.011111.forecast$.mean,
  e2=DATA.holdout$`Log RPM`-sarima.011211.forecast$.mean,
  alternative = c("two.sided"), h = 1, power = 2)

######Disclaimer: Superfluous code below########
######Don't Keep########Testing on season####################
fit.ds <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log Southwest` = log(Southwest)) %>%
  model(TSLM(`Log Southwest` ~ 1 + season() ))
report(fit.ds)

plot.fit <- augment(fit.ds) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = `Log Southwest`, colour = "Data"), size=1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size=1) +
  labs(title="Log RPM", 
       subtitle="") +
  scale_color_manual(values=c(Data="gray23",Fitted="royalblue4")) +
  guides(colour = guide_legend(title = NULL)) +
  theme(legend.title=element_blank(),
        legend.position = c(0.98, 0.05),  
        legend.justification = c("right", "bottom") )

plot.resid <- augment(fit.ds) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid), size=1, colour = "turquoise4", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Seasonal Adjusted Residuals") 

plot.resid.d1 <- augment(fit.ds) %>%  
  mutate(d1.resid = difference(.resid,1)) %>%
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = d1.resid), size=0.5, colour = "black", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="First Difference of Residuals") 

multifig <- plot.fit / ( plot.resid | plot.resid.d1 )
multifig

#Data, ACF, PACF
#Nonstationary with trend present
plot.resid <- augment(fit.ds) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid), size=1, colour = "turquoise4", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Seasonally Adjusted Residuals") 
plot1.d1 <- augment(fit.ds) %>%
  ACF(.resid, lag_max = 36) %>%
  autoplot() +
  labs(title="ACF") + xlab("Lag")
plot2.d1 <- augment(fit.ds) %>%
  PACF(.resid, lag_max = 36) %>%
  autoplot() +
  labs(title="PACF") + xlab("Lag")
multifig <- ( plot.resid / (plot1.d1 | plot2.d1) )
multifig

#Tests prove unit root exists beyond seasonal roots
#First differencing will be required
augment(fit.ds) %>%  
  pull(.resid) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,0), lag.method="BIC", maxlag=12)
augment(fit.ds) %>%  
  pull(.resid) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,1), lag.method="BIC", maxlag=12)
augment(fit.ds) %>%  
  pull(.resid) %>% 
  urca::ur.df(type = c("trend"), lags=4) %>% # matches the number of lags
  urca::summary()
#All tests point to 1st diff being necessary 
#Not just seasonal dummies

######Don't Keep########Testing on trend####################
fit.dt <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log Southwest` = log(Southwest)) %>%
  model(TSLM(`Log Southwest` ~ 1 + trend() ))
report(fit.dt)

plot.fit <- augment(fit.dt) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = `Log Southwest`, colour = "Data"), size=1) +
  geom_line(aes(y = .fitted, colour = "Fitted"), size=1) +
  labs(title="Log RPM", 
       subtitle="") +
  scale_color_manual(values=c(Data="gray23",Fitted="royalblue4")) +
  guides(colour = guide_legend(title = NULL)) +
  theme(legend.title=element_blank(),
        legend.position = c(0.98, 0.05),  
        legend.justification = c("right", "bottom") )

plot.resid <- augment(fit.dt) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid), size=1, colour = "turquoise4", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Trend Adjusted Residuals") 

plot.resid.d1 <- augment(fit.dt) %>%  
  mutate(d1.resid = difference(.resid,1)) %>%
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = d1.resid), size=0.5, colour = "black", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="First Difference of Residuals") 

multifig <- plot.fit / ( plot.resid | plot.resid.d1 )
multifig

plot.fit <- augment(fit.dt) %>%  # augments model with .fitted & .resid
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = `Log Southwest`, colour = "Data"), linewidth=1) +
  geom_line(aes(y = .fitted, colour = "Linear DT"), linewidth=1) +
  labs(title="Log RPM") + xlab("Quarter") +
  scale_color_manual(values=c(Data="gray23",`Linear DT`="deepskyblue4")) +
  guides(colour = guide_legend(title = NULL)) + 
  theme( legend.position = c(0.95, 0.05),  # x, y in [0,1] relative to plot area
         legend.justification = c("right", "bottom") )

# Plot fitted residuals
plot.resid <- augment(fit.dt) %>%  # augments model with .fitted & .resid
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid, colour = "Detrended"), linewidth=1) +
  labs(title="Log RPM") + xlab("Quarter") + ylab("Residualized Log GDP") +
  scale_color_manual(values=c(Detrended="deeppink4")) +
  guides(colour = guide_legend(title = NULL)) +
  theme( legend.position = c(0.95, 0.95),  # x, y in [0,1] relative to plot area
         legend.justification = c("right", "top") )

multifig <- plot.fit / plot.resid
multifig

#Data ACF PACF
plot.resid <- augment(fit.dt) %>%  
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid), size=1, colour = "turquoise4", show.legend = FALSE) +
  geom_hline(yintercept=0,lty='dashed') +
  labs(title="Trend Adjusted Residuals") 
plot1.d1 <- augment(fit.dt) %>%
  ACF(.resid, lag_max = 35) %>%
  autoplot() +
  labs(title="ACF") + xlab("Lag")
plot2.d1 <- augment(fit.dt) %>%
  PACF(.resid, lag_max = 35) %>%
  autoplot() +
  labs(title="PACF") + xlab("Lag")
multifig <- ( plot.resid / (plot1.d1 | plot2.d1) )
multifig

#tests tell me that unit root still exits and 
#differencing is what gets rid of trend not trend()
augment(fit.dt) %>%  
  pull(.resid) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,0), lag.method="BIC", maxlag=12)
augment(fit.dt) %>%  
  pull(.resid) %>% 
  ts(start  = c(2002, 1), frequency = 12) %>%  
  uroot::hegy.test(deterministic = c(1,0,1), lag.method="BIC", maxlag=12)
augment(fit.dt) %>%  
  pull(.resid) %>% 
  urca::ur.df(type = c("none"), lags=4) %>% # matches the number of lags
  urca::summary()

######Don't Keep########Detrending the Series############################
fit.mave <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log RPM`=log(Southwest)) %>%
  mutate(`12-MA` = slider::slide_dbl(`Log RPM`, mean, .before = 5, .after = 6, .complete = TRUE), 
         `2x12-MA` = slider::slide_dbl(`12-MA`, mean, .before = 0, .after = 1, .complete = TRUE) )

# Plot the fitted regression
plot.fit <- fit.mave %>% 
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = `Log RPM`, colour = "Data"), linewidth=1.5) +
  geom_line(aes(y = `2x12-MA`, colour = "MA DT"), linewidth=1) +
  labs(title="Southwest RPM") + xlab("Month") +
  scale_color_manual(values=c(Data="gray23",`MA DT`="deepskyblue")) +
  guides(colour = guide_legend(title = NULL)) + 
  theme( legend.position = c(0.95, 0.05),  # x, y in [0,1] relative to plot area
         legend.justification = c("right", "bottom") )

# Plot fitted residuals
plot.resid <- fit.mave %>% 
  mutate(.resid = `Log RPM` - `2x12-MA`) %>%
  ggplot(aes(x=MONTH)) +
  geom_line(aes(y = .resid, colour = "Detrended"), linewidth=1) +
  labs(title="Log RPM") + xlab("Month") + ylab("Residualized Log RPM") +
  scale_color_manual(values=c(Detrended="deeppink4")) +
  guides(colour = guide_legend(title = NULL)) +
  theme( legend.position = c(0.95, 0.05),  # x, y in [0,1] relative to plot area
         legend.justification = c("right", "bottom") )

# Correlogram for residuals
plot1 <- fit.mave %>% 
  mutate(.resid = `Log RPM` - `2x12-MA`) %>%
  ACF(.resid, lag_max = 16) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,16,1)) +
  labs(title="ACF for detrended series") + xlab("Lag")
plot2 <- fit.mave %>% 
  mutate(.resid = `Log RPM` - `2x12-MA`) %>%
  PACF(.resid, lag_max = 16) %>%
  autoplot() +
  scale_x_continuous(breaks=seq(1,16,1)) +
  labs(title="PACF for detrended series") + xlab("Lag")

multifig <- plot.fit / plot.resid / (plot1 | plot2)
multifig
#Out of Sample Forecasts
######Don't Keep########Out of sample forecast#############

##12-step forecasting
#user chosen models below
sarima.1f <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(1,1,0) + PDQ(1,1,1),
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.1f)
sarima.2f <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,0) + PDQ(0,1,1),
              ic="aicc", stepwise=FALSE, approximation=FALSE))
report(sarima.2f)

#extending index by 12 (1 year)
DATA.out <- tsibble(
  MONTH = yearmonth(max(DATA$MONTH)) + 1:12,
  time = max(DATA$time) + 1:12   )
DATA.out <- DATA.out %>%
  mutate(`Log RPM` = NA)
DATA <- bind_rows(DATA, DATA.out)
DATA.out <- DATA %>%
  filter_index("2025 Feb" ~ .) 
P <- nrow(DATA.out)

#SARIMA.1
trained.model <- sarima.1f
sarima.1.forecast <- trained.model %>% forecast(h=1) 
DATA.out$MONTH
for (q in DATA.out$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(1,1,0) + PDQ(1,1,1), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.1.forecast <- bind_rows(sarima.1.forecast, append.forecast)
}
sarima.1.forecast <- sarima.1.forecast %>% mutate(.model="SARIMA.1")
kable(sarima.1.forecast)


#SARIMA.2
trained.model <- sarima.2f
sarima.2.forecast <- trained.model %>% forecast(h=1) 
for (q in DATA.out$time[-P]) {
  update.trained.model <- DATA %>%
    filter(time <= q) %>%
    model(ARIMA(`Log RPM` ~ 1 + pdq(2,1,0) + PDQ(0,1,1), fixed=coef(trained.model)$estimate))
  append.forecast <- update.trained.model %>% forecast(h=1)
  sarima.2.forecast <- bind_rows(sarima.2.forecast, append.forecast)
}
sarima.2.forecast <- sarima.2.forecast %>% mutate(.model="SARIMA.2")
kable(sarima.2.forecast)

all_forecasts <- bind_rows(
  sarima.1.forecast    %>% mutate(.model = "SARIMA(1,1,0)(1,1,1)"),
  sarima.2.forecast    %>% mutate(.model = "SARIMA(1,1,1)(1,1,1)")
)
all_forecasts %>%
  autoplot(filter_index(DATA, "2021 Jan" ~ "2025 Jan"), level=NULL) +
  autolayer(sarima.1.forecast, level = NULL, size = 1, color = "darkorchid1") +
  autolayer(sarima.2.forecast, level = NULL, size = 1, color = "chartreuse1") +
  autolayer(DATA.holdout, .vars=`Log RPM`, color = "black", linewidth=0.5, linetype="longdash") +
  labs(title = "Forecasts of Log RPM") +
  guides(colour = guide_legend(title = "Forecast"))

#Path forecasting and graphing
fit.path <- DATA %>%
  filter_index("2002 Oct" ~ "2025 Jan") %>%
  mutate(`Log RPM`= log(Southwest)) %>%
  model(
    "SARIMA(1,1,0)(1,1,1)"=ARIMA(`Log RPM` ~ 1 + pdq(1,1,1) + PDQ(1,1,1)),
    "SARIMA(1,1,1)(1,1,1)"=ARIMA(`Log RPM` ~ 1 + pdq(2,1,0) + PDQ(0,1,1))) 
forecast.path <- fit.path %>% 
  forecast(h=12)

forecast.path %>%
  autoplot(filter_index(DATA,"2021 Jan" ~ "2025 Jan"), level=NULL, linewidth=1) +
  labs(title = "Forecasts of Log RPM") +
  guides(colour = guide_legend(title = "Forecast Model"))


