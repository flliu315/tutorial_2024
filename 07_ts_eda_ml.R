# --------------------------------------------
# Script Name: Timeseries EDA and ML
# Purpose: This script is to show how conduct EDA and ML of
#                time series. The purpose is to allow students to
#                be familiar with the main packages, such as timetk,
#               tidymodels, modeltime, etc. The key is to extract
#               the features from timestamps.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-04-6
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 01-create timestamp and timeseries objects
# https://rpubs.com/ravipmum/Time_Series_Fundamentals

# A) create timestamp

# Convert string to date object

library(lubridate) # manipulating dates

date = ymd("2017-01-31")
print(date)

# create from individual components
library(tidyverse)
data(flights, package="nycflights13")
head(flights)
flights %>% 
  select(year, month, day, hour, minute) %>% 
  mutate(dep_date_time = make_datetime(year, month, day, hour, minute)) %>% head()

# switch between date and date-time

as_date(now()) # as date

##-------------------------------------------------------
# B) create timeseries

# use ts() to create time series
# devtools::install_github("PascalIrz/aspe")
# library(aspe)

data=read.table('data/SPdata/fishBiomassData.txt',h=TRUE)
head(data)
data_clean <- data |>
  dplyr::select(-YEAR) |>
  drop_na() |>
  distinct()

unique(data_clean$STATION) # check stations
table(data_clean$STATION)
unique(data_clean$SP) # check stations
table(data_clean$SP)
mydata <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")

data_ts = ts(data = mydata[, -c(1:5)], # All columns excluding 3rd column
          start = c(1994), # Start Year 1994
          frequency = 1)  # freq = 1

# Plot data with faceting
library(forecast) # work with ggplot2 for autoplot()
library(ggplot2)
autoplot(data_ts, facets = TRUE) +
  ggtitle("CHE of Doubs river") +
  ylab("Changes") + xlab("Year")

# use timetk() to create timeseries
library(timetk)
mydata <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")
datatk_ts <- mydata |>
  tk_tbl() |> # Convert to tibble
  # mutate(DATE = as_date(as.POSIXct.Date(DATE))) |>
  select(-1) |>
  rename(date = DATE) |>
  relocate(date, .before = STATION) |>
  pivot_longer( # convert to to long format
    cols = c("BIOMASS","DENSITY"))

# plot with timetk

datatk_ts |>
  group_by(name) |>
  plot_time_series(date, value, 
                   .facet_ncol = 2, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "CHE of Le Doubs river"
                   )

#################################################
# 02-EDA of timeseries objects

# A) Find the outlier in timeseries
goldoutlier <- which.max(data_ts)
goldoutlier

##---------------------------------------------------
# B) represent/reduce dimensions

library(TSrepr)

mydata <- data_clean |>
  filter(STATION=="VOLPla" & SP == "CHE")

biom_ts <- ts(mydata[,-c(1:5,7)],
              start= c(1994,1),
              frequency =1)

p1 <- autoplot(biom_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")

data_dwt <- repr_dwt(mydata$BIOMASS, level = 1) 
data_dwt_ts <- ts(data_dwt,
              start= c(1994,1),
              frequency =1)

p2 <- autoplot(data_dwt_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")

library(patchwork)
p1+p2

# C) serial autocorrelation or ACF
# for ts objects
ggAcf(data_ts)

mydata <- data_clean |>
  filter(STATION=="VOLPla" & SP == "CHE")

biom_ts <- ts(mydata[,-c(1:5,7)],
              start= c(1994,1),
              frequency =1)
autoplot(biom_ts) +
  ggtitle("CHE biomass of Doubs river") +
  ylab("Changes") + xlab("Year")

ggAcf(biom_ts)
Box.test(biom_ts, lag =15, fitdf = 0, type ="Ljung")

# for tk objects
# https://www.r-bloggers.com/2020/06/time-series-in-5-minutes-part-2-autocorrelation-and-cross-correlation/

library(timetk)
mydata <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")
datatk_ts <- mydata |>
  tk_tbl() |> # Convert to tibble
  # mutate(DATE = as_date(as.POSIXct.Date(DATE))) |>
  # select(-1) |>
  rename(date = DATE) |>
  relocate(date, .before = STATION) |>
  pivot_longer( # convert to to long format
    cols = c("BIOMASS","DENSITY"))

datatk_ts |>
  group_by(name) |>
  plot_acf_diagnostics(
    date, value,               # ACF & PACF
    .lags = "5 years",    
    .interactive = FALSE
  )

#######################################################
# 03- generating new features

library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(lubridate)

mydata <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")

biomtk_ts <- mydata |>
  tk_tbl() |> # Convert to tibble
  # mutate(DATE = as_date(as.POSIXct.Date(DATE))) |>
  select(DATE, BIOMASS)
  # rename(date = DATE) |>
  # relocate(date, .before = STATION) |>
  # pivot_longer( # convert to to long format
  #   cols = c("BIOMASS"))

biomtk_ts |>
  plot_time_series(DATE, BIOMASS,
                   .facet_ncol  = NULL,
                   .smooth      = FALSE, 
                   .interactive = TRUE,
                   .title = "Biomass timeseries")

# Check the regularity of the time series
biomtk_ts |>
  tk_summary_diagnostics(.date_var = DATE)

##-----------------------------------------------
# A) Calendar-based features

biomtk_ts_features_C <- biomtk_ts |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # Measure Transformation: standardization
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
# Add Calendar-based (or signature) features
tk_augment_timeseries_signature(.date_var = DATE) |>
  glimpse()

biomtk_ts_features_C

# Perform linear regression    
timetk::plot_time_series_regression(.date_var = DATE,
                            .data = biomtk_ts_features_C,
                            .formula = BIOMASS ~ as.numeric(DATE) + index.num
                            + year + half + quarter + month + month.lbl,
                            .show_summary = TRUE)

##----------------------------------------------------
# B) Fourier terms features

biomtk_ts_features_F <- biomtk_ts |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # Measure Transformation: standardization
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # Add Fourier features
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) 

biomtk_ts_features_F

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_F,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              DATE_sin5_K1 + DATE_cos5_K1,
                            .show_summary = TRUE)

##------------------------------------------------------------
## C) Lag features

biomtk_ts_features_L <- biomtk_ts |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # Measure Transformation: standardization
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # Add lag features
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7))  

biomtk_ts_features_L 

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_L,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_lag4 + BIOMASS_lag7,
                            .show_summary = TRUE)

##-----------------------------------------------------------
## D) Moving window statistics

biomtk_ts_features_M <- biomtk_ts |>
  # Measure Transformation: variance reduction with Log(x+1)
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  # Measure Transformation: standardization
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  tk_augment_lags(.value = BIOMASS, .lags = c(4, 7)) |>
  # Add moving window statistics
  tk_augment_slidify(.value   = contains("BIOMASS"),
                   .f       = ~ mean(.x, na.rm = TRUE), 
                   .period  = c(3, 6),
                   .partial = TRUE,
                   .align   = "center")

biomtk_ts_features_M 

# Perform linear regression
plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_M,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                            .show_summary = TRUE)


##-------------------------------------------------------------
## E) put all features together 

biomtk_ts_features_all <- biomtk_ts |>
  mutate(BIOMASS =  log1p(x = BIOMASS)) |>
  mutate(BIOMASS =  standardize_vec(BIOMASS)) |>
  # Add Calendar-based (or signature) features
  tk_augment_timeseries_signature(.date_var = DATE) |>
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  # dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  # Add Fourier features
  tk_augment_fourier(.date_var = DATE, .periods = 5, .K=1) |>
  # Add lag features
  tk_augment_lags(.value = BIOMASS, .lags = c(4,7)) |>
  # Add moving window statistics
 tk_augment_slidify(.value   = contains("BIOMASS"),
                   .f       = ~ mean(.x, na.rm = TRUE), 
                   .period  = c(3, 6),
                   .partial = TRUE,
                   .align   = "center")

biomtk_ts_features_all |>
  glimpse()

plot_time_series_regression(.date_var = DATE, 
                            .data = biomtk_ts_features_all,
                            .formula = BIOMASS ~ as.numeric(DATE) + 
                              index.num + year + half + quarter + month + 
                              DATE_sin5_K1 + DATE_sin5_K1 + 
                              # BIOMASS_lag4 + BIOMASS_lag7 + 
                              BIOMASS_roll_3 + BIOMASS_roll_6,
                              # BIOMASS_lag4_roll_3 + BIOMASS_lag7_roll_3 + 
                              # BIOMASS_lag4_roll_6 + BIOMASS_lag7_roll_6,
                            .show_summary = TRUE)

#######################################################
# 04- machine learning for time series 
# https://www.r-bloggers.com/2022/01/time-series-forecasting-lab-part-3-machine-learning-with-workflows/
# A) load packages and data

library(tidyverse)  
library(timetk) 
library(tidymodels)
library(modeltime)
library(timetk)

mydata <- data_clean |>
  subset(STATION=="VOLPla" & SP == "CHE")

biomtk_ts <- mydata |> # Convert to tibble
  tk_tbl() |> 
  select(index, DATE, BIOMASS) # keep date and target

# biomtk_ts |>
#   plot_time_series(DATE, BIOMASS,
#                    .facet_ncol  = NULL,
#                    .smooth      = FALSE,
#                    .interactive = TRUE,
#                    .title = "Biomass timeseries")

library(tidyquant)
ggplot(biomtk_ts, aes(x = DATE, y = BIOMASS)) +
  geom_line() +
  ggtitle("Biomass of Fishes in Doubs")

##---------------------------------------------------
# B) Train/Test Splitting and creating features

# splits <- biomtk_ts |>
#   time_series_split(DATE,
#                     assess = "3 year", 
#                     cumulative = TRUE)
# 
# splits

n_rows <- nrow(biomtk_ts)
train_rows <- round(0.8 * n_rows)

train_data <- biomtk_ts |>
  slice(1:train_rows) # slice() from dplyr
test_data <- biomtk_ts |>
  slice((train_rows):n_rows)

ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Training"), 
            linewidth = 1) +
  geom_line(data = test_data, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  scale_color_manual(values = c("Training" = "blue", 
                                "Test" = "red")) +
  labs(title = "Training and Test Sets", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()

# creating features with recipes

library(recipes)
library(tidymodels)

recipe_spec_final <- recipe(BIOMASS ~ ., train_data) |>
  step_mutate_at(index, fn = ~if_else(is.na(.), -12345, . )) |>
  step_timeseries_signature(DATE) |>
  step_rm(DATE) |>
  step_zv(all_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

summary(prep(recipe_spec_final))

##---------------------------------------------------
## C) training and evluating models
# 1) Training a boosted tree model

# Workflow
bt <- workflow() |>
  add_model(
    boost_tree("regression") |> set_engine("xgboost")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

bt

# evaluating model performance

bt_test <- bt |> 
  predict(test_data) |>
  bind_cols(test_data) 

bt_test

pbt <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = bt_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "bt-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()


# Calculating forecast error
bt_test |>
  metrics(BIOMASS, .pred)

## 2) training a random forest model

rf <- workflow() |>
  add_model(
    spec = rand_forest("regression") |> set_engine("ranger")
  ) |>
  add_recipe(recipe_spec_final) |>
  fit(train_data)

rf

# evaluating model performance

rf_test <- rf |> 
  predict(test_data) |>
  bind_cols(test_data) 

rf_test

prf <- ggplot() +
  geom_line(data = train_data, 
            aes(x = DATE, y = BIOMASS, color = "Train"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = BIOMASS, color = "Test"), 
            linewidth = 1) +
  geom_line(data = rf_test, 
            aes(x = DATE, y = .pred, color = "Test_pred"), 
            linewidth = 1) +
  scale_color_manual(values = c("Train" = "blue", 
                                "Test" = "red",
                                "Test_pred" ="black")) +
  labs(title = "rf-Train/Test and validation", 
       x = "DATE", y = "BIOMASS") +
  theme_minimal()


# Calculating forecast error
rf_test |>
  metrics(BIOMASS, .pred)

library(patchwork)
pbt + prf

##-------------------------------------------------------
# D) comparing among different algorithms

# create a Modeltime Table

model_tbl <- modeltime_table(
  bt,
  rf
)

model_tbl

# Calibration table

calibrated_tbl <- model_tbl |>
  modeltime_calibrate(new_data = test_data)

calibrated_tbl 

# Model Evaluation

calibrated_tbl |>
  modeltime_accuracy(test_data) |>
  arrange(rmse)

##----------------------------------------------------------
# Forecast Plot

calibrated_tbl |>
  modeltime_forecast(
    new_data    = test_data,
    actual_data = biomtk_ts,
    keep_data   = TRUE 
  ) |>
  plot_modeltime_forecast(
    .facet_ncol         = 2, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )

##--------------------------------------------------------
# E) save the work

workflow_Doubs <- list(
  
  workflows = list(
    
    wflw_random_forest = rf,
    wflw_xgboost = bt
    
  ),
  
  calibration = list(calibration_tbl = calibrated_tbl)
  
)

workflow_Doubs |>
  write_rds("data/TSdata/workflows_Doubs_list.rds")


