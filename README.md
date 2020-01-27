# Seminar
Forecasting GDP
#install.packages("devtools")
#install.packages("drat")
#install.packages("xlsx")
#drat::addRepo("timemod")
#install.packages("regts")
library(xlsx)
library(devtools)
library(regts)
setwd("C:/Users/carol/Documents/Key docs/Master Econometrics/Seminar/codes")
#devtools::install_local("bvarcpb/BVARCPB_0.0.4.tar.gz")

rm(list = ls())

library(BVARCPB)
library(regts)
library(xlsx)

# Set working directory
# change object folder to folder where the data is
folder <- 'C:/Users/carol/Documents/Key docs/Master Econometrics/Seminar/BBP'
xls_input_file <-  file.path(folder, "dec20_pad01_bvar.xls")
xls_output_file <- file.path(folder, "bvar_dec2020_14nov.xlsx")

# first and last period for realisations in output-file
# forecast for coming and next year
first_last_per <- "2017q1/2019q3"
first_per_forecast <- "2019q4"
last_per_forecast <- "2020q4"

###########################################################################################
#                                   Data preparation                                      #
###########################################################################################

# Import data for BVAR (only from 2001q1 onwards)
data_raw <- read_ts_xlsx(xls_input_file, na_string = "NA")
data <- data_raw["2001q1/",]
end <- nrow(data)

# Transform data (logarithms)
data[,c(1:9,12,17,23,27:29)] = log(data[,c(1:9,12,17,23,27:29)]);

# Swap output data to the first place
data <- data[,c(2,1,3:29)]

# Variable selection (new model contains 22 variables)
data <- data[,-c(15,18,19,24,26,27,28,29)]

# Create variable for GDP growth
real_gdp_growth <- regts(c(NA,data[2:end,"YBMAN___"]-data[1:end-1,"YBMAN___"]),start="2001q1")


###########################################################################################
#                                       Settings                                          #
###########################################################################################

# Hyperparameters for prior
alpha = 1
lambda = 0.2
rho = 1
mu = 1
delta = 1

# Row with last observations (end of the dataset)
end <- nrow(data)

# Forecast horizon
h = 12

# Start date for evaluation
start <- 20


###########################################################################################
#                                   BVAR Estimation                                       #
###########################################################################################

# Estimating a BVAR with Combination prior (Minnesota, Sum-of-coefficients and Dummy-initial-observations prior)
bvar_c_4 <- BVARC_CPB(data=data, p=4, alpha=alpha,lambda=lambda,mu=mu,delta=delta)

###########################################################################################
#                                    BVAR Forecasts                                       #
###########################################################################################

bvar_forecast_c_4 <- predBVAR(bvar_c_4, h = h)

# Calculating the implied growth forecasts
bvar_forecast_growth_c_4 <- diff(rbind(data[end,],bvar_forecast_c_4))

# Write results of preferred specification (combination prior with 4 lags)
variables <- c("YBMAN___","C__AN___", "I__AN___", "BGDAN___", "MGDAN___", "CPIPI___", "LL_PI___")
realisations <- t(as.data.frame(diff(data[,variables])[first_last_per,]))
forecasts <- t(as.data.frame(regts(bvar_forecast_growth_c_4[,variables],start=first_per_forecast,end=last_per_forecast)))
output <- as.data.frame(100*cbind(realisations,forecasts))
write.xlsx(output, file=xls_output_file, sheetName="bvar_growth")

