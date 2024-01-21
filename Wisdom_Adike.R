# Task 1: Statistical Analysis

# Loading and displaying our dataset from working directory
pop_data = read.csv("bb0d7ebe-d54b-4bfa-b67a-b8317d46f804_Data.csv")

# Replacing the names of our columns
present_col_name = colnames(pop_data)

cols_for_rename = c(
  "Population.ages.0.14..total..SP.POP.0014.TO.",
  "Population.ages.15.64..total..SP.POP.1564.TO.",
  "Population.ages.65.and.above..total..SP.POP.65UP.TO.",
  "Population..total..SP.POP.TOTL.",
  "Population.growth..annual.....SP.POP.GROW.",
  "Population.density..people.per.sq..km.of.land.area...EN.POP.DNST.",
  "Rural.population..SP.RUR.TOTL.",
  "Urban.population....of.total...SP.URB.TOTL.IN.ZS."
)

new_col_name = c("Pop_ages014", "Pop_ages1564", "Pop_ages65",
                 "Pop_total", "Pop_annual", "Pop_density", 
                 "Pop_rural", "Pop_urban")
cols_for_rename_indexing = match(cols_for_rename, present_col_name)
colnames(pop_data)[cols_for_rename_indexing] = new_col_name


names(pop_data)

# The first six rows
head(pop_data)

# The last 6 rows
tail(pop_data)

# Viewing the data seperately
View(pop_data)

# Checking the data type
str(pop_data)

# Summarizing the data
summary(pop_data)

# Lets list the column names for easy identification
col_names <- names(pop_data)
print(col_names)

# Calculating the Mean, Median, Mode,Standard deviation, Skewness and Kurtosis for each indicators
# Installing and loading 'e1071' for skewness and kurtosis.
install.packages("e1071")
library(e1071)

# Creating an R function for the mode
fetch_modal = function(x) {
  unique_values = unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

# Statistical description for Population Total ages 0 - 14
mean(pop_data$Pop_ages014,na.rm = TRUE)
median(pop_data$Pop_ages014,na.rm = TRUE)
sd(pop_data$Pop_ages014,na.rm = TRUE)
var(pop_data$Pop_ages014,na.rm = TRUE)
length(pop_data$Pop_ages014)
range(pop_data$Pop_ages014,na.rm = TRUE)
skewness(pop_data$Pop_ages014, na.rm = TRUE)
kurtosis(pop_data$Pop_ages014,na.rm = TRUE)
fetch_modal(pop_data$Pop_ages014) # Calculating mode

# Statistical description for Population Total ages 15 - 64
mean(pop_data$Pop_ages1564,na.rm = TRUE)
median(pop_data$Pop_ages1564,na.rm = TRUE)
sd(pop_data$Pop_ages1564,na.rm = TRUE)
var(pop_data$Pop_ages1564,na.rm = TRUE)
length(pop_data$Pop_ages1564)
range(pop_data$Pop_ages1564,na.rm = TRUE)
skewness(pop_data$Pop_ages1564, na.rm = TRUE)
kurtosis(pop_data$Pop_ages1564,na.rm = TRUE)
fetch_modal(pop_data$Pop_ages1564) # Calculating mode

# Statistical description for Population Total ages 65 and above
mean(pop_data$Pop_ages65,na.rm = TRUE)
median(pop_data$Pop_ages65,na.rm = TRUE)
sd(pop_data$Pop_ages65,na.rm = TRUE)
var(pop_data$Pop_ages65,na.rm = TRUE)
length(pop_data$Pop_ages65)
range(pop_data$Pop_ages65,na.rm = TRUE)
skewness(pop_data$Pop_ages65, na.rm = TRUE)
kurtosis(pop_data$Pop_ages65,na.rm = TRUE)
fetch_modal(pop_data$Pop_ages65) # Calculating mode

# Statistical description for Population Total
mean(pop_data$Pop_total,na.rm = TRUE)
median(pop_data$Pop_total,na.rm = TRUE)
sd(pop_data$Pop_total,na.rm = TRUE)
var(pop_data$Pop_total,na.rm = TRUE)
length(pop_data$Pop_total)
range(pop_data$Pop_total,na.rm = TRUE)
skewness(pop_data$Pop_total, na.rm = TRUE)
kurtosis(pop_data$Pop_total,na.rm = TRUE)
fetch_modal(pop_data$Pop_total) # Calculating mode

# Statistical description for Population Growth Annual %
mean(pop_data$Pop_annual,na.rm = TRUE)
median(pop_data$Pop_annual,na.rm = TRUE)
sd(pop_data$Pop_annual,na.rm = TRUE)
var(pop_data$Pop_annual,na.rm = TRUE)
length(pop_data$Pop_annual)
range(pop_data$Pop_annual,na.rm = TRUE)
skewness(pop_data$Pop_annual, na.rm = TRUE)
kurtosis(pop_data$Pop_annual,na.rm = TRUE)
fetch_modal(pop_data$Pop_annual) # Calculating mode

# Statistical description for Population Density (People per sq Km of land area)
mean(pop_data$Pop_density,na.rm = TRUE)
median(pop_data$Pop_density,na.rm = TRUE)
sd(pop_data$Pop_density,na.rm = TRUE)
var(pop_data$Pop_density,na.rm = TRUE)
length(pop_data$Pop_density)
range(pop_data$Pop_density,na.rm = TRUE)
skewness(pop_data$Pop_density, na.rm = TRUE)
kurtosis(pop_data$Pop_density,na.rm = TRUE)
fetch_modal(pop_data$Pop_density) # Calculating mode

# Statistical description for Rural Population
mean(pop_data$Pop_rural, na.rm = TRUE)
median(pop_data$Pop_rural,na.rm = TRUE)
sd(pop_data$Pop_rural,na.rm = TRUE)
var(pop_data$Pop_rural,na.rm = TRUE)
length(pop_data$Pop_rural)
range(pop_data$Pop_rural,na.rm = TRUE)
skewness(pop_data$Pop_rural, na.rm = TRUE)
kurtosis(pop_data$Pop_rural,na.rm = TRUE)
fetch_modal(pop_data$Pop_rural) # Calculating mode

# Statistical description for Urban Population
mean(pop_data$Pop_urban,na.rm = TRUE)
median(pop_data$Pop_urban,na.rm = TRUE)
sd(pop_data$Pop_urban,na.rm = TRUE)
var(pop_data$Pop_urban,na.rm = TRUE)
length(pop_data$Pop_urban)
range(pop_data$Pop_urban,na.rm = TRUE)
skewness(pop_data$Pop_urban, na.rm = TRUE)
kurtosis(pop_data$Pop_urban,na.rm = TRUE)
fetch_modal(pop_data$Pop_urban) # Calculating mode

# Handling missing values
# Knowing the number of missing values in each column
missing_num = colSums(is.na(pop_data))
missing_num

# Installing and Loading the 'dplyr' and 'purrr' package
install.packages("dplyr")
library(dplyr)
library(purrr)

# Replacing the missing values with the mean of each numerical columns
pop_data = pop_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Confirming if the missing values has been handled
missing_num = colSums(is.na(pop_data))
missing_num

summary(pop_data)

# Interesting fact about the dataset
library(ggplot2)

# Population trends over time
pop_data$Time = as.numeric(pop_data$Time) # Converting time to numeric

ggplot(pop_data, aes(x = Time, y = Pop_total, color = Country.Name)) +
  geom_line() +
  labs(title = "Population Trends Over Time",
       x = "Year",
       y = "Total Population",
       color = "Country") +
  theme_minimal()

# Population density
ggplot(pop_data, aes(x = Country.Name, y = Pop_density, fill = Country.Name)) +
  geom_bar(stat = "identity") +
  labs(title = "Population Density Comparison",
       x = "Country",
       y = "Population Density",
       fill = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Detecting and removing outliers
# First of all, we will be creating an IQR function for removal
zscore_filter = function(col, threshold = 3) {
  zscore = scale(col)
  filtered_col = col
  filtered_col[abs(zscore) > threshold] = NA
  return(filtered_col)
}

# Now, we will be removing the outliers with the above function
pop_data_process = pop_data
numeric_col = names(pop_data_process)[sapply(pop_data_process, is.numeric)]
pop_data_process[numeric_col] <- lapply(pop_data_process[numeric_col], zscore_filter)

# Removing NA
pop_data_process = na.omit(pop_data_process)

# Dataset without outlier
summary(pop_data_process)

# Correlation Analysis
# Installing the packages for corr
install.packages("corrplot")
library(corrplot)

# Lets filter a country for our correlation analysis
france_pop_data =  filter(pop_data_process, Country.Name == "France")

plot(france_pop_data$Pop_ages014, france_pop_data$Pop_total)
plot(france_pop_data$Pop_density, france_pop_data$Pop_total)

cor(france_pop_data$Pop_ages014, france_pop_data$Pop_total)
cor(france_pop_data$Pop_density, france_pop_data$Pop_total)

continuous_cols_france = france_pop_data %>% select(Pop_ages014, Pop_ages1564, Pop_ages65, Pop_total, Pop_annual, Pop_density, Pop_rural, Pop_urban)
head(continuous_cols_france, 5)

round(cor(continuous_cols_france), digits = 2)
corrplot(cor(continuous_cols_france), method = "number", type = "upper")

# Lets do a correlation analysis for the entire data

plot(pop_data_process$Pop_ages014, pop_data_process$Pop_total)
plot(pop_data_process$Pop_density, pop_data_process$Pop_total)

cor(pop_data_process$Pop_ages014, pop_data_process$Pop_total)
cor(pop_data_process$Pop_density, pop_data_process$Pop_total)

plot(pop_data_process$Pop_ages1564, pop_data_process$Pop_total)
plot(pop_data_process$Pop_rural, pop_data_process$Pop_total)

cor(pop_data_process$Pop_ages1564, pop_data_process$Pop_total)
cor(pop_data_process$Pop_rural, pop_data_process$Pop_total)

plot(pop_data_process$Pop_ages65, pop_data_process$Pop_total)
plot(pop_data_process$Pop_urban, pop_data_process$Pop_total)

cor(pop_data_process$Pop_ages65, pop_data_process$Pop_total)
cor(pop_data_process$Pop_urban, pop_data_process$Pop_total)

continuous_cols = pop_data_process %>% select(Pop_ages014, Pop_ages1564, Pop_ages65, Pop_total, Pop_annual, Pop_density, Pop_rural, Pop_urban)
head(continuous_cols, 5)

round(cor(continuous_cols), digits = 2)
corrplot(cor(continuous_cols), method = "number", type = "upper")

# Hypothesis Testing
# Lets test for normality via visualization
# Installing the neccessary package
install.packages("tidyverse")
install.packages("ggplot2")

library(ggplot2)
library(tidyverse)

qqplot_func = function(col_name) {
  qq_plot = ggplot(pop_data_process, aes(sample = pop_data_process[[col_name]])) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(paste("Q-Q Plot ", col_name))
  
  print(qq_plot)
}

for (x in names(pop_data_process)) {
  qqplot_func(x)
}

# Testing for hypothesis
# First Hypothesis
# Null Hypothesis (H0): There is no significant difference in the mean population growth from age 0-14 in Europe and Asia compared to the mean total population.
# Alternative Hypothesis (H1): There is a significant difference in the mean population growth from age 0-14 in Europe and Asia compared to the mean total population.
# H0: μPop_ages014 = μPop_total
# H1: μPop_ages014 != μPop_total

wilcox.test(pop_data_process$Pop_ages014, pop_data_process$Pop_total)

# Second Hypothesis
# Null Hypothesis (H0): There is no significant difference in the standard deviation of rural population growth in Europe and Asia compared to the standard deviation of total population growth.
# Alternative Hypothesis (H1): There is a significant difference in the standard deviation of rural population growth compared to the standard deviation of total population growth.
# H0: σPop_rural = σPop_total
# H1: σPop_rural != σPop_total

fligner.test(pop_data_process$Pop_rural, pop_data_process$Pop_total)

# Third Hypothesis
# Null Hypothesis (H0): There is no significant difference in the mean population growth from age 15-64 in Europe and Asia compared to the mean total population.
# Alternative Hypothesis (H1): There is a significant difference in the mean population growth from age 15-64 in Europe and Asia compared to the mean total population.
# H0: μPop_ages1564 = μPop_total
# H1: μPop_ages1564 != μPop_total

wilcox.test(pop_data_process$Pop_ages1564, pop_data_process$Pop_total)

# Regression Analysis for the entire dataset
# First of all, lets extract the numerical variable from the entire dataset

pop_data_reduced = pop_data_process[ ,c("Pop_ages014",
                                        "Pop_ages1564",
                                        "Pop_ages65",
                                        "Pop_total",
                                        "Pop_annual",
                                        "Pop_density",
                                        "Pop_rural",
                                        "Pop_urban")]
cor(pop_data_reduced)
corrplot(cor(pop_data_reduced))

# SLR of Population growth from 15-64 to the total population
# X = Pop_ages1564
# Y = Pop_total

first_model = lm(Pop_total ~ Pop_ages1564, pop_data_reduced)
summary.lm(first_model)

# Fitting the regression line on the scatter plot
plot(Pop_total ~ Pop_ages1564,
     pop_data_reduced,
     col = "blue",
     main = "Total Population & Population ages from 15-64",
     xlab = "Total Population", ylab = "Population ages from 15-64")

abline(first_model, col="black")

plot(first_model, 1)
plot(first_model, 2)
plot(first_model, 3)

# MLR of the highly correlated variables to the total population
# Installing the neccessary package
install.packages("car")
library(car)

# X1 = Pop_ages1564
# X2 = Pop_ages014
# X3 = Pop_rural
# X4 = Pop_ages65
# Y = Pop_total

second_model = lm(Pop_total ~ Pop_ages1564 + Pop_ages014 + Pop_rural + Pop_ages65 , pop_data_reduced)
summary.lm(second_model)

third_model = lm(Pop_total ~ Pop_ages1564 + Pop_ages014 + Pop_rural, pop_data_reduced)
summary.lm(third_model)

fourth_model = lm(Pop_total ~ Pop_ages1564 + Pop_ages014, pop_data_reduced)
summary.lm(fourth_model)

colnames(pop_data_reduced)
pairs(pop_data_reduced[,c(4,2,1,7,3)], lower.panel = NULL, pch = 19,cex = 0.2)

vif(fourth_model)

# Regression Analysis for France

france_data_reduced = france_pop_data[ ,c("Pop_ages014",
                                        "Pop_ages1564",
                                        "Pop_ages65",
                                        "Pop_total",
                                        "Pop_annual",
                                        "Pop_density",
                                        "Pop_rural",
                                        "Pop_urban")]
cor(france_data_reduced)
corrplot(cor(france_data_reduced))

# Pop_density, Pop_urban, Pop_ages014, Pop_ages65 are the most correlated to Total Population

# SLR of Population density to the total population in France
# X = Pop_density
# Y = Pop_total

first_model = lm(Pop_total ~ Pop_density, france_data_reduced)
summary.lm(first_model)

# Fitting the regression line on the scatter plot
plot(Pop_total ~ Pop_density,
     pop_data_reduced,
     col = "blue",
     main = "Total Population & Population Density",
     xlab = "Total Population", ylab = "Population Density")

abline(first_model, col="black")

plot(first_model, 1)
plot(first_model, 2)
plot(first_model, 3)

# MLR of the highly correlated variables to the total population
# Installing the neccessary package
install.packages("car")
library(car)

# X1 = Pop_density
# X2 = Pop_urban
# X3 = Pop_ages014
# X4 = Pop_ages65
# Y = Pop_total

second_model_france = lm(Pop_total ~ Pop_density + Pop_urban + Pop_ages014 + Pop_ages65 , france_data_reduced)
summary.lm(second_model_france)

third_model_france = lm(Pop_total ~ Pop_density + Pop_urban + Pop_ages014, france_data_reduced)
summary.lm(third_model_france)

fourth_model_france = lm(Pop_total ~ Pop_ages65 + Pop_ages014, france_data_reduced)
summary.lm(fourth_model_france)

colnames(france_data_reduced)
pairs(france_data_reduced[,c(4,6,8,1,3)], lower.panel = NULL, pch = 19,cex = 0.2)

vif(fourth_model_france)


# Time Series Analysis
# Installing the packages
install.packages("TTR")
install.packages("forecast")

library(TTR)
library(forecast)

View(pop_data_process)
uk_data_frame =  filter(pop_data_process, Country.Name == "United Kingdom")


ts_uk_col = uk_data_frame$Pop_annual
ts_uk_col

# Confirming the frequency spread of the time series in UNIT 
ts_uk_data = ts(ts_uk_col, frequency = 1, start = c(2002, 1))
ts_uk_data

#Lets plot our Time Series
plot.ts(ts_uk_data)

# Decomposing
# plot.ts(decompose(ts_uk_data))

# Smoothing
ts_uk_data_sma3 = SMA(ts_uk_data, n=3)
plot.ts(ts_uk_data_sma3)

# My time series has trend but no seasonality
ts_uk_data_forecasts = HoltWinters(ts_uk_data, gamma=FALSE)
ts_uk_data_forecasts

# Sum of squared error
ts_uk_data_forecasts$SSE

plot(ts_uk_data_forecasts)

HoltWinters(ts_uk_data, gamma=FALSE, l.start=0.4233371, b.start=0.042304)

ts_uk_data_forecasts2 = forecast(ts_uk_data_forecasts, h = 8)
plot(ts_uk_data_forecasts2)
ts_uk_data_forecasts2

# Correlation between the lags
acf(ts_uk_data_forecasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(na.omit(ts_uk_data_forecasts2$residuals), lag=10, type="Ljung-Box")

plotForecastErrors = function(forecasterrors) {
  # make a histogram of the forecast errors: 
  mybinsize <- IQR(forecasterrors)/4
  mysd <- sd(forecasterrors) 
  mymin <- min(forecasterrors) - mysd*5 
  mymax <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd 
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 } 
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd 
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2) }

plot.ts(ts_uk_data_forecasts2$residuals) # make time series plot

plotForecastErrors(na.omit(ts_uk_data_forecasts2$residuals)) # make a histogram

# Arima
plot(ts_uk_data)
ts_uk_data_seriesdiff1 = diff(ts_uk_data, differences=1)
plot.ts(ts_uk_data_seriesdiff1)

ts_uk_data_seriesdiff2 = diff(ts_uk_data, differences=2)
plot.ts(ts_uk_data_seriesdiff2)

# Plotting to know the p and q
acf(ts_uk_data_seriesdiff1, lag.max=20)
acf(ts_uk_data_seriesdiff1, lag.max=20, plot=FALSE)

pacf(ts_uk_data_seriesdiff1, lag.max=20)
pacf(ts_uk_data_seriesdiff1, lag.max=20, plot=FALSE)

uk_arima = arima(ts_uk_data, order=c(0,1,0))
uk_arima

uk_ar_forecasts_ = forecast(uk_arima, h=8)
plot(uk_ar_forecasts_)
uk_ar_forecasts_

acf(uk_ar_forecasts_$residuals, lag.max=20)
Box.test(uk_ar_forecasts_$residuals, lag=10, type="Ljung-Box")

plot.ts(uk_ar_forecasts_$residuals) # time plot forecast error 
plotForecastErrors(uk_ar_forecasts_$residuals)

