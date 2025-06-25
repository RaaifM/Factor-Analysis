# Asset Risk Management Project

# -----------------------------------------------
# Market Equity Factor Data
# -----------------------------------------------

# -----------------------------------------------
# Step 1: Preparing and Understanding the Market Equity Factor Data
# -----------------------------------------------

# We begin our analysis with the Market Equity factor data obtained from the Global Factor Dataset.

# According to the dataset documentation:
# - The column "ret" refers to the monthly total return of the factor portfolio.
# - The column "direction" indicates the sorting direction:
#     - "1" corresponds to a long portfolio (e.g., Small minus Big).
#     - "-1" corresponds to a short portfolio (e.g., Big minus Small).

# For context, the seminal paper by Banz (1981) examined the size effect using stock market data from the United States,
# which is classified as a developed market.

# To broaden the scope of our analysis beyond a single developed country, we include Austria as a second developed market.

# To further enhance the robustness and generalizability of our findings, we also include data from India,
# which is considered an emerging market.

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("lubridate", quietly = TRUE)) install.packages("lubridate")
if (!requireNamespace("zoo", quietly = TRUE)) install.packages("zoo")
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("xts", quietly = TRUE)) install.packages("xts")

library(dplyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(quantmod)
library(xts)

# Reading the 7_data.RData

load("factor_analysis_data.RData")

# Filtering for countries: "USA", "AUT", "IND" 
# Reading the date columns as Date

me_usa <- me_raw %>%
  filter(location == "usa") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)

me_aut <- me_raw %>%
  filter(location == "aut") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret))%>%
  arrange(date)

me_ind <- me_raw %>%
  filter(location == "ind") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)

mean_ret_usa <- mean(me_usa$ret, na.rm = TRUE)
std_dev_usa <- sd(me_usa$ret, na.rm = TRUE)
sharpe_ratio_usa <- mean_ret_usa / std_dev_usa  # assuming zero risk-free rate

mean_ret_aut <- mean(me_aut$ret, na.rm = TRUE)
std_dev_aut <- sd(me_aut$ret, na.rm = TRUE)
sharpe_ratio_aut <- mean_ret_aut / std_dev_aut  # assuming zero risk-free rate

mean_ret_ind <- mean(me_ind$ret, na.rm = TRUE)
std_dev_ind <- sd(me_ind$ret, na.rm = TRUE)
sharpe_ratio_ind <- mean_ret_ind / std_dev_ind  # assuming zero risk-free rate

# Display summary table
summary_table <- data.frame(
  Metric = c("Mean Return", "Standard Deviation", "Sharpe Ratio"),
  USA = c(mean_ret_usa, std_dev_usa, sharpe_ratio_usa),
  AUT = c(mean_ret_aut, std_dev_aut, sharpe_ratio_aut),
  IND = c(mean_ret_ind, std_dev_ind, sharpe_ratio_ind)
)

# Print the table
print(summary_table)

# Cumulative Return Plot
ggplot(me_usa, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - United States (Market Equity Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(me_aut, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - Austria (Market Equity Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(me_ind, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - India (Market Equity Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

# Rolling Window Analysis 

# We decided to choose a rolling window of 12 months since this is reflective of the 
# annual investment horizon. Furthermore, the one year period captures short term economic 
# fluctuations.

# Rolling window length (in months)
window_length <- 12

# Create rolling mean, SD, and Sharpe ratio - usa
me_usa <- me_usa %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - aut
me_aut <- me_aut %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - ind
me_ind <- me_ind %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

ggplot(me_usa, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Market Equity Factor (United States)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(me_aut, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Market Equity Factor (Austria)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(me_ind, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Market Equity Factor (India)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

# Now we can analyze the plot to see any possible trends 
# To analyze trend we look at the maximum, minimum, and average of the Sharpe ratio's
rolling_sharpe_mean_usa <- mean(me_usa$roll_sharpe, na.rm = TRUE)
rolling_max_usa <- max(me_usa$roll_sharpe, na.rm = TRUE)
max_sharpe_date_usa <- me_usa$date[which.max(me_usa$roll_sharpe)]
rolling_min_usa <- min(me_usa$roll_sharpe, na.rm = TRUE)
min_sharpe_date_usa <- me_usa$date[which.min(me_usa$roll_sharpe)]

# The lowest rolling Sharpe ratio for the Market Equity factor occurred in 1973 at 
# -1.44, during the oil crisis and ensuing stagflation, when small-cap stocks were 
# disproportionately affected by economic uncertainty and market stress. Conversely, 
# the highest Sharpe ratio was observed in 1978 of 1.90, a period marked by economic 
# recovery and a notable rebound in small-cap stock performance. These findings align 
# with the notion that the size premium is time-varying, performing poorly in downturns 
# but rebounding strongly in recoveries — suggesting that its risk-adjusted performance is highly cyclical.

rolling_sharpe_mean_aut <- mean(me_aut$roll_sharpe, na.rm = TRUE)
rolling_max_aut <- max(me_aut$roll_sharpe, na.rm = TRUE)
max_sharpe_date_aut <- me_aut$date[which.max(me_aut$roll_sharpe)]
rolling_min_aut <- min(me_aut$roll_sharpe, na.rm = TRUE)
min_sharpe_date_aut <- me_aut$date[which.min(me_aut$roll_sharpe)]

# In Austria, the Market Equity factor’s Sharpe ratio peaked in 2014, likely due to
# strong performance of small-cap stocks in the aftermath of the Eurozone debt crisis
# and amid favorable monetary policy from the ECB. By contrast, the minimum Sharpe ratio
# occurred in 2017, possibly reflecting rising global uncertainty and a slowdown in 
# small-cap out performance. This highlights the time-varying nature of the size effect,
# particularly in smaller, open economies like Austria that are sensitive to broader 
# regional and global dynamics.

rolling_sharpe_mean_ind <- mean(me_ind$roll_sharpe, na.rm = TRUE)
rolling_max_ind <- max(me_ind$roll_sharpe, na.rm = TRUE)
max_sharpe_date_ind <- me_ind$date[which.max(me_ind$roll_sharpe)]
rolling_min_ind <- min(me_ind$roll_sharpe, na.rm = TRUE)
min_sharpe_date_ind <- me_ind$date[which.min(me_ind$roll_sharpe)]

# In India, the maximum Sharpe ratio for the Market Equity factor occurred in 1989,
# during a period of strong investor sentiment and speculative equity market growth. 
# Small-cap stocks experienced significant appreciation with relatively low volatility,
# resulting in a high risk-adjusted return for the size factor.

# In contrast, the minimum occurred in 1996, a year marked by political instability,
# reduced investor confidence, and market correction. These conditions disproportionately
# affected small-cap firms, leading to poor or volatile returns. This highlights the size
# factor’s sensitivity to political and regulatory shifts in emerging markets, where market
# dynamics can change rapidly and affect the risk-return profile of smaller firms.

# Display summary table rolling Sharpe
summary_table_rs <- data.frame(
  Country = c("USA", "AUT", "IND"),
  Mean_Sharpe = c(rolling_sharpe_mean_usa, rolling_sharpe_mean_aut, rolling_sharpe_mean_ind),
  Max_Sharpe = c(rolling_max_usa, rolling_max_aut, rolling_max_ind),
  Min_Sharpe = c(rolling_min_usa, rolling_min_aut, rolling_min_ind)
)

print(summary_table_rs)

# Now we assess the performance of market equity factor before and after the
# publication of Banz (1981)

# Before/after Banz indicator cutoff = Mar 1981
cutoff_date <- as.Date("1981-03-01")

# usa 
me_usa <- me_usa %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

# Group by Before/after Banz and summarize mean, sd, Sharpe
performance_summary_usa <- me_usa %>%
  mutate(period = factor(period, levels = c("Before Banz","After Banz"))) %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# View result
print(performance_summary_usa)

# aut 
# Before/after Banz indicator cutoff = Mar 1981
me_aut <- me_aut %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

# Group by Before/after Banz and summarize mean, sd, Sharpe
performance_summary_aut <- me_aut %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# ind 
# Before/after Banz indicator cutoff = Mar 1981
me_ind <- me_ind %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

# Group by Before/after Banz and summarize mean, sd, Sharpe
performance_summary_ind <- me_ind %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# Add country labels
performance_summary_usa$Country <- "USA"
performance_summary_aut$Country <- "AUT"
performance_summary_ind$Country <- "IND"

# Combine into one data frame
combined_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Reorder columns for clarity
combined_summary <- combined_summary %>%
  select(Country, period, Mean_Return, Std_Dev, Sharpe_Ratio)

# Optional: Round for presentation
combined_summary <- combined_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View result
print(combined_summary)

# Creating a cumulative return plot - using period wise grouping 

me_usa <- me_usa %>%
  arrange(date) %>%
  group_by(period) %>%
  mutate(period_cum_return = cumprod(1 + ret))

ggplot(me_usa, aes(x = date, y = period_cum_return, color = period)) +
  geom_line() +
  labs(
    title = "Cumulative Return of Market Equity Factor (USA)",
    x = "Date",
    y = "Cumulative Return",
    color = "Period"
  ) +
  theme_minimal()

# Check word for interpretation 

# Get S&P 500 (USA) index data (ticker: ^GSPC) (only data from 1927-12-30 available)
getSymbols("^GSPC", src = "yahoo", from = "1926-01-01",to = "2024-12-31", get = "stock.prices")
sp500_monthly_ret <- monthlyReturn(GSPC[, "GSPC.Adjusted"], name = "sp500_ret")
sp500_df <- data.frame(
  date = index(sp500_monthly_ret),
  ret = coredata(sp500_monthly_ret)
)

cutoff_date <- as.Date("1981-03-01")
sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

performance_summary_sp500 <- sp500_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_sp500)

# ATX (Austria) — ticker symbol: ^ATX (only data from 1999-09-30 available)
getSymbols("^ATX", src = "yahoo", from = "1986-09-30", to = "2024-12-31", get = "stock.prices")

atx_monthly_ret <- monthlyReturn(ATX[, "ATX.Adjusted"], name = "atx_ret")
atx_df <- data.frame(
  date = index(atx_monthly_ret),
  ret = coredata(atx_monthly_ret)
)

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

performance_summary_atx <- atx_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_atx)


# Nifty 50 (India) — ticker symbol: ^NSEI (only data from 2007-09-17 available )
getSymbols("^NSEI", src = "yahoo", from = "1988-09-30", to = "2024-12-31", get = "stock.prices")

nsei_monthly_ret <- monthlyReturn(NSEI[, "NSEI.Adjusted"], name = "nsei_ret")
nsei_df <- data.frame(
  date = index(nsei_monthly_ret),
  ret = coredata(nsei_monthly_ret)
)

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

performance_summary_nsei <- nsei_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_nsei)

# Full summary including the benchmark 

# Add country labels to benchmark summaries
performance_summary_sp500$Country <- "USA"
performance_summary_atx$Country <- "AUT"
performance_summary_nsei$Country <- "IND"

# Rename benchmark columns
performance_summary_sp500 <- performance_summary_sp500 %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_atx <- performance_summary_atx %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_nsei <- performance_summary_nsei %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

# Combine factor summaries
factor_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Combine benchmark summaries
benchmark_summary <- bind_rows(
  performance_summary_sp500,
  performance_summary_atx,
  performance_summary_nsei
)

# Join on Country and period
full_summary <- factor_summary %>%
  left_join(benchmark_summary, by = c("Country", "period"))

# Reorder and round
full_summary <- full_summary %>%
  select(Country, period,
         Mean_Return, Std_Dev, Sharpe_Ratio,
         BM_Mean_Return, BM_Std_Dev, BM_Sharpe_Ratio) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View final table
print(full_summary)

# Interpretation in the word document 

# Now we compare with an alternative asset class - we choose gold 

gold_raw <- gold_raw %>%
  mutate(date = mdy(Date)) %>%
  arrange(date)

# Convert to xts object
gold_xts <- xts(gold_raw$Last.Price, order.by = gold_raw$date)

# Now compute monthly returns
gold_monthly <- monthlyReturn(gold_xts, name = "Gold_Return")

# Now if you closely observe the monthly returns they are all close to zero
# This is because of the Bretton Woods system creating a peg of usd to gold
# Since the Bretton Woods system ended on August 1971 we only use the data from that point on
gold_xts_post <- gold_monthly["1971-08/"]

gold_df <- data.frame(
  date = index(gold_xts_post),
  ret = coredata(gold_xts_post$monthly.returns)
)

cutoff_date <- as.Date("1981-01-01")

gold_df <- gold_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Banz", "After Banz"))

gold_summary <- gold_df %>%
  group_by(period) %>%
  summarise(
    Gold_Mean_Return  = mean(monthly.returns, na.rm = TRUE),
    Gold_Std_Dev  = sd(monthly.returns, na.rm = TRUE),
    Gold_Sharpe_Ratio  = Gold_Mean_Return / Gold_Std_Dev
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Join gold summary to the consolidated country summary
full_summary_with_gold <- full_summary %>%
  left_join(gold_summary, by = "period")

print(full_summary_with_gold)

# Gold has a higher Sharpe ratio than market equity factor 
# Interpretation in word

# Spanning Regressions 
fama_french_df <- FF3 %>%
  mutate(
    date = ymd(paste0(Time, "01")), 
    date = ceiling_date(date, "month") - 1,
    Mkt_RF = Mkt_RF/100, 
    SMB = SMB/100,
    HML = HML/100,
    RF = RF/100) %>%
  filter(date >= as.Date("1926-01-01") & date <= as.Date("2024-12-31")) %>%
  arrange(date)

fama_french_df

# Merge Market Equity with Fama-French factors
merged_df_me <- me_usa %>%
  inner_join(fama_french_df, by = "date")

# CAPM Spanning Regression 
capm_model <- lm(ret ~ Mkt_RF, data = merged_df_me)
summary(capm_model)

# Fama French 3- Factor Spanning Regression
ff3_model <- lm(ret ~ Mkt_RF + SMB + HML, data = merged_df_me)
summary(ff3_model)

# Hou et. al Factors 
hou_five_factor <- hou_five_factor %>%
  mutate(
    date = make_date(year, month, 1),           # creates the 1st of each month
    date = ceiling_date(date, "month") - days(1),
    R_F = R_F/100, 
    R_MKT = R_MKT/100,
    R_ME = R_ME/100,
    R_IA = R_IA/100,
    R_ROE = R_ROE/100,
    R_EG = R_EG/100)

merged_df_me_hou <- me_usa %>%
  inner_join(hou_five_factor, by = "date")

# Hou et. al Factors Spanning Regression
hou_model <- lm(ret ~ R_MKT + R_ME + R_IA + R_ROE + R_EG , data = merged_df_me_hou)
summary(hou_model)

# Alternative model: FF3 + R&D capital to book assets + Amihud measure 

# Ensuring that the date column is formatted in the correct manner 
amihud <- amihud %>%
  mutate(date = as.Date(date)) %>%
  rename(AMI = ret) %>%
  select(date, AMI)

RD_cba <- RD_cba %>%
  mutate(date = as.Date(date)) %>%
  rename(RDC = ret) %>%
  select(date, RDC)

# Merging the amihud and RD_cba data on to the fama_french_df
fama_extended <- merged_df_me %>%
  left_join(amihud, by = "date") %>%
  left_join(RD_cba, by = "date")

# Finally we can now run the regression with the alternative model 
# FF3 Extended - Factor Spanning Regression
ff3_model_extended <- lm(ret ~ Mkt_RF + SMB + HML + RDC + AMI, data = fama_extended)
summary(ff3_model_extended)

# -----------------------------------------------
# R&D to Sales Factor Data
# -----------------------------------------------

# -----------------------------------------------
# Step 1: Preparing and Understanding the R&D to Sales Factor Data
# -----------------------------------------------

# We begin our analysis with the R&D to Sales factor data obtained from the Global Factor Dataset.

# According to the dataset documentation:
# - The column "ret" refers to the monthly total return of the factor portfolio.
# - The column "direction" indicates the sorting direction:
#     - "1" corresponds to a long portfolio (e.g., Small minus Big).
#     - "-1" corresponds to a short portfolio (e.g., Big minus Small).

# For context, the seminal paper by (Chan et al., 2001) examined the size effect using stock market data from the United States,
# which is classified as a developed market.

# To broaden the scope of our analysis beyond a single developed country, we include Austria as a second developed market.

# To further enhance the robustness and generalizability of our findings, we also include data from India,
# which is considered an emerging market.

# Filtering for countries: "USA", "AUT", "IND" 
# Reading the date columns as Date
# Using the direction to calculate adjusted returns

rds_usa <- rds_raw %>%
  filter(location == "usa") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)


rds_aut <- rds_raw %>%
  filter(location == "aut") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret))%>%
  arrange(date)

rds_ind <- rds_raw %>%
  filter(location == "ind") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)

mean_ret_usa <- mean(rds_usa$ret, na.rm = TRUE)
std_dev_usa <- sd(rds_usa$ret, na.rm = TRUE)
sharpe_ratio_usa <- mean_ret_usa / std_dev_usa  # assuming zero risk-free rate

mean_ret_aut <- mean(rds_aut$ret, na.rm = TRUE)
std_dev_aut <- sd(rds_aut$ret, na.rm = TRUE)
sharpe_ratio_aut <- mean_ret_aut / std_dev_aut  # assuming zero risk-free rate

mean_ret_ind <- mean(rds_ind$ret, na.rm = TRUE)
std_dev_ind <- sd(rds_ind$ret, na.rm = TRUE)
sharpe_ratio_ind <- mean_ret_ind / std_dev_ind  # assuming zero risk-free rate

# Display summary table
summary_table <- data.frame(
  Metric = c("Mean Return", "Standard Deviation", "Sharpe Ratio"),
  USA = c(mean_ret_usa, std_dev_usa, sharpe_ratio_usa),
  AUT = c(mean_ret_aut, std_dev_aut, sharpe_ratio_aut),
  IND = c(mean_ret_ind, std_dev_ind, sharpe_ratio_ind)
)

# Print the table
print(summary_table)

# Cumulative Return Plot
ggplot(rds_usa, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - United States (R&D to Sales Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(rds_aut, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - Austria (R&D to Sales Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(rds_ind, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - India (R&D to Sales Factor)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

# Rolling Window Analysis 

# We decided to choose a rolling window of 12 months since this is reflective of the 
# annual investment horizon. Furthermore, the one year period captures short term economic 
# fluctuations.

# Rolling window length (in months)
window_length <- 12

# Create rolling mean, SD, and Sharpe ratio - usa
rds_usa <- rds_usa %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - aut
rds_aut <- rds_aut %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - ind
rds_ind <- rds_ind %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

ggplot(rds_usa, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: R&D to Sales Factor (United States)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(rds_aut, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: R&D to Sales Factor (Austria)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(rds_ind, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: R&D to Sales Factor (India)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

# Now we can analyze the plot to see any possible trends 
# To analyze trend we look at the maximum, minimum, and average of the Sharpe ratio's
rolling_sharpe_mean_usa <- mean(rds_usa$roll_sharpe, na.rm = TRUE)
rolling_max_usa <- max(rds_usa$roll_sharpe, na.rm = TRUE)
max_sharpe_date_usa <- rds_usa$date[which.max(rds_usa$roll_sharpe)]
rolling_min_usa <- min(rds_usa$roll_sharpe, na.rm = TRUE)
min_sharpe_date_usa <- rds_usa$date[which.min(rds_usa$roll_sharpe)]

rolling_sharpe_mean_aut <- mean(rds_aut$roll_sharpe, na.rm = TRUE)
rolling_max_aut <- max(rds_aut$roll_sharpe, na.rm = TRUE)
max_sharpe_date_aut <- rds_aut$date[which.max(rds_aut$roll_sharpe)]
rolling_min_aut <- min(rds_aut$roll_sharpe, na.rm = TRUE)
min_sharpe_date_aut <- rds_aut$date[which.min(rds_aut$roll_sharpe)]

rolling_sharpe_mean_ind <- mean(rds_ind$roll_sharpe, na.rm = TRUE)
rolling_max_ind <- max(rds_ind$roll_sharpe, na.rm = TRUE)
max_sharpe_date_ind <- rds_ind$date[which.max(rds_ind$roll_sharpe)]
rolling_min_ind <- min(rds_ind$roll_sharpe, na.rm = TRUE)
min_sharpe_date_ind <- rds_ind$date[which.min(rds_ind$roll_sharpe)]

# Display summary table rolling Sharpe
summary_table_rs <- data.frame(
  Country = c("USA", "AUT", "IND"),
  Mean_Sharpe = c(rolling_sharpe_mean_usa, rolling_sharpe_mean_aut, rolling_sharpe_mean_ind),
  Max_Sharpe = c(rolling_max_usa, rolling_max_aut, rolling_max_ind),
  Min_Sharpe = c(rolling_min_usa, rolling_min_aut, rolling_min_ind)
)

print(summary_table_rs)

# Now we assess the performance of R&D to Sales factor before and after the
# publication of Chan et al. (Chan et al., 2001)

# Before/after Chan et al. indicator cutoff = Dec 2001
cutoff_date <- as.Date("2001-12-01")

# usa 
rds_usa <- rds_usa %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

# Group by Before/after Chan et al. and summarize mean, sd, Sharpe
performance_summary_usa <- rds_usa %>%
  mutate(period = factor(period, levels = c("Before Chan et al.","After Chan et al."))) %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# View result
print(performance_summary_usa)

# aut 
# Before/after Chan et al. indicator cutoff = Dec 2001
rds_aut <- rds_aut %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

# Group by Before/after Chan et al. and summarize mean, sd, Sharpe
performance_summary_aut <- rds_aut %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# ind 
# Before/after Chan et al. indicator cutoff = Dec 2001
rds_ind <- rds_ind %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

# Group by Before/after Chan et al. and summarize mean, sd, Sharpe
performance_summary_ind <- rds_ind %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# Add country labels
performance_summary_usa$Country <- "USA"
performance_summary_aut$Country <- "AUT"
performance_summary_ind$Country <- "IND"

# Combine into one data frame
combined_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Reorder columns for clarity
combined_summary <- combined_summary %>%
  select(Country, period, Mean_Return, Std_Dev, Sharpe_Ratio)

# Optional: Round for presentation
combined_summary <- combined_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View result
print(combined_summary)

# Creating a cumulative return plot - using period wise grouping 

rds_usa <- rds_usa %>%
  arrange(date) %>%
  group_by(period) %>%
  mutate(period_cum_return = cumprod(1 + ret))

ggplot(rds_usa, aes(x = date, y = period_cum_return, color = period)) +
  geom_line() +
  labs(
    title = "Cumulative Return of R&D to Sales Factor (USA)",
    x = "Date",
    y = "Cumulative Return",
    color = "Period"
  ) +
  theme_minimal()

# Check word for interpretation 

# Get S&P 500 (USA) index data (ticker: ^GSPC) (only data from 1927-12-30 available)
getSymbols("^GSPC", src = "yahoo", from = "1926-01-01",to = "2024-12-31", get = "stock.prices")
sp500_monthly_ret <- monthlyReturn(GSPC[, "GSPC.Adjusted"], name = "sp500_ret")
sp500_df <- data.frame(
  date = index(sp500_monthly_ret),
  ret = coredata(sp500_monthly_ret)
)

cutoff_date <- as.Date("2001-12-01")
sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

performance_summary_sp500 <- sp500_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_sp500)

# ATX (Austria) — ticker symbol: ^ATX (only data from 1999-09-30 available)
getSymbols("^ATX", src = "yahoo", from = "1986-09-30", to = "2024-12-31", get = "stock.prices")

atx_monthly_ret <- monthlyReturn(ATX[, "ATX.Adjusted"], name = "atx_ret")
atx_df <- data.frame(
  date = index(atx_monthly_ret),
  ret = coredata(atx_monthly_ret)
)

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

performance_summary_atx <- atx_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_atx)


# Nifty 50 (India) — ticker symbol: ^NSEI (only data from 2007-09-17 available )
getSymbols("^NSEI", src = "yahoo", from = "1988-09-30", to = "2024-12-31", get = "stock.prices")

nsei_monthly_ret <- monthlyReturn(NSEI[, "NSEI.Adjusted"], name = "nsei_ret")
nsei_df <- data.frame(
  date = index(nsei_monthly_ret),
  ret = coredata(nsei_monthly_ret)
)

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

performance_summary_nsei <- nsei_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_nsei)

# Full summary including the benchmark 

# Add country labels to benchmark summaries
performance_summary_sp500$Country <- "USA"
performance_summary_atx$Country <- "AUT"
performance_summary_nsei$Country <- "IND"

# Rename benchmark columns
performance_summary_sp500 <- performance_summary_sp500 %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_atx <- performance_summary_atx %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_nsei <- performance_summary_nsei %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

# Combine factor summaries
factor_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Combine benchmark summaries
benchmark_summary <- bind_rows(
  performance_summary_sp500,
  performance_summary_atx,
  performance_summary_nsei
)

# Join on Country and period
full_summary <- factor_summary %>%
  left_join(benchmark_summary, by = c("Country", "period"))

# Reorder and round
full_summary <- full_summary %>%
  select(Country, period,
         Mean_Return, Std_Dev, Sharpe_Ratio,
         BM_Mean_Return, BM_Std_Dev, BM_Sharpe_Ratio) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View final table
print(full_summary)

# Interpretation in the word document 

# Now we compare with an alternative asset class - we choose gold 

gold_raw <- gold_raw %>%
  mutate(date = mdy(Date)) %>%
  arrange(date)

# Convert to xts object
gold_xts <- xts(gold_raw$Last.Price, order.by = gold_raw$date)

# Now compute monthly returns
gold_monthly <- monthlyReturn(gold_xts, name = "Gold_Return")

# Now if you closely observe the monthly returns they are all close to zero
# This is because of the Bretton Woods system creating a peg of usd to gold
# Since the Bretton Woods system ended on August 1971 we only use the data from that point on
gold_xts_post <- gold_monthly["1971-08/"]

gold_df <- data.frame(
  date = index(gold_xts_post),
  ret = coredata(gold_xts_post$monthly.returns)
)

cutoff_date <- as.Date("2001-12-01")

gold_df <- gold_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Chan et al.", "After Chan et al."))

gold_summary <- gold_df %>%
  group_by(period) %>%
  summarise(
    Gold_Mean_Return  = mean(monthly.returns, na.rm = TRUE),
    Gold_Std_Dev  = sd(monthly.returns, na.rm = TRUE),
    Gold_Sharpe_Ratio  = Gold_Mean_Return / Gold_Std_Dev
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Join gold summary to the consolidated country summary
full_summary_with_gold <- full_summary %>%
  left_join(gold_summary, by = "period")

print(full_summary_with_gold)

# Gold has a higher Sharpe ratio than R&D to Sales factor 
# Interpretation in word

# Spanning Regressions 
fama_french_df <- FF3 %>%
  mutate(
    date = ymd(paste0(Time, "01")), 
    date = ceiling_date(date, "month") - 1,
    Mkt_RF = Mkt_RF/100, 
    SMB = SMB/100,
    HML = HML/100,
    RF = RF/100) %>%
  filter(date >= as.Date("1926-01-01") & date <= as.Date("2024-12-31")) %>%
  arrange(date)

fama_french_df

# Merge R&D to Sales with Fama-French factors
merged_df_rds <- rds_usa %>%
  inner_join(fama_french_df, by = "date")

# CAPM Spanning Regression 
capm_model <- lm(ret ~ Mkt_RF, data = merged_df_rds)
summary(capm_model)

# Fama French 3- Factor Spanning Regression
ff3_model <- lm(ret ~ Mkt_RF + SMB + HML, data = merged_df_rds)
summary(ff3_model)

# Hou et. al Factors 
hou_five_factor <- hou_five_factor %>%
  mutate(
    date = make_date(year, month, 1),           # creates the 1st of each month
    date = ceiling_date(date, "month") - days(1),
    R_F = R_F/100, 
    R_MKT = R_MKT/100,
    R_ME = R_ME/100,
    R_IA = R_IA/100,
    R_ROE = R_ROE/100,
    R_EG = R_EG/100)

merged_df_rds_hou <- rds_usa %>%
  inner_join(hou_five_factor, by = "date")

# Hou et. al Factors Spanning Regression
hou_model <- lm(ret ~ R_MKT + R_ME + R_IA + R_ROE + R_EG , data = merged_df_rds_hou)
summary(hou_model)

# Alternative model: FF3 + R&D capital to book assets + Amihud measure 

# Ensuring that the date column is formatted in the correct manner 
amihud <- amihud %>%
  mutate(date = as.Date(date)) %>%
  select(date, AMI)

RD_cba <- RD_cba %>%
  mutate(date = as.Date(date)) %>%
  select(date, RDC)

# Merging the amihud and RD_cba data on to the fama_french_df
fama_extended <- merged_df_rds %>%
  left_join(amihud, by = "date") %>%
  left_join(RD_cba, by = "date")

# Finally we can now run the regression with the alternative model 
# FF3 Extended - Factor Spanning Regression
ff3_model_extended <- lm(ret ~ Mkt_RF + SMB + HML + RDC + AMI, data = fama_extended)
summary(ff3_model_extended)

# -----------------------------------------------
# Liquidity of Book Assets
# -----------------------------------------------

# -----------------------------------------------
# Step 1: Preparing and Understanding the Liquidity of Book Assets
# -----------------------------------------------

# We begin our analysis with the Liquidity of Book Assets factor data obtained from the Global Factor Dataset.

# According to the dataset documentation:
# - The column "ret" refers to the monthly total return of the factor portfolio.
# - The column "direction" indicates the sorting direction:
#     - "1" corresponds to a long portfolio (e.g., Small minus Big).
#     - "-1" corresponds to a short portfolio (e.g., Big minus Small).

# For context, the seminal paper by (Ortiz-Molina & Phillips, 2014) 
# which is classified as a developed market.

# To broaden the scope of our analysis beyond a single developed country, we include Austria as a second developed market.

# To further enhance the robustness and generalizability of our findings, we also include data from India,
# which is considered an emerging market.

# Filtering for countries: "USA", "AUT", "IND" 
# Reading the date columns as Date
# Using the direction to calculate adjusted returns

lba_usa <- lba_raw %>%
  filter(location == "usa") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)

lba_aut <- lba_raw %>%
  filter(location == "aut") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret))%>%
  arrange(date)

lba_ind <- lba_raw %>%
  filter(location == "ind") %>%
  mutate(date = as.Date(date),               
         cum_ret = cumprod(1+ret)) %>%
  arrange(date)

mean_ret_usa <- mean(lba_usa$ret, na.rm = TRUE)
std_dev_usa <- sd(lba_usa$ret, na.rm = TRUE)
sharpe_ratio_usa <- mean_ret_usa / std_dev_usa  # assuming zero risk-free rate

mean_ret_aut <- mean(lba_aut$ret, na.rm = TRUE)
std_dev_aut <- sd(lba_aut$ret, na.rm = TRUE)
sharpe_ratio_aut <- mean_ret_aut / std_dev_aut  # assuming zero risk-free rate

mean_ret_ind <- mean(lba_ind$ret, na.rm = TRUE)
std_dev_ind <- sd(lba_ind$ret, na.rm = TRUE)
sharpe_ratio_ind <- mean_ret_ind / std_dev_ind  # assuming zero risk-free rate

# Display summary table
summary_table <- data.frame(
  Metric = c("Mean Return", "Standard Deviation", "Sharpe Ratio"),
  USA = c(mean_ret_usa, std_dev_usa, sharpe_ratio_usa),
  AUT = c(mean_ret_aut, std_dev_aut, sharpe_ratio_aut),
  IND = c(mean_ret_ind, std_dev_ind, sharpe_ratio_ind)
)

# Print the table
print(summary_table)

# Cumulative Return Plot
ggplot(lba_usa, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - United States (Liquidity to Book Assets)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(lba_aut, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - Austria (Liquidity to Book Assets)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

ggplot(lba_ind, aes(x = date, y = cum_ret)) +
  geom_line(color = "blue") +
  labs(
    title = "Cumulative Return - India (Liquidity to Book Assets)",
    x = "Date",
    y = "Cumulative Return"
  ) +
  theme_minimal()

# Rolling Window Analysis 

# We decided to choose a rolling window of 12 months since this is reflective of the 
# annual investment horizon. Furthermore, the one year period captures short term economic 
# fluctuations.

# Rolling window length (in months)
window_length <- 12

# Create rolling mean, SD, and Sharpe ratio - usa
lba_usa <- lba_usa %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - aut
lba_aut <- lba_aut %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

# Create rolling mean, SD, and Sharpe ratio - ind
lba_ind <- lba_ind %>%
  mutate(
    roll_mean = rollapply(ret, width = window_length, FUN = mean, align = "right", fill = NA, na.rm = TRUE),
    roll_sd = rollapply(ret, width = window_length, FUN = sd, align = "right", fill = NA, na.rm = TRUE),
    roll_sharpe = roll_mean / roll_sd
  )

ggplot(lba_usa, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Liquidity to Book Assets (United States)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(lba_aut, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Liquidity to Book Assets (Austria)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

ggplot(lba_ind, aes(x = date, y = roll_sharpe)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Rolling 12-Month Sharpe Ratio: Liquidity to Book Assets (India)",
    x = "Date",
    y = "Sharpe Ratio"
  ) +
  theme_minimal()

# Now we can analyze the plot to see any possible trends 
# To analyze trend we look at the maximum, minimum, and average of the Sharpe ratio's
rolling_sharpe_mean_usa <- mean(lba_usa$roll_sharpe, na.rm = TRUE)
rolling_max_usa <- max(lba_usa$roll_sharpe, na.rm = TRUE)
max_sharpe_date_usa <- lba_usa$date[which.max(lba_usa$roll_sharpe)]
rolling_min_usa <- min(lba_usa$roll_sharpe, na.rm = TRUE)
min_sharpe_date_usa <- lba_usa$date[which.min(lba_usa$roll_sharpe)]

rolling_sharpe_mean_aut <- mean(lba_aut$roll_sharpe, na.rm = TRUE)
rolling_max_aut <- max(lba_aut$roll_sharpe, na.rm = TRUE)
max_sharpe_date_aut <- lba_aut$date[which.max(lba_aut$roll_sharpe)]
rolling_min_aut <- min(lba_aut$roll_sharpe, na.rm = TRUE)
min_sharpe_date_aut <- lba_aut$date[which.min(lba_aut$roll_sharpe)]

rolling_sharpe_mean_ind <- mean(lba_ind$roll_sharpe, na.rm = TRUE)
rolling_max_ind <- max(lba_ind$roll_sharpe, na.rm = TRUE)
max_sharpe_date_ind <- lba_ind$date[which.max(lba_ind$roll_sharpe)]
rolling_min_ind <- min(lba_ind$roll_sharpe, na.rm = TRUE)
min_sharpe_date_ind <- lba_ind$date[which.min(lba_ind$roll_sharpe)]

# Display summary table rolling Sharpe
summary_table_rs <- data.frame(
  Country = c("USA", "AUT", "IND"),
  Mean_Sharpe = c(rolling_sharpe_mean_usa, rolling_sharpe_mean_aut, rolling_sharpe_mean_ind),
  Max_Sharpe = c(rolling_max_usa, rolling_max_aut, rolling_max_ind),
  Min_Sharpe = c(rolling_min_usa, rolling_min_aut, rolling_min_ind)
)

print(summary_table_rs)

# Now we assess the performance of Liquidity to Book Assets before and after the
# publication of Ortiz-Molina & Phillips (Ortiz-Molina & Phillips, 2014)

# Before/after Ortiz-Molina & Phillips indicator cutoff = Feb 2014
cutoff_date <- as.Date("2014-02-01")

# usa 
lba_usa <- lba_usa %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

# Group by Before/after Ortiz-Molina & Phillips and summarize mean, sd, Sharpe
performance_summary_usa <- lba_usa %>%
  mutate(period = factor(period, levels = c("Before Ortiz-Molina & Phillips","After Ortiz-Molina & Phillips"))) %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# View result
print(performance_summary_usa)

# aut 
# Before/after Ortiz-Molina & Phillips indicator cutoff = Feb 2014
lba_aut <- lba_aut %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

# Group by Before/after Ortiz-Molina & Phillips and summarize mean, sd, Sharpe
performance_summary_aut <- lba_aut %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# ind 
# Before/after Ortiz-Molina & Phillips indicator cutoff = Feb 2014
lba_ind <- lba_ind %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

# Group by Before/after Ortiz-Molina & Phillips and summarize mean, sd, Sharpe
performance_summary_ind <- lba_ind %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(ret, na.rm = TRUE),
    Std_Dev = sd(ret, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  )

# Add country labels
performance_summary_usa$Country <- "USA"
performance_summary_aut$Country <- "AUT"
performance_summary_ind$Country <- "IND"

# Combine into one data frame
combined_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Reorder columns for clarity
combined_summary <- combined_summary %>%
  select(Country, period, Mean_Return, Std_Dev, Sharpe_Ratio)

# Optional: Round for presentation
combined_summary <- combined_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View result
print(combined_summary)

# Creating a cumulative return plot - using period wise grouping 

lba_usa <- lba_usa %>%
  arrange(date) %>%
  group_by(period) %>%
  mutate(period_cum_return = cumprod(1 + ret))

ggplot(lba_usa, aes(x = date, y = period_cum_return, color = period)) +
  geom_line() +
  labs(
    title = "Cumulative Return of Liquidity to Book Assets (USA)",
    x = "Date",
    y = "Cumulative Return",
    color = "Period"
  ) +
  theme_minimal()

# Check word for interpretation 

# Get S&P 500 (USA) index data (ticker: ^GSPC) (only data from 1927-12-30 available)
getSymbols("^GSPC", src = "yahoo", from = "1926-01-01",to = "2024-12-31", get = "stock.prices")
sp500_monthly_ret <- monthlyReturn(GSPC[, "GSPC.Adjusted"], name = "sp500_ret")
sp500_df <- data.frame(
  date = index(sp500_monthly_ret),
  ret = coredata(sp500_monthly_ret)
)

cutoff_date <- as.Date("2014-02-01")
sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

sp500_df <- sp500_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

performance_summary_sp500 <- sp500_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_sp500)

# ATX (Austria) — ticker symbol: ^ATX (only data from 1999-09-30 available)
getSymbols("^ATX", src = "yahoo", from = "1986-09-30", to = "2024-12-31", get = "stock.prices")

atx_monthly_ret <- monthlyReturn(ATX[, "ATX.Adjusted"], name = "atx_ret")
atx_df <- data.frame(
  date = index(atx_monthly_ret),
  ret = coredata(atx_monthly_ret)
)

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

atx_df <- atx_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

performance_summary_atx <- atx_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_atx)


# Nifty 50 (India) — ticker symbol: ^NSEI (only data from 2007-09-17 available )
getSymbols("^NSEI", src = "yahoo", from = "1988-09-30", to = "2024-12-31", get = "stock.prices")

nsei_monthly_ret <- monthlyReturn(NSEI[, "NSEI.Adjusted"], name = "nsei_ret")
nsei_df <- data.frame(
  date = index(nsei_monthly_ret),
  ret = coredata(nsei_monthly_ret)
)

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

nsei_df <- nsei_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

performance_summary_nsei <- nsei_df %>%
  group_by(period) %>%
  summarise(
    Mean_Return = mean(monthly.returns, na.rm = TRUE),
    Std_Dev = sd(monthly.returns, na.rm = TRUE),
    Sharpe_Ratio = Mean_Return / Std_Dev
  ) %>%
  mutate(across(where(is.numeric), round, 4))

# View result
print(performance_summary_nsei)

# Full summary including the benchmark 

# Add country labels to benchmark summaries
performance_summary_sp500$Country <- "USA"
performance_summary_atx$Country <- "AUT"
performance_summary_nsei$Country <- "IND"

# Rename benchmark columns
performance_summary_sp500 <- performance_summary_sp500 %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_atx <- performance_summary_atx %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

performance_summary_nsei <- performance_summary_nsei %>%
  rename(
    BM_Mean_Return = Mean_Return,
    BM_Std_Dev = Std_Dev,
    BM_Sharpe_Ratio = Sharpe_Ratio
  )

# Combine factor summaries
factor_summary <- bind_rows(
  performance_summary_usa,
  performance_summary_aut,
  performance_summary_ind
)

# Combine benchmark summaries
benchmark_summary <- bind_rows(
  performance_summary_sp500,
  performance_summary_atx,
  performance_summary_nsei
)

# Join on Country and period
full_summary <- factor_summary %>%
  left_join(benchmark_summary, by = c("Country", "period"))

# Reorder and round
full_summary <- full_summary %>%
  select(Country, period,
         Mean_Return, Std_Dev, Sharpe_Ratio,
         BM_Mean_Return, BM_Std_Dev, BM_Sharpe_Ratio) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# View final table
print(full_summary)

# Interpretation in the word document 

# Now we compare with an alternative asset class - we choose gold 

gold_raw <- gold_raw %>%
  mutate(date = mdy(Date)) %>%
  arrange(date)

# Convert to xts object
gold_xts <- xts(gold_raw$Last.Price, order.by = gold_raw$date)

# Now compute monthly returns
gold_monthly <- monthlyReturn(gold_xts, name = "Gold_Return")

# Now if you closely observe the monthly returns they are all close to zero
# This is because of the Bretton Woods system creating a peg of usd to gold
# Since the Bretton Woods system ended on August 1971 we only use the data from that point on
gold_xts_post <- gold_monthly["1971-08/"]

gold_df <- data.frame(
  date = index(gold_xts_post),
  ret = coredata(gold_xts_post$monthly.returns)
)

cutoff_date <- as.Date("2014-02-01")

gold_df <- gold_df %>%
  mutate(period = ifelse(date < cutoff_date, "Before Ortiz-Molina & Phillips", "After Ortiz-Molina & Phillips"))

gold_summary <- gold_df %>%
  group_by(period) %>%
  summarise(
    Gold_Mean_Return  = mean(monthly.returns, na.rm = TRUE),
    Gold_Std_Dev  = sd(monthly.returns, na.rm = TRUE),
    Gold_Sharpe_Ratio  = Gold_Mean_Return / Gold_Std_Dev
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4)))

# Join gold summary to the consolidated country summary
full_summary_with_gold <- full_summary %>%
  left_join(gold_summary, by = "period")

print(full_summary_with_gold)

# Gold has a higher Sharpe ratio than Liquidity to Book Assets 
# Interpretation in word

# Spanning Regressions 

fama_french_df <- FF3 %>%
  mutate(
    date = ymd(paste0(Time, "01")), 
    date = ceiling_date(date, "month") - 1,
    Mkt_RF = Mkt_RF/100, 
    SMB = SMB/100,
    HML = HML/100,
    RF = RF/100) %>%
  filter(date >= as.Date("1926-01-01") & date <= as.Date("2024-12-31")) %>%
  arrange(date)

fama_french_df

# Merge R&D to Sales with Fama-French factors
merged_df_lba <- lba_usa %>%
  inner_join(fama_french_df, by = "date")

# CAPM Spanning Regression 
capm_model <- lm(ret ~ Mkt_RF, data = merged_df_lba)
summary(capm_model)

# Fama French 3- Factor Spanning Regression
ff3_model <- lm(ret ~ Mkt_RF + SMB + HML, data = merged_df_lba)
summary(ff3_model)

# Hou et. al Factors 
hou_five_factor <- hou_five_factor %>%
  mutate(
    date = make_date(year, month, 1),           # creates the 1st of each month
    date = ceiling_date(date, "month") - days(1),
    R_F = R_F/100, 
    R_MKT = R_MKT/100,
    R_ME = R_ME/100,
    R_IA = R_IA/100,
    R_ROE = R_ROE/100,
    R_EG = R_EG/100)

merged_df_lba_hou <- lba_usa %>%
  inner_join(hou_five_factor, by = "date")

# Hou et. al Factors Spanning Regression
hou_model <- lm(ret ~ R_MKT + R_ME + R_IA + R_ROE + R_EG , data = merged_df_lba_hou)
summary(hou_model)

# Alternative model: FF3 + R&D capital to book assets + Amihud measure 
# Ensuring that the date column is formatted in the correct manner 
amihud <- amihud %>%
  mutate(date = as.Date(date)) %>%
  select(date, AMI)

RD_cba <- RD_cba %>%
  mutate(date = as.Date(date)) %>%
  select(date, RDC)

# Merging the amihud and RD_cba data on to the fama_french_df
fama_extended <- merged_df_lba %>%
  left_join(amihud, by = "date") %>%
  left_join(RD_cba, by = "date")

# Finally we can now run the regression with the alternative model 
# FF3 Extended - Factor Spanning Regression
ff3_model_extended <- lm(ret ~ Mkt_RF + SMB + HML + RDC + AMI, data = fama_extended)
summary(ff3_model_extended)

