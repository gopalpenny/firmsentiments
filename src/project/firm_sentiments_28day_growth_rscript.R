# This script prepares and joins firmrisk Sentiments and stock market growth. 
# After merging the two datasets, it selects only dates within 14 days of the
# Earnings statement, and calculates average daily market growth over these
# days. It then considered three regressions to determine how closely related
# both datasets are in terms of firm Sentiment and stock market growth. The code
# follows the outline here:
# 0. Load and prep the data
# 1a. the raw data
# 1b. data binned by average market growth
# 1c. data binned by sentiment
# 2. Plot the data

# load libraries
library(data.table)
library(tidyverse)

# 0. Load and prep the data

# Load data if file exists already. Otherwise, prepare using data.table
if (file.exists("data/format/stocks_28day_avg_growth.csv")) {
  stocks_28day_growth <- read_csv("data/format/stocks_28day_avg_growth.csv")
} else {
  # Read firmrisk to data.table
  firmrisk <- fread("data/format/firmquarter_2020q4.csv")
  firmrisk <- firmrisk[, .(year_quarter = date, ticker, Risk, Sentiment, date_earningscall)]
  firmrisk <- firmrisk[, RollDate := date_earningscall - 45] # Shift join data by 45 days for rolling join
  
  # Read stock histories to data.table
  stocks <- fread("data/format/yfinance_stock_histories.csv")
  stocks <- stocks[order(ticker, date), .(Close, date, ticker)]
  stocks <- stocks[, `:=`(RollDate = date, CloseLag = shift(Close)), by = ticker] # to calculate daily growth
  stocks <- stocks[, `:=`(CloseGrowth = (Close - CloseLag)/CloseLag)]
  
  # dt_test <- setDT(data.frame(num = 1:7, group = c("a","a","b","b","b","c","c")))
  
  # Set join keys
  setkey(stocks, ticker, RollDate)
  setkey(firmrisk, ticker, RollDate)
  
  # Join stocks and firmrisk data, using rolling date match
  stocks_growth_rolljoin <- firmrisk[stocks, roll = TRUE, nomatch = 0]
  
  # Filter stock growth data to be within 14 days of earnings call
  stocks_growth_28day_all <- stocks_growth_rolljoin[abs(date_earningscall - date) <= 14]
  
  # Get 28-day average stock growth, centered on the date of the earnings call
  stocks_28day_growth <- as_tibble(stocks_growth_28day[, .(avg_growth = mean(CloseGrowth, na.rm = TRUE), 
                                                           sd_growth = sd(CloseGrowth, na.rm = TRUE), 
                                                           Risk = first(Risk), Sentiment = first(Sentiment)),
                                                       by = .(ticker, year_quarter, date_earningscall)])
  
  rm(list = c("stocks", "stocks_growth_rolljoin", "stocks_growth_28day_all"))
  
  write_csv(stocks_28day_growth, "data/format/stocks_28day_avg_growth.csv")
}

# Identify outliers in average 28day growth
stocks_28day_growth_IQR <- quantile(stocks_28day_growth$avg_growth, c(0.25, 0.75), na.rm = TRUE)
stocks_28day_growth_IQR_outliers <- stocks_28day_growth_IQR + 
  (stocks_28day_growth_IQR[2] - stocks_28day_growth_IQR[1]) * c(-1.5, 1.5)
stocks_28day_growth <- stocks_28day_growth %>%
  mutate(outlier = !between(avg_growth, stocks_28day_growth_IQR_outliers[1], stocks_28day_growth_IQR_outliers[2]))


# 1a. Firm sentiment vs average growth using the raw 28-day averages
firms_df <- stocks_28day_growth %>% filter(!outlier)
lm_28day_all <- summary(lm(Sentiment ~ avg_growth, firms_df))
lm_28day_all



# Group the average 28day growth into 10 equal-sized bins, and do the same for Sentiment scores.
firms_df_binned <- firms_df %>%
  mutate(avg_growth_bins = cut(avg_growth, quantile(avg_growth, seq(0, 1, by = 0.1), na.rm = TRUE), include.lowest = TRUE),
         sentiment_bins = cut(Sentiment, quantile(Sentiment, seq(0, 1, by = 0.1), na.rm = TRUE), include.lowest = TRUE))

# Get the lower limit of each bin
avg_growth_bins_ll <- as.numeric(gsub("(\\()|(\\[)|(,.*)", "", levels(firms_df_binned$avg_growth_bins)))
sentiment_bins_ll <- as.numeric(gsub("(\\()|(\\[)|(,.*)", "", levels(firms_df_binned$sentiment_bins)))#)



## Binned average quarterly growth (1b and 1c)

# 1b. Firm sentiment vs average growth using averages of grouped data -- binned
# into 10 equal-sized groups based on **Average growth**.
firms_df_binned_growth_means <- firms_df_binned %>% 
  group_by(avg_growth_bins) %>%
  summarize(mean_growth = mean(avg_growth, na.rm = TRUE),
            mean_sentiment = mean(Sentiment, na.rm = TRUE))

lm_growth_binned <- summary(lm(mean_sentiment ~ mean_growth, firms_df_binned_growth_means))
lm_growth_binned


# 1c. Firm sentiment vs average growth using averages of grouped data -- binned
# into 10 equal-sized groups based on **Sentiment**.
firms_df_binned_sentiment_means <- firms_df_binned %>% 
  group_by(sentiment_bins) %>%
  summarize(mean_growth = mean(avg_growth,na.rm = TRUE),
            mean_sentiment = mean(Sentiment,na.rm = TRUE))

lm_sentiment_binned <- summary(lm(mean_sentiment ~ mean_growth, firms_df_binned_sentiment_means))
lm_sentiment_binned



# 2. Plot the data
## Average quarterly growth versus Sentiment

# The following plot shows average average 28-day growth versus average
# sentiment for all year-quarter combinations as well as for both summaries of
# binned data.
lm_coefficients <- rbind(with(lm_28day_all, c(coefficients[,1], coefficients[2,4], r.squared)),
                         with(lm_growth_binned, c(coefficients[,1], coefficients[2,4], r.squared)),
                         with(lm_sentiment_binned, c(coefficients[,1], coefficients[2,4], r.squared))) %>%
  as_tibble() %>% set_names(c("intercept", "slope", "p_val_slope", "regression_r_squared"))

ggplot() + 
  geom_point(data = firms_df, aes(avg_growth, Sentiment), alpha = 0.01) +
  geom_vline(data = data.frame(intercept = avg_growth_bins_ll[-1]), 
             aes(xintercept = intercept,color = "Growth bins"), alpha = 0.75, linetype = "dashed") +
  geom_abline(data = data.frame(intercept = sentiment_bins_ll[-1], slope = 0), 
              aes(slope = slope, intercept = intercept,color = "Sentiment bins"), alpha = 0.75, linetype = "dashed") +
  geom_point(data = firms_df_binned_sentiment_means, aes(mean_growth, mean_sentiment, color = "Sentiment bins")) +
  geom_point(data = firms_df_binned_growth_means, aes(mean_growth, mean_sentiment, color = "Growth bins")) +
  xlab("Average 28-day growth, [%/day]") +
  coord_flip()