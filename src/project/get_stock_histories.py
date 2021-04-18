#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 18 13:46:53 2021

@author: gopal
"""

import os
import yfinance as yf
import pandas as pd
from datetime import datetime

working_path = "/Users/gopal/Projects/DataScience/firmsentiments"
os.chdir(working_path)
fq = pd.read_csv("data/format/firmquarter_2020q4.csv", parse_dates=['date_earningscall'])

fq.date_earningscall.min()


#ticker = fq.ticker.unique()[2]
def get_ticker_history(ticker):
    # Define yfinance ticker and get stock history
    corp_ticker = yf.Ticker(ticker)
    corp_hist = corp_ticker.history(interval = '1d', 
                                    start = datetime.strptime("2000-01-01", "%Y-%m-%d"), 
                                    end = datetime.today())
    corp_hist['date'] = pd.to_datetime(corp_hist.index)
    corp_hist['date'] = corp_hist['date'].dt.date
    
    # Define if data is unavailable, add a row
    if corp_hist.shape[0] == 0:
        corp_hist = corp_hist.append(pd.Series(name=ticker))
        
    # Add the ticker to the dataframe
    corp_hist['ticker'] = ticker
    
    # return pandas df with stock history
    return corp_hist


unique_tickers = fq[fq['ticker'].notnull()].ticker.unique() # get non-null tickers

ticker_histories_list = []
for ticker in unique_tickers:
    print(ticker)
    ticker_histories_list.append(get_ticker_history(ticker))
    
ticker_histories = pd.concat(ticker_histories_list, ignore_index = True)

ticker_histories.to_csv("data/format/yfinance_stock_histories.csv")



#
#get_ticker_history(fq.ticker.unique()[12])
#
#
#ticker = 'ACETQ'
## Define yfinance ticker and get stock history
#corp_ticker = yf.Ticker(ticker)
#corp_hist = corp_ticker.history(period="max", interval = '1d')
#corp_hist['date'] = pd.to_datetime(corp_hist.index)
#corp_hist['date'] = corp_hist['date'].dt.date
#
## Define if data is unavailable, add a row
#if corp_hist.shape[0] == 0:
#    corp_hist = corp_hist.append(pd.Series(name=ticker))
#    
## Add the ticker to the dataframe
#corp_hist['ticker'] = ticker
#
## return pandas df with stock history
#return corp_hist