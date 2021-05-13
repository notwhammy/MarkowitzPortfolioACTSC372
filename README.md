# MarkowitzPortfolioACTSC372
Markowitz Optimal Portfolio for 10 Canadian Stocks, completed as a part of the ACTSC 372 project

**Please message me to request for source code (in R and R Markdown)**

Project summary:
- Collected data of 10 Canadian Stocks (listed on TSX) from November 2018 to November 2020 using QuantMod and Yahoo Finance in R. The latest 4-month data will be the testing set, and the rest will be used for training / computation
- Estimated the mean and covariance matrix of the daily returns
- Computed the individual weights of optimum portfolio with the Markowitz objective function and constraints
- Backtested portfolio against training set and compare it with the S&P 500 and 1/N strategy using metrics such as mean and variance of returns, beta, sharpe ratio, treynor ratio, and jensen's alpha
- Backtested portfolio against testing set (out-of-sample, July to November 2020). While the S&P 500 returned 10.7% and 1/N returned 6.35% within 4 months, the "least aggressive" optimum portfolio (with tau = 0.05) returned 15.2%, and the "most aggressive" optimum portfolio (with tau = 0.5) returned 94% in just 4 months.

To be added:
- Add more index to compare such as TSX 60, NASDAQ, and Dow
- Add more stocks to the portfolio, or select different styles (growth and value portfolios)
- Add more benchmark metrics such as Sortino Ratio
