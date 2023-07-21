---
title: "ACTSC 372 Project: Markowitz Portfolio Optimization"
author: "Samuel Benedict - 20803232"
date: "11/1/2020"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# a. Data Collection
In this project, we aim to optimize the following portfolio of stocks that are publicly traded in the Toronto Stock Exchange (TSE):
\begin{center}
 \begin{tabular}{||c c c c||} 
 \hline
 Ticker (TSE) & Full Name & Industry & Sub-Industry \\ [0.5ex] 
 \hline\hline
 RY & Royal Bank of Canada & Financial Services & Bank \\ 
 \hline
 MFC & Manulife Corporation & Financial Services & Insurance \\
 \hline
 SHOP & Shopify Inc. & Information Technology & E-Commerce (Service) \\
 \hline
 CSU & Constellation Software Inc. & Information Technology & Software (Holding) \\
 \hline
 DOL & Dollarama Inc. & Consumer Staples & Retail \\
 \hline
 CTC & Canadian Tire Corporation & Consumer Discretionary & Equipments Retail \\
 \hline
 CNR & Canadian National Railway & Industrials & Transportation \\
 \hline
 T & TELUS Corporation & TMT & Telecommunication \\
 \hline
 ENB & Enbridge Inc & Energy & Oil and Gas \\
 \hline
 BEP.UN & Brookfield Renewable Partners LP & Utilities & Renewable Energy \\
 \hline
\end{tabular}
\end{center}

We will be using Quantmod library connected with Yahoo Finance to get the adjusted stock price from November 8, 2018 to November 6, 2020 (24 months). We will use the data from November 8, 2018 to July 7, 2020 (20 months) as our training set, and the data from July 8, 2020 to November 6, 2020 (4 months) as our testing set. 

```{r, warning=FALSE, message=FALSE, error=FALSE, echo = FALSE}
library(quantmod)
library(lubridate)
library(rlist)
tickers <- c("RY.TO", "MFC.TO", "SHOP.TO", "CSU.TO", "DOL.TO", 
             "CTC.TO", "CNR.TO", "T.TO", "ENB.TO", "BEP-UN.TO")

#Initialize training and testing set
train_set <- data.frame(matrix(nrow = 417, ncol = 11))
test_set <- data.frame(matrix(nrow = 85, ncol = 11))

for (i in 1:10){
  temp <- getSymbols(tickers[i], src = "yahoo", from = "2018-11-08", 
                     to = "2020-11-07", env = NULL, auto.assign = FALSE)
  num <- nrow(temp)
  train_set[i+1] <- temp[1:417, 6] # We will only use the "Adjusted" column
  test_set[i+1] <- temp[418:num, 6]
  if (i == 1) {
    train_set[1] <- time(temp[1:417])
    test_set[1] <- time(temp[418:num])
  }
}
colnames(train_set)[2:11] <- tickers
colnames(test_set)[2:11] <- tickers
colnames(train_set)[1] <- "Date"
colnames(test_set)[1] <- "Date"
```

Here are the first 6 (from training set 1:6) and last 6 (from testing set 1:6) adjusted prices
```{r, echo = FALSE}
head(train_set)
tail(test_set)
```

\newpage
# b. Estimation
Here, we are going to estimate the mean and covariance matrix of the stock daily returns. SHown benlow is the first 6 daily returns from the training and testing set, respectively.
```{r, echo = FALSE}
# calculate daily returns
no_train <- nrow(train_set)
no_test <- nrow(test_set)

# set df to store daily returns
daily_returns <- data.frame(matrix(nrow = no_train - 1, ncol = 11))
test_returns <- data.frame(matrix(nrow = no_test, ncol = 11))

for (i in 1:10) {
  if (i == 1) { # store date
    daily_returns[1] <- train_set[[1]][c(2:no_train)]
    test_returns[1] <- test_set[[1]]
  }
  daily_returns[i+1] <- dailyReturn(train_set[[i+1]])[c(2:no_train)] # compute daily return 
  # since we don't have prior data (Price at t<=0), we will eliminate first daily return
  test_returns[2:no_test, i+1] <- dailyReturn(test_set[[i+1]])[c(2:no_test)] 
  # we do have the first daily return for the test set by referring to the last price from train set
  test_returns[1, i+1] <- (test_set[1, i+1][[1]] - train_set[417, i+1][[1]]) /
                          train_set[417,i+1][[1]] 
  
}
# add headers
colnames(daily_returns)[2:11] <- tickers
colnames(daily_returns)[1] <- "Date"
colnames(test_returns)[2:11] <- tickers
colnames(test_returns)[1] <- "Date"

head(daily_returns) # first 6 daily returns (day 2 to 7)
head(test_returns)
```

Here is the expected return $\hat{\mu} = [\mu_{1} ,...,\mu_{10}]$ for the training set
```{r, echo = FALSE}
# calculate expected return 
exp_return <- array() # for train set 
exp_testret <- array() # for test set
for (i in 1:10){
  exp_return[i] <- mean(daily_returns[[i+1]])
  exp_testret[i] <- mean(test_returns[[i+1]])
}
exp_return
```

Here is the estimated covariance matrix $\hat{\Sigma}$ for the training set
```{r, echo = FALSE}
# calculate covariance matrix
cov_matrix <- cov(daily_returns[2:11])
cov_test <- cov(test_returns[2:11])
cov_matrix
```

\newpage
# c. Construction of Portfolios
According to the Markowitz Portfolio Optimization Model, to optimize the objective function
$$\max \ \  \tau w^{T}\mu - \frac{1}{2}w^{T}\Sigma w \ \  \ \text{s.t.} \ \ e^{T}w=1$$, we get that
$$w_{opt} = \tau w_{z} + w_{m}$$
where $w_{opt}$ is the optimum portfolio weight, $w_{z}$ is the zero-covariance portfolio, and $w_{m}$ is the minimum risk portfolio that is given as follows:
$$w_{z} = \Sigma^{-1} \mu - \frac{e^{T}\Sigma^{-1}\mu}{e^{T}\Sigma^{-1}e}\Sigma^{-1}e$$
and
$$w_{m} = \frac{\Sigma^{-1}e}{e^{T}\Sigma^{-1}e}$$
Here is the minimum risk portfolio $w_{m}$
```{r, echo = FALSE}
# set values of tau
tau = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 
        0.1, 0.125, 0.15, 0.175, 0.2, 0.25, 0.3, 0.4, 0.5)
e = rep(1, 10)  # vector of ones
inv_cov_matrix <- solve(cov_matrix)  # sigma inverse

# Minimum-risk portfolio (W_m)
wm_nom = inv_cov_matrix %*% e
wm_denom = t(e) %*% inv_cov_matrix %*% e
wm = wm_nom / wm_denom[1]
wm
```

Here is the zero-covariance portfolio $w_{z}$
```{r, echo = FALSE}
# Zero-covariance portfolio (W_z)
sigmainv_mu = inv_cov_matrix %*% exp_return
const = (t(e) %*% sigmainv_mu)[1] / wm_denom[1]
constsigma = const * wm_nom
wz = sigmainv_mu - constsigma
wz
```

\newpage
Combining $w_{z}$ and $w_{m}$, here are some optimal portfolios that is constructed from the several tau as selected below 
```{r, echo = FALSE}
# Optimal portfolio with varying tau
n_tau = length(tau)
w_opt <- as.data.frame(matrix(nrow = 10, ncol = n_tau))
for (i in 1:n_tau) {
  w_opt[i] <- tau[i] * wz + wm
}
colnames(w_opt) <- tau
rownames(w_opt) <- tickers
w_opt[1:10, c(1,2,3,6,11,15,19)] # Sample optimum weights with ranging tau
```

\newpage
# d. In-Sample Performance Evaluation
In this section, we want to evaluate the performance of our optimal weighting by backtesting it to our training set/prices.
```{r, echo = FALSE}
# Initializing vectors  
mu_port <- array()
sigma_port <- array()
mu_test <- array()
sigma_test <- array()

# Calculate the expected daily return and standard deviation of each portfolio
for (i in 1:n_tau) {
  w_mu = t(w_opt[i]) %*% exp_return
  mu_port[i] <- w_mu[1]
  w_sig = t(w_opt[i]) %*% cov_matrix
  w_sig_w = w_sig %*% as.matrix(w_opt[i])
  sigma_port[i] <- sqrt(w_sig_w[1])
  
  w_mu_test = t(w_opt[i]) %*% exp_testret
  mu_test[i] <- w_mu_test[1]
  w_sig_test = t(w_opt[i]) %*% cov_test %*% as.matrix(w_opt[i])
  sigma_test[i] <- sqrt(w_sig_test[1])
}
```

We have the S&P 500 as our market portfolio. The first few daily returns of the S&P 500, as well as their mean daily returns and standard deviations during training is as follows:
```{r, echo = FALSE}
# Mean returns and standard deviations of S&P 500
sp500 <- getSymbols("^GSPC", src = "yahoo", from = "2018-11-08", 
                     to = "2020-11-07", env = NULL, auto.assign = FALSE)
# Initialize training and testing sets
sp_train <- sp500[1:416,6]
sp_test <- sp500[417:(num+1), 6]
len_spt = length(sp_test)

# Put the returns on a dataframe. This is for the daily returns during training
sptreturn <- data.frame(matrix(nrow = 415, ncol = 2)) # non-NA dates
sptreturn[[2]] <- dailyReturn(sp_train)[c(2:length(sp_train))]
sptreturn[[1]] <- time(sp_train)[c(2:length(sp_train))]

# This is for the daily returns during testing
sptestret <- data.frame(matrix(nrow = len_spt, ncol = 2)) # non-NA dates
sptestret[2:len_spt, 2] <- dailyReturn(sp_test)[c(2:len_spt)]
sptestret[1, 2] <- (sp_test[1][[1]] - sp_train[416][[1]])/sp_train[416][[1]]
sptestret[[1]] <- time(sp_test)

spt_exp_ret <- mean(sptreturn[[2]])
spt_sigma <- sd(sptreturn[[2]])
spt_exp_test <- mean(sptestret[[2]])
spt_sigma_test <- sd(sptestret[[2]])

# Headings
colnames(sptreturn) <- c("Date", "S&P 500")
colnames(sptestret) <- c("Date", "S&P 500")

head(sptreturn)
sprintf("The mean return of the S&P 500 is %f", spt_exp_ret)
sprintf("The standard deviation of the S&P 500 strategy is %f", spt_sigma)
```

```{r, echo = FALSE}
# Mean returns and standard deviations of TSX
tsx <- getSymbols("^GSPTSE", src = "yahoo", from = "2018-11-08", 
                     to = "2020-11-07", env = NULL, auto.assign = FALSE)
ts_train <- tsx[1:417,6]
ts_test <- tsx[418:num, 6]
tsxreturn <- dailyReturn(ts_train)[c(2:length(ts_train))]
tsx_exp_ret <- mean(tsxreturn)
tsx_sigma <- sd(tsxreturn)
```


We also have our 1/N strategy (each stock given a weighting of 0.1) for the performance comparison. Below are the mean daily returns and daily standard deviations.
```{r, echo = FALSE}
# Mean returns and standard deviations of 1/N strategy
w_uniform <- rep(0.1, 10)
w_mu_unif = t(w_uniform) %*% exp_return
mu_unif <- w_mu_unif
w_sig_unif = t(w_uniform) %*% cov_matrix %*% as.matrix(w_uniform)
sigma_unif <- sqrt(w_sig_unif)
mu_unif_test = t(w_uniform) %*% exp_testret
w_sig_unif_t = t(w_uniform) %*% cov_test %*% as.matrix(w_uniform)
sigma_unif_test <- sqrt(w_sig_unif_t)

sprintf("The mean return of the 1/N strategy is %f", mu_unif)
sprintf("The standard deviation of the 1/N strategy is %f", sigma_unif)
```

Here is the mean and standard deviation of daily interest rate yield of the US 10-Year Treasury Bond, assuming that there are 250 trading days (for compounded daily rates) 
```{r, warning=FALSE, echo = FALSE}
# Mean returns and standard deviations of 10-Year US Treasury
rf <- getSymbols("^TNX", src = "yahoo", from = "2018-11-08", 
                     to = "2020-11-07", env = NULL, auto.assign = FALSE)
rf_train <- rf[1:503,6]
rf_train <- na.omit(rf_train)
rf_test <- rf[504:607,6]
rf_test <- na.omit(rf_test)
len_rft = length(rf_test)

rfreturn <- data.frame(matrix(nrow = 410, ncol = 2)) # non-NA dates
for (i in 1:410) {
  rfreturn[[2]][i] <- (1 + rf_train[i] / 100)^(1/246) - 1 # compounding daily
  # 410 days over 20 months -> 20.5 days per month -> 246 days per year
}
rfreturn[[1]] <- time(rf_train)[c(2:length(rf_train))]
rftestret <- data.frame(matrix(nrow = len_rft, ncol = 2)) # non-NA dates
for (i in 1:len_rft) {
  rftestret[[2]][i] <- (1 + rf_test[i] / 100)^(1/250) - 1 # we assume 250 trading days
}
rftestret[[1]] <- time(rf_test)

colnames(rfreturn) <- c("Date", "Risk-Free")
colnames(rftestret) <- c("Date", "Risk-Free")
head(rfreturn)

rf_exp_ret <- mean(rfreturn[[2]])
rf_sigma <- sd(rfreturn[[2]])
rf_exp_test <- mean(rftestret[[2]])
sprintf("The mean return of the 10-Year US Treasury is %f", rf_exp_ret)
sprintf("The standard deviation of the 10-Year US Treasury is %f", rf_sigma)
```


\newpage
Here is the plot of the efficient frontier made from optimal portfolios, as well as the market portfolio (S&P 500 and TSX), 1/N portfolio, and the min-risk portfolio.

```{r, echo = FALSE}
plot(sigma_port[1:15], mu_port[1:15], main = "Efficient Frontiers of Portfolios on Daily Returns",
     xlab = expression(sigma (daily)), ylab = expression(mu (daily)),
     xlim = range(0.01, 0.04), ylim = range(-0.001, 0.0075))
points(sigma_port[1], mu_port[1], type = "p", pch = 19, col = "red", lwd = 2)
points(spt_sigma, spt_exp_ret, type = "p", pch = 19, col = "green", lwd = 2)
points(tsx_sigma, tsx_exp_ret, type = "p", pch = 19, col = "blue", lwd = 2)
points(sigma_unif, mu_unif, type = "p", pch = 19, col = "pink", lwd = 2)
legend("topleft", legend = c("Minimum Risk Portfolio", "S&P 500 Expected Return",
                               "TSX Expected Return", "1/N Expected Return"),
       col = c("red", "green", "blue", "pink"), pch = c(19,19,19,19), cex = 0.8)
abline(0, 0, lty = 2)
```
As seen from the picture above, the optimum portfolios will form an efficient frontier that signifies the best expected daily return given the daily standard deviation. The market portfolios (S&P 500 and TSX) as well as the 1/N portfolio all lies below the efficient frontier, signifying that their expected daily returns are inefficient (not optimal) given their daily standard deviation (risk). The min-risk portfolio is located at the left-most part of the frontier, and since it has the lowest risk, it has a lower reward as well. However, we can see that even with this minimum risk, this portfolio still have a higher expected return compared to the market portfolio. This becomes a basis for us to explore deeper and test if our set of efficient portfolio can actually outperform the market. 


Here is the daily returns of our portfolios with ranging tau and 1/N strategy.
```{r, echo = FALSE}
tau_select <- c(1,2,3,6,11,15,19) # tau indexes that we will compare
comparison <- data.frame(matrix(ncol = 9, nrow = 7)) # this is for the comparison table
# Headings
rownames(comparison) <- c("Mean Returns (Daily)", "Standard Deviation (Daily)", 
                          "Cumulative Returns (20m)", "Sharpe Ratio", "Treynor Ratio", 
                          "Jensen's Alpha", "Beta")
colnames(comparison) <- c("Tau = 0", "Tau = 0.01", "Tau = 0.02", "Tau = 0.05", "Tau = 0.1",
                          "Tau = 0.2", "Tau = 0.5", "S&P 500", "1/N")

# Calculate Daily Returns
port_dailyret <- data.frame(matrix(nrow = 416, ncol = 10))
for (i in 1:8) {
  ret <- rep(0,416)  
  if (i == 8) {
    ret <- ret + daily_returns[[j+1]] * 0.1   # 1/N strategy
    port_dailyret[[i+2]] <- ret 
  } else {
    for (j in 1:10) { 
      ret <- ret + daily_returns[[j+1]] * w_opt[tau_select[i]][[1]][j]  # from weight
      port_dailyret[[i+1]] <- ret
    }
  }
}

# Headings
colnames(port_dailyret)[2:10] <- colnames(comparison)
colnames(port_dailyret)[1] <- "Date"

port_dailyret[[1]] <- train_set[[1]][c(2:no_train)]
# The S&P 500 Daily Returns have been posted above (in the S&P 500 section)
port_dailyret <- port_dailyret[-c(9)] 

head(port_dailyret)

```

We can compare the performance of these portfolios using some financial metrics as follows:
```{r, echo = FALSE}
# Calculating the performance evaluation metrics of the optimum portfolio 
colnames(port_dailyret) <- c("Tau = 0", "Tau = 0.01", "Tau = 0.02", "Tau = 0.05", "Tau = 0.1",
                          "Tau = 0.2", "Tau = 0.5")

for (i in 1:length(tau_select)) {
  comparison[[i]][1] <- mu_port[tau_select[i]]  # expected daily return
  comparison[[i]][2] <- sigma_port[tau_select[i]]  # daily standard deviation
  p_first <- 0
  p_last <- 0
  estim_a <- 0
  estim_b <- 0
  num_shares <- rep(0,10)
  for (j in 1:10) { # for first and last price (total cumulative return) & CAPM
    # we start with 10000 dollars, and we calculate no. of shares we can buy/short for each stock
    num_shares[j] <- 10000 * w_opt[tau_select[i]][[1]][j] / train_set[[j+1]][[1]] 
    # first price (day 1) and last price (day 416)
    p_first = p_first + num_shares[j] * train_set[[j+1]][[1]]
    p_last = p_last + num_shares[j] * train_set[[j+1]][[417]]
    # daily returns of each stock (t1), rf (t2), and market/s&p500 (t3)
    t1 <- daily_returns[c(1,j+1)]
    t2 <- rfreturn[1:2]
    t3 <- sptreturn[1:2]
    # since the dimensions are not the same (Canadian stocks vs US market vs Treasury Yields),
    # we consider only days that matches to get the correlations between the stock, rf, and market
    comp <- merge(t1, t3, by = "Date")
    compfull <- merge(comp, t2, by = "Date")
    stock_rf <- as.vector(compfull[2] - compfull[4])[[1]] # mu_stock - rf
    market_rf <- as.vector(compfull[3] - compfull[4])[[1]] # mu_market - rf
    estim <- lm(stock_rf ~ market_rf) # fit into linear regression (CAPM)
    estim_a <- estim_a + w_opt[tau_select[i]][[1]][j] * estim[[1]][[1]] # estimated alpha
    estim_b <- estim_b + w_opt[tau_select[i]][[1]][j] * estim[[1]][[2]] # estimated beta
  }
  # convert to annual rates, we assume 250 trading days in a year
  stdev_ann <- sigma_port[tau_select[i]] * sqrt(250)
  mu_ann <- (1 + mu_port[tau_select[i]])^250 - 1
  rf_ann <- (1 + rf_exp_ret)^250 - 1
  comparison[[i]][3] <- (p_last - p_first)/p_first   # cumulative return (total growth)
  comparison[[i]][4] <- (mu_ann - rf_ann)/stdev_ann  # sharpe ratio
  comparison[[i]][5] <- (mu_ann - rf_ann)/estim_b    # treynor ratio
  comparison[[i]][6] <- estim_a                      # jensen's alpha
  comparison[[i]][7] <- estim_b                      # beta
}
```


```{r, echo = FALSE}
# metrics for S&P 500 performance
comparison[[8]][1] <- spt_exp_ret
comparison[[8]][2] <- spt_sigma
comparison[[8]][3] <- (sp_train[416][[1]] - sp_train[1][[1]])/sp_train[1][[1]] # cumulative growth
# Annualize rates
stdev_spt_ann <- spt_sigma * sqrt(250)
mu_spt_ann <- (1 + spt_exp_ret)^250 - 1
comparison[[8]][4] <- (mu_spt_ann - rf_ann)/stdev_spt_ann
comparison[[8]][5] <- (mu_spt_ann- rf_ann) # beta = 1 since it is the market portfolio
comparison[[8]][6] <- 0
comparison[[8]][7] <- 1
```

```{r, echo = FALSE}
# metrics for 1/N strategy, similar as before
comparison[[9]][1] <- mu_unif
comparison[[9]][2] <- sigma_unif
unif_p_first <- 0
unif_p_last <- 0
unif_shares <- rep(0,10)
for (i in 1:10) {
  # assume we have 10000 dollars at first, calculate how much shares we can buy per stock
  unif_shares[i] <- 10000 * 0.1 / train_set[[i+1]][[1]] 
  # for first and last daily returns
  unif_p_first = unif_p_first + unif_shares[i] * train_set[[i+1]][[1]]
  unif_p_last = unif_p_last + unif_shares[i] * train_set[[i+1]][[417]]
}
comparison[[9]][3] <- (unif_p_last - unif_p_first)/unif_p_first
port_unif <- array()
estim_unif_a <- 0
estim_unif_b <- 0
for (i in 1:7) {
  ret <- rep(0,416)
  for (j in 1:10) {
    ret <- ret + daily_returns[[j+1]] * 0.1
  }
  port_unif <- ret
}

for(j in 1:10) {
  # finding beta of the portfolio
  t1 <- daily_returns[c(1,j+1)]
  t2 <- rfreturn[1:2]
  t3 <- sptreturn[1:2]
  # merge data with same trading days
  comp <- merge(t1, t3, by = "Date")
  compfull <- merge(comp, t2, by = "Date")
  stock_rf <- as.vector(compfull[2] - compfull[4])[[1]] 
  market_rf <- as.vector(compfull[3] - compfull[4])[[1]]
  estim <- lm(stock_rf ~ market_rf) # fit linear regression (CAPM)
  estim_unif_a <- estim_unif_a + 0.1 * estim[[1]][[1]]
  estim_unif_b <- estim_unif_b + 0.1 * estim[[1]][[2]]
}
# convert to annual rates
stdev_unif_ann <- sigma_unif * sqrt(250)
mu_unif_ann <- (1 + mu_unif)^250 - 1
comparison[[9]][4] <- (mu_unif_ann - rf_ann)/stdev_unif_ann
comparison[[9]][5] <- (mu_unif_ann - rf_ann)/estim_unif_b
comparison[[9]][6] <- estim_unif_a
comparison[[9]][7] <- estim_unif_b
comparison
```

From the above performance comparison, we can see that the optimal portfolio outperforms the market portfolio during the in-sample (training) period. This can be clearly seen from the mean daily returns and cumulative returns. The min-risk portfolio can generate higher expected return and cumulative return than the market, while still maintaining lower risk (low standard deviation and beta) compared to the market. We can also see an increasing trend in expected return as we increase the tau (trade-off parameter). 
\newline
One of the optimal portfolios that "wins" in all metrics against the market and 1/N portfolio is the portfolio with $\tau = 0.05$. Aside from having higher returns and lower risk (lower $\sigma$ and $\beta$), it also has a higher Sharpe Ratio and Treynor Ratio. This means that with every increase in $\sigma$ and $\beta$ of the portfolio, the portfolio is able to return higher net yield (expected return - risk-free) compared to the market and 1/N portfolios. Additionally, it also has a daily alpha "advantage" of 0.002, suggesting that it outperforms the required rate of return. 
\newline
Looking back into the portfolio weighting, we have that the min-risk portfolio invests pretty heavily in more "conservative" stocks such as Canadian Rail (TSX: CNR) and TELUS (TSX: T), where these stocks have relatively low beta. This makes sense since the nature of these businesses are more "essential" and unaffected by cyclicalties that might happen in the market. People will still need communication providers like TELUS regardless of how the market performs. Similarly, Canadian Rail has a huge presence in rail transportation, and companies will still need them anyway. They are more prone to swings in the market since they expect revenues to be relatively stable from time to time, and that there are usually less breakthroughs announced compared with growth companies. 
\newline
On the other hand, the zero-covariance portfolio $w_{z}$ suggests that we should push for growth stocks such as Brookfield Renewable Partners (TSX:BEP-UN), Shopify (TSX: SHOP), and Constellation Software (TSX: CSU). These companies are in their growing years and are expected to grow their enterprise value by several times before reaching peak. Moreover, with these companies being in the technology and renewable energy sectors, they are competing in the "niche" and futuristic industry, where their presence can be really felt in the years ahead, but requires a massive amount of upfront costs/capital before breaking even in the future. In fact, after the March 2020 crash, these sectors are among the fastest to rebound due to sudden fluctuations in daily technology usage that is not normally as high. In contrast, this portfolio also instructed us to short the "laggards" such as Enbridge (TSX:ENB) and Manulife (TSX:MFC). The recent oil price slump in late April 2020 and COVID crash during March 2020 have wiped the market values of Canadian companies in the energy and financial sector.
\newline
Aside from the cyclicalities shown in different sectors in response to the COVID crash and rebound, it is worth noting that from April to July and beyond, the interest rate (US 10 Year Yields) has fallen to record low levels in decades. This is due to efforts by the Feds to lower interest rates in order to re-ignite the economy, as well as more investors trying to find save investments during the current volatile times. As the result, the Sharpe Ratio and Treynor Ratio may seem a bit higher than usual since the stocks rebound (increases) faster than the decreasing interest rate.

\newpage
# e. Out-of-Sample Performance Evaluation
In this section, we will compare the performance of the portfolios on the testing set (July - Novemeber 2020). The first 6 daily returns of the portfolios are as follows: 

```{r, echo = FALSE}
testing <- data.frame(matrix(ncol = 9, nrow = 7)) # will be used to compare the metrics
# Headings
rownames(testing) <- c("Mean Returns (Daily)", "Standard Deviation (Daily)", 
                       "Cumulative Returns (4m)", "Sharpe Ratio", "Treynor Ratio", 
                       "Jensen's Alpha", "Beta")
colnames(testing) <- c("Tau = 0", "Tau = 0.01", "Tau = 0.02", "Tau = 0.05", "Tau = 0.1",
                       "Tau = 0.2", "Tau = 0.5", "S&P 500", "1/N")
len_tst = nrow(test_set)

# Calculate daily returns on test set
port_testret <- data.frame(matrix(nrow = len_tst, ncol = 7))
for (i in 1:8) {
  ret <- rep(0,len_tst)  
  if (i == 8) {
    ret <- ret + test_returns[[j+1]] * 0.1   # 1/N strategy
    port_testret[[i+2]] <- ret 
  } else {
    for (j in 1:10) { 
      ret <- ret + test_returns[[j+1]] * w_opt[tau_select[i]][[1]][j]  # from weight
      port_testret[[i+1]] <- ret
    }
  }
}

# Headings
colnames(port_testret)[2:10] <- colnames(testing)
colnames(port_testret)[1] <- "Date"
port_testret[[1]] <- test_set[[1]]
port_testret <- port_testret[-c(9)] # The S&P 500 return is presented separately below
head(port_testret)
```
Here are the first 6 daily returns of S&P 500 on the testing set
```{r, echo = FALSE}
head(sptestret) # called from the S&P 500 section in d.
```

We can compare the performance of these portfolios using some financial metrics as follows:
```{r, echo = FALSE}
# same comparison table concept as in d.
for (i in 1:length(tau_select)) {
  testing[[i]][1] <- mu_test[tau_select[i]]     # mean daily returns
  testing[[i]][2] <- sigma_test[tau_select[i]]  # daily standard deviations
  p_first <- 0
  p_last <- 0
  estim_a <- 0
  estim_b <- 0
  num_shares <- rep(0,10)
  for (j in 1:10) { # for first and last price (total cumulative return) & capm
    # we start with 10000 dollars
    num_shares[j] <- 10000 * w_opt[tau_select[i]][[1]][j] / test_set[[j+1]][[1]] 
    # first and last day returns
    p_first = p_first + num_shares[j] * test_set[[j+1]][[1]]
    p_last = p_last + num_shares[j] * test_set[[j+1]][[len_tst]]
    # comparing daily returns for beta, merging data that have same trading days
    t1 <- test_returns[c(1,j+1)]
    t2 <- rftestret[1:2]
    t3 <- sptestret[1:2]
    comp <- merge(t1, t3, by = "Date")
    compfull <- merge(comp, t2, by = "Date")
    stock_rf <- as.vector(compfull[2] - compfull[4])[[1]] 
    market_rf <- as.vector(compfull[3] - compfull[4])[[1]]
    estim <- lm(stock_rf ~ market_rf)    # fit into linear regression
    estim_a <- estim_a + w_opt[tau_select[i]][[1]][j] * estim[[1]][[1]]
    estim_b <- estim_b + w_opt[tau_select[i]][[1]][j] * estim[[1]][[2]]
  }
  # convert to annual rates, we assume 250 trading days in a year
  stdev_ann <- sigma_test[tau_select[i]] * sqrt(250)
  mu_ann <- (1 + mu_test[tau_select[i]])^250 - 1
  rf_ann <- (1 + rf_exp_test)^250 - 1
  testing[[i]][3] <- (p_last - p_first)/p_first     #  cumulative return (4 months)
  testing[[i]][4] <- (mu_ann - rf_ann)/stdev_ann    #  sharpe ratio
  testing[[i]][5] <- (mu_ann - rf_ann)/estim_b      #  treynor ratio
  testing[[i]][6] <- estim_a                        #  jensen's alpha
  testing[[i]][7] <- estim_b                        #  beta
}
```

```{r, echo = FALSE}
# calculate metrics for market portfolio
testing[[8]][1] <- spt_exp_test
testing[[8]][2] <- spt_sigma_test
testing[[8]][3] <- (sp_test[len_spt][[1]] - sp_test[1][[1]])/sp_test[1][[1]]
stdev_spt_ann_t <- spt_sigma_test * sqrt(250)
mu_spt_ann_t <- (1 + spt_exp_test)^250 - 1
testing[[8]][4] <- (mu_spt_ann_t - rf_ann)/stdev_spt_ann_t
testing[[8]][5] <- (mu_spt_ann_t - rf_ann) # beta = 1
testing[[8]][6] <- 0
testing[[8]][7] <- 1
```

```{r, echo = FALSE}
# calculate metrics for 1/N portfolio
testing[[9]][1] <- mu_unif_test
testing[[9]][2] <- sigma_unif_test
unif_p_first <- 0
unif_p_last <- 0
unif_shares <- rep(0,10)
for (i in 1:10) {
  unif_shares[i] <- 10000 * 0.1 / test_set[[i+1]][[1]] 
  unif_p_first = unif_p_first + unif_shares[i] * test_set[[i+1]][[1]]
  unif_p_last = unif_p_last + unif_shares[i] * test_set[[i+1]][[len_tst]]
}
testing[[9]][3] <- (unif_p_last - unif_p_first)/unif_p_first
estim_unif_a <- 0
estim_unif_b <- 0

for(j in 1:10) {
  t1 <- test_returns[c(1,j+1)]
  t2 <- rftestret[1:2]
  t3 <- sptestret[1:2]
  comp <- merge(t1, t3, by = "Date")
  compfull <- merge(comp, t2, by = "Date")
  stock_rf <- as.vector(compfull[2] - compfull[4])[[1]] 
  market_rf <- as.vector(compfull[3] - compfull[4])[[1]]
  estim <- lm(stock_rf ~ market_rf)
  estim_unif_a <- estim_unif_a + 0.1 * estim[[1]][[1]]
  estim_unif_b <- estim_unif_b + 0.1 * estim[[1]][[2]]
}
stdev_unif_ann_t <- sigma_unif_test * sqrt(250)
mu_unif_ann_t <- (1 + mu_unif_test)^250 - 1
testing[[9]][4] <- (mu_unif_ann_t - rf_ann)/stdev_unif_ann_t
testing[[9]][5] <- (mu_unif_ann_t - rf_ann)/estim_unif_b
testing[[9]][6] <- estim_unif_a
testing[[9]][7] <- estim_unif_b
testing
```

Price comparison between first day and last day of testing set
```{r, echo = FALSE}
x <- test_set[c(1,85),1:11]
x
```

As seen from the above tables, our efficient portfolio can still outperform the market portfolio in the testing (out-of-sample) period. This can be seen from the returns (daily mean and cumulative), risk (standard deviation and beta), and risk-reward comparisons (Sharpe, Treynor, Jensen). In general, the optimum portfolios have maintained lower risk while reaching better return compared to the broad market index. One of the best optimum portfolios is the one with $\tau = 0.05$. With a relatively small standard deviation and beta (as compared to 1/N strategy), it is able to return 15.2% within the last 4 months. Our most aggressive strategy ($\tau = 0.5$) can yield up to 94% in expected return in just 4 months.
\newline
The 1/N strategy performed worse than the market portfolio, mainly due to the portfolio not being able to "short" or not invest in some of the stocks. This causes the high return stock to be netted down by the negative return stock that is invested with the same proportion. Another thing to note here is that our efficient portfolio return is actually very dependent on only some stocks. If we see the table that compares between the first and last day, we can see that some of the expectedly "good" stocks such as Shopify and Constellation Software went down, mainly due to the tech sell-off in September 2020. Conversely, stocks that have been shorted such as Manulife and Canadian Rail actually went up, further diminishing the return. However, the trade that really lift up the return is by leveraging on BEP and shorting on ENB. The portfolio leverages BEP by 50 to 260%, and fortunately the stock did actually went up by almost 50% in the last 4 months, mostly due to the recent US election news, where investors are expecting the future government to use renewable energy services by BEP. On the other side of the correlation, oil and gas stocks like Enbridge are predicted to face a gloomy future. Overall, while the trade has generated a huge amount of return in just 4 months, it is worth noting that it is still relatively risky an may advanced models such as machine learning and preddictive models might be needed to better understand other factors that can contribute to stock prices.
\newline
Visualized below is the sharp contrast between BEP-UN and ENB during the testing period. The optimum portfolio would be able to capitalize twice by leveraging on BEP-UN while shorting on ENB. On the other hand, 1/N strategy is forced to invest equally in both stocks.


```{r, echo = FALSE}
par(mfrow = c(2,1))
plot(test_set$"BEP-UN.TO")
plot(test_set$ENB.TO)
```


