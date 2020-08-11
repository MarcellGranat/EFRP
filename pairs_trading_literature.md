Pairs Trading Literature

# Pairs trading with partial cointegration (Clegg, 2017)

*There is reason to believe that cointegration may not be the most appropriate model for pairs trading.* *Companies often experience idiosyncratic shocks that are permanent in nature.* (The reason can be technological change, development of new markets, legal action etc.) Clegg (2014) shows that poverty of being cointegrated is not persistent. This paper introduce the concept of `partial cointegration (PCI)`.

PCI allows the residuals to have a mean-reverting and random walk components. For this modelling there is an R package called [partialAR](https://cran.r-project.org/web/packages/partialAR/index.html). *A key statistic of a PAR model is the proportion of variance attributable to mean-reversion.* The system is identifiable, the estimation is based on *maximum likelihood estimation of the associated [Kalman filter](https://hu.wikipedia.org/wiki/K%C3%A1lm%C3%A1n-sz%C5%B1r%C5%91).* 

The paper contains a simulative and an empirical analysis. As a benchmark distance method and two cointegration-based strategies (CI1 is testing with ADF and Johansen, while CI2 is based on Phillips-Perron-test (see Clegg, 2014)) are performed. For CI1, CI2 and CPI the best pairs to trade are choosen based on Sharpe ratio.

The conclusion from the simulative analysis is that PCI outperforms CI, but the advantage disappers if the proportion of variance attributable to mean-reversion or the AR(1)-coefficient is high. Testing for optimal opening and closing threshold also commited.

Evaluating the performance shows that **PCI outperforms the other methods**, (suggested R package: `PerformanceAnalytics`) its skewness and kurtosis are higher. *Fama-French three-factor model* is also performed. Subperiod analysis shows that the strategy performed significantly better at the time of a crisis, but in **recent years its profitability started to decline (assumed that the reason is its growing popularity)**.

The authors suggest to use PCI in other economic contexts as a possible further research (other financial markets or macroeconomic). 

- Refer to half-life of mean-reversion

- Securities splitted into sectors defined by [Global Industry Classification Standard](https://www.msci.com/gics)

# Are pairs trading profits robust to trading costs? (Do, 2012)

Authors examine the impact of trading costs on pairs trading profitability. These cost are: **commissions, market impact, short selling fees**. After contorlling for these, pairs trading remains profitable.

