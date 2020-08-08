# Cointegration Literature

## Pairs trading with partial cointegration (Clegg, 2017)

`DOI: 10.1080/14697688.2017.1370122`

*There is reason to believe that cointegration may not be the most appropriate model for pairs trading.* *Companies often experience idiosyncratic shocks that are permanent in nature.* (The reason can be technological change, development of new markets, legal action etc.) Clegg (2014) shows that poverty of being cointegrated is not persistent. This paper introduce the concept of `partial cointegration (PCI)`.

PCI allows the residuals to have a mean-reverting and random walk components. For this modelling there is an R package called [partialAR](https://cran.r-project.org/web/packages/partialAR/index.html). *A key statistic of a PAR model is the proportion of variance attributable to mean-reversion.* The system is identifiable, the estimation is based on *maximum likelihood estimation of the associated [Kalman filter](https://hu.wikipedia.org/wiki/K%C3%A1lm%C3%A1n-sz%C5%B1r%C5%91).*  



###### Interesting features

- Refer to half-life of mean-reversion
- Securities splitted into sectors defined by [Global Industry Classification Standard](https://www.msci.com/gics)
-

