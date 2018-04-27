# Google Queries for Nowcasting New Housing Sales
Replicate the results of nowcasting housing sales by Google Queries, using Bayesian Structural Time-Series Model (Choi &amp; Varian, 2009, 2012). 

#### References
* Scott, S. L., & Varian, H. R. (2014). ["Predicting the present with bayesian structural time series"](https://www.inderscienceonline.com/doi/abs/10.1504/IJMMNO.2014.059942). International Journal of Mathematical Modelling and Numerical Optimisation, 5(1-2), 4-23.
* Scott, S. L., & Varian, H. R. (2015). ["Bayesian variable selection for nowcasting economic time series"](http://www.nber.org/chapters/c12995). In Economic analysis of the digital economy (pp. 119-135). University of Chicago Press.
* **[More intuitive]** Varian, H. R. (2014). ["Big data: New tricks for econometrics"](https://www.aeaweb.org/articles?id=10.1257/jep.28.2.3). Journal of Economic Perspectives, 28(2), 3-28.

**Nowcasting** - The needs of timely estimating current values (*Housing Sales*), which are usually available with publication lags motivates to use the **Google Queries** (nearly real-time (as potential predictors). By [**Google Correlate**](https://www.google.com/trends/correlate), we can derive the hundred of google "keywords" searching most correlated with our target time-series (Housing Sales).


## Bayesian Structural Time Series (BSTS) method
This decompose the target time series into different components: **i) Time Components (Trend, Seasonality, etc.)**; **ii) Regression Component (Google Predictors)**

1. Structural Time-series model (Kalman Filter) for time components
2. Spike-and-Slab Regression for regression components
3. Markov Cahin Monte Carlo Simulation 

This method enables us to decompose the time-series and analyse the contribution of each components to the target time-series
![alt text](https://github.com/maianhdang/nowcasting_google_queries/blob/master/graphs/decompose.png)

Incremental Fit Plot of Housing Sales, by adding respectively:
* Trend
* Seasonality
* First and Second Important Google Keywords


![alt text](https://github.com/maianhdang/nowcasting_google_queries/blob/master/graphs/nowcast_google.gif)

## High-dimentsional Google Queries
One should bear in mind the nature of this data is high-dimensional. Not all google queries are meaningful predictors. We need a mechanism for variables selections, and *Spike-and-Slab* approach is used. Predictors with high inclusion probabiliries are more important. 
![alt text](https://github.com/maianhdang/nowcasting_google_queries/blob/master/graphs/inclusion_probabilities.png)
