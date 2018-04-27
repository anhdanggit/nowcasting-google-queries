library("bsts")
library("lubridate")
library("ggplot2")
library("reshape2")
library("dyn")
library("glmnet")

## (1) Load data ####

# Reading the edited data (all spurious queries were dropped)
data_housing <- read.csv("./data/econ-HSN1FNSA.csv")

# Setting dependent variable
y <- zoo(data_housing[,2],as.Date(data_housing[,1]))
y.name <- "Housing price"


## (2) BSTS ####

# Setting predictors
x <- data_housing[,3:ncol(data_housing)]

# Setting parameters
numiter <- 4000
size <- 5

# Setting trend and seasonal components
ss <- AddLocalLinearTrend(list(), y)
ss <- AddSeasonal(ss, y, nseasons=12)

### Model estimation
bsts.model <- bsts(y~., state.specification=ss, data=x, niter=numiter,
              expected.model.size=size, ping=200, seed=123)


## (3) Results of BSTS: Inclusive Prob ####

# Posterior distribution and actual outcome.
plot(bsts.model, main = "Fitted vs Actual values")

# Obtain suggested burn
burn <- SuggestBurn(0.1, bsts.model)

# Inclusion Probabilities Plot
inclusionprobs <- melt(colMeans(bsts.model$coefficients[-(1:burn),] != 0))
inclusionprobs$Variable <- as.character(row.names(inclusionprobs))
ggplot(data=inclusionprobs, aes(x=Variable, y=value)) + 
  geom_bar(stat="identity", position="identity") + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
  xlab("") + ylab("") + ggtitle("Inclusion probabilities")

# Probability of inclusion of top predictors (p > .15)
plot(bsts.model, "coef", inc=.15, main = "Top selected predictors")

# Contribution of trend, seasonal and regression components.
plot(bsts.model, "comp")



## (4) Decompose into components ####

# Extract the components
components <- cbind.data.frame(
  colMeans(bsts.model$state.contributions[-(1:burn),"trend",]),                               
  colMeans(bsts.model$state.contributions[-(1:burn),"seasonal.12.1",]),
  colMeans(bsts.model$state.contributions[-(1:burn),"regression",]),
  as.Date(time(y)))  
names(components) <- c("Trend", "Seasonality", "Regression", "Date")
components <- melt(components, id="Date")
names(components) <- c("Date", "Component", "Value")


# Components Contribution Plot
ggplot(data=components, aes(x=Date, y=Value)) + geom_line() + 
  theme_bw() + theme(legend.title = element_blank()) + ylab("") + xlab("") + 
  facet_grid(Component ~ ., scales="free") + guides(colour=FALSE) + 
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


## (5) Incremental Fit Plot ####

# incremental plots
beta.top.7 <- summary(bsts.model)$coef[1:7,1]  # top 7 nonzero coef
state <- bsts.model$state.contributions
time <- length(y)

# get the trend at time t, by averaging all MCMC simulated result for time t
trend <- rep(NA,time)
for (t in 1:time) {
  trend[t] <- mean(state[,1,t])}

# regression component: beta*x
reg.comp <- as.matrix(x[,names(beta.top.7)])%*%diag(beta.top.7) 

# seasonality
seasonal <- rep(NA,time)
for (t in 1:time) {
  seasonal[t] <- mean(state[,2,t])}

df.plot <- data.frame(y,trend,seasonal, reg.comp)
names(df.plot) <- c("y","trend","seasonal",names(beta.top.7))

source("plot_incremental_fit.R") # loading codes for incremental plot (Copyright 2012 Google Inc.)

nplots <- 2:6 # trend, seasonal, + top 3 predictors
PlotIncrementalFit(df.plot,response.colname="y",component.colnames=names(df.plot)[nplots])


## (6) Compare BSTS: Out-of-sample-forecasts ####

source("oosf.R") # loading codes for out-of-sample-forecasts, (Copyright 2014 Google Inc.)

# Choose top predictors (based on Inclusion Probabilities)
x1 <- zoo(x[,cbind("appreciation.rate","irs.1031","century.21.realtors",
                   "real.estate.purchase")],as.Date(data_housing[,1]))
reg1 <- OutOfSampleForecast12(y,x1,k=24)

# mae.delta is the ratio of the trends MAE to the base MAE
MaeReport(reg1)


## (7) Compare Ridge, LASSO and Elastic Net ####

x <- as.matrix(x)
lasso <- cv.glmnet(x, y, alpha = 1, nlambda = 80)
ridge <- cv.glmnet(x, y, alpha = 0, nlambda = 80)
elasticnet <- cv.glmnet(x, y)

# Selected Variables
coef(lasso, s = "lambda.min")
coef(ridge, s = "lambda.min")
coef(elasticnet, s = "lambda.min")