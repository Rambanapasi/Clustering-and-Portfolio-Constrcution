FMX Project
================

# Goal

Model an *edge* that serves a pre-requiste to structuring trades. I want
to be able to quickly identifying charcateristics in groups of stock
given some factors.

# Clustering to Create Smart Portfolio

## Roadmap

I want to use a combination of clustering and factor analysis.
Essentially, I want to reduce the dimensions of our sample data, making
it easier to uncover latent relationships in asset performance over
various time periods. For this, I will use K means clustering, as it
presents for an interesting technique to reduce dimensions and cluster
stock into groups with similar characteristics. Yes, the major
disadvantage is that it is suspectible to produce unstable clusters but
this is a feature that makes it interesting, the modeler can produce
unique results thus providing an *edge*.

## Metrics used

To assess performance, first I will use a simple look-back period to
observe performance and risk over 3, 6, and 12 months. Subsequently, we
conduct a rolling backtest over the sample period to evaluate the
robustness of the cluster characteristics formed through our factor
filter.

## Proposed Data & Methodology

- use historical stock price data sourced from Bloomberg, transform to
  monthly data by considering EOM observations.

- perform additional transformations on historical prices to obtain
  momentum and volatility measures. That is, filter top 40 stock by 200D
  moving average. Following this rank stock by 12 month return (momentum
  ), 100D trailing SD. (For this study we dont include fundmental data,
  due to the effort of cleaning data, which is beyond the scope of this
  project but could be explored further for more meaningful results).
  Apply a percentile scoring relative to the factors.

- Apply K-means. (clusters are formed on a euclidean distance, how then
  do you interpret the characteristics of the clusters)

- Filter each clusters stocks by the top n stock (this depends on the
  size of the factors) relative momentum values).

- Conduct a return and risk attribution to each clusters by back
  testing. Re-balancing quarterly, this would be my out sample testing

# Lets Start

# Retrieving the data

# Cleaning the data

# Prepping data for the K- Means Clustering

# Cluster constituent review

I want to get an idea of cluster constituent characteristics, that way I
can better describe aggregate charcateristics.

- in tabular format, get the industries, annualized return & risk,
  together with the range. This will give an idea of portfolio
  characteristics.

# Backtesting

From the constituent review, I noticed through the rolling return and
risk metrics there was some consistency amongst constituents in
clusters. This gives some encouragement that constituents have some
commonalities in their risk/return profile over 12 months. Therefore
carrying on this theme, I will filter out the volatile stock to get an
idea of the clusters, and see how long these factors work or not.

## Return Attribution

In hindsight, how well did the clusters perform, relative to the JSE all
share?

- give this information we can charcaterize our clusters and construct
  cluster portfolios to study aggregate charcateristics.

# Rolling Backtest

- take the clusters formed the backward looking analysis and test them
  on a series investment horizons. How long this strategy works and why,
  but the why bit links to the constituents.

# Please note

- results on the readme and write may be different. clusters formed
  sometimes include other industries and assets despite my best efforts
  to hard set certain inputs.

Apart from that hope you enjoyed the read.
