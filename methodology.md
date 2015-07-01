Methodology
===========

All predictions where performed in R using support vector machine from package
*e1071* as learner. For each time period, a separate model was tuned.
Selection of the optimal model (the SVM hyper-parameters and the selected
windows of data) was performed  by grammatical evolution (using R package
*gramEvol*) to minimize  cross-validation error on the each of the time-series
periods.

The first and last time-series were only used for validation of this technique
against linear approximation.

## Supply temperature setpoint

The setpoint was discretized to several bins, and a classifier SVM was trained
to predict each setpoint based on other 3 time-series.

As a visual inspection of data determined behavior changes around 28th of
February, two models were trained for before and after 2014/02/28.

## System supply temperature

SVM regression was used to predict the first difference (i.e., changes) of
the temperature using the other 3 time-series. The first differences were
accumulated forwards and backwards, and then averaged to find the true value
of temperature.

## Return temperature

A similar technique to System supply temperature was used.

## System power

A similar technique to Supply temperature setpoint was used, although using 
only one model.

## All 4 time series

For each series, a single-step-ahead time-series forecaster was trained based
on the previous observed values of all time-series. Both classification and
regression were used, as described for individual series.

The forecasting was performed both forwards (as in usual time-series
forecasting) and backwards (reverse in time). The resulting predictions were
averaged to find the true values.

The daylight saving time was handled separately, using linear interpolation
and added later.


Changes from original submission
================================
Only a "round" function was applied on the "Supply Temperature Setpoint" for
the 6th period (all data missing at once). This may introduce a minor rounding
error when recomputing the competition score. 
To compute the exact value as used in the original submission, remove the 
round function from prediction/pred-5.R line 183.


Reproducibility 
===============
During the training process, the random number
generator is seeded with a constant number. As a result, it is expected that
the same models are selected regardless of the execution environment.
