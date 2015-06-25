[![Circle CI](https://circleci.com/gh/roqua/autovarCore.png?style=shield&circle-token=6934b57a8e350e98f46f7798cf9347dce3c0d74f)](https://circleci.com/gh/roqua/autovarCore)

AutovarCore
===========

AutovarCore generates networks given a data frame. AutovarCore is a simplified/efficient version of [Autovar](https://github.com/roqua/autovar).

To install, type the following:

    install.packages('devtools')
    devtools::install_github('roqua/autovarCore')

For more information on Autovar, see [autovar.nl](https://autovar.nl).


#### Example use

    library('autovarCore')
    
    # AutovarCore requires input data in data.frame format.
    # If you have data in a .csv, .dta, or .sav file, use
    # the 'foreign' library to load this data into R first.
    # (You may need to type:
    #    install.packages('foreign')
    #  if you do not have the foreign library installed on
    #  your system.)
    library('foreign')
    
    # This example data set can be found on
    # https://autovar.nl/datasets/aug_pp5_da.sav
    suppressWarnings(dfile <- read.spss('~/Downloads/aug_pp5_da.sav'))
    dframe <- data.frame(Activity = dfile$Activity, Depression = dfile$Depression)
    
    # Call autovar with the given data frame. Type:
    #   ?autovar
    # (after having typed "library('autovarCore')") to see 
    # which other options are available.
    models_found <- autovar(dframe, selected_column_names = c('Activity', 'Depression'))
    
    # Show details for the best model found
    print(models_found[[1]])


#### Should I use Autovar or AutovarCore?

You should use Autovar if you

* Prefer a slightly better model fit over a model with less outlier dummies (less outlier dummies means that the model explains more of the measurements).
* Are okay with Autovar sometimes returning NULL because it could not find any models that passed all residual tests.
* Need VAR models with more than one lag or with zero lags.
* Need models with automatically determined restrictions.
* Need debugging information such as a full list of all evaluated models.
* Want detailed summary information such as a plot of contemporaneous correlations or Granger causalities.
* Need named dummy variables for interpretation (e.g., "morning", "afternoon", "Monday", "Tuesday" instead of "day\_part\_1", "day\_part\_1", "day\_3", "day\_4")

You should use AutovarCore if you

* Prefer a model with less outlier dummies over a model with a slightly better model fit (less outlier dummies means that the model explains more of the measurements).
* Always want a list of best models even if those do not pass all residual tests at the default p-level (this is indicated by the 'bucket' property, see ?autovar for details).
* Are not interested in any models except for models with lag 1 and models with lag 2 where the second lag is autoregressive only.
* May have missing data (i.e., NA values). Autovar also has a function "impute\_dataframe" to impute values, but AutovarCore does this automatically (if needed).
* Need more flexibility as to which residual tests should constitute model validity (e.g., portmanteau, portmanteau\_squared, skewness, kurtosis, joint_sktest). Autovar uses a fixed set of residual tests.
* Deem performance to be an issue and prefer memory-efficient and fast code.
