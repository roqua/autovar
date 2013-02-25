Autovar
=======

Autovar is an R package for automating and simplifying the process from raw SPSS or STATA data to VAR models. For the actual VAR calculations, Bernhard Pfaff's vars package is used.


Selecting data
--------------

Before calling any other function that takes an `av_state` argument, the function `load_file` needs to be called. It returns an `av_state` object that is used by the other functions. The `av_state` object supports the print command (`print(av_state)`).


### load_file

    av_state <- load_file(filename,file_type = c('SPSS','STATA'))
    
**In the web application, this function does not need to be called explicitly. It is always prepended to the code. Use the variable `d` to refer to the av_state for the selected data set.**

This function prints the columns of the loaded data set. The abbreviation `(scl)` is used to denote scale (numeric) columns, and `(nom)` is used to denote nominal (factor) columns. The function returns an object of class `av_state`. Objects of this class are used throughout this package to store VAR data sets, models, and results.

#### Arguments

The `file_type` argument is optional. When not specified, it is determined from the `filename`, i.e., `.dta` extensions are treated as STATA files and `.sav` extensions are treated as SPSS files.

#### Results

This function creates the following variables in the `av_state` list:

* `file_name` - The full file name including path. The working directory is prepended if a partial path is supplied.
* `file_type` - The type of the file. Can be `SPSS` or `STATA`. Used for determining how to read the file.
* `raw_data` - The raw file data as it is read, before any added columns, imputations, sorting, or splitting.
* `data` - The current data set. Initially, `data` is a list containing a single item, such that `raw_data` is identical to `data[['multiple']]`.

#### Syntax

Example: `av_state <- load_file("../data/input/RuwedataAngela.sav")`


### group_by

    av_state <- group_by(av_state,id_field)

The `group_by` function splits up the initial data set into multiple sets based on their value for `id_field`.

For example, if we have a data set with a field named `id`  that has values ranging from 11 to 15, calling `av_state <- group_by(av_state,'id')` will set `av_state$data` to a list of five items. This list is ordered by the value of the id field. Then, we can use `av_state$data[[1]]` (or equivalently, `av_state$data[['11']]`) to retrieve the rows in the data set that have `id` 11. Likewise, use `av_state$data[[2]]` or `av_state$data[['12']]` for rows with `id` 12.

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

#### Results

Other than adjusting `av_state$data`, the `group_by` function creates the following variables in the `av_state` list:

* `group_by` - the `id_field` used for grouping the data.


### order_by

    av_state <- order_by(av_state,id_field,impute_method=c('BEST_FIT','ONE_MISSING','ADD_MISSING','NONE'))

The `order_by` function determines the order of the data rows in the data set. For vector autoregression, you may want to use this to make sure that the data set is sorted by the date/time column. The supplied `id_field` parameter is often a measurement index (e.g., `'tijdstip'`). The `id_field` column has to be numeric.

#### Arguments

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

The `impute_method` argument has three possible values:

* `BEST_FIT` - This is not an impute method itself, but tells the function to determine the optimal impute method and use that. This is the default choice for `impute_method` when it is not specified.
* `ONE_MISSING` - Only works when the `id_field` in each data subset is an integer range with exactly one value missing and exactly one `NA` value. The `NA` value is then substituted by the missing index.
* `ADD_MISSING` - Does not work when one or more rows have an `NA` value for `id_field`. Only works for integer ranges of `id_field` with single increments. Works by adding rows for all missing values in the range between the minimum and maximum value of `id_field`. All values in the added rows are `NA` except for the `id_field` and the field used for grouping the data (if there was one).
* `NONE` - No imputation is performed.

#### Results

After the substitutions, the data sets in `av_state$data` are sorted by their `id_field` value. This sorting step moves any rows with value `NA` for the `id_field` to the end.

Other than adjusting `av_state$data`, the `order_by` function creates the following variables in the `av_state` list:

* `impute_method` - the `impute_method` used.
* `order_by` - the `id_field` used.

#### Syntax

Example: `av_state <- order_by(av_state,'tijdstip',impute_method='ONE_MISSING')`


### select_range

    av_state <- select_range(av_state, subset_id='multiple',column,begin,end)

The `select_range` function selects which rows of a data set should be included. If the data set is grouped into multiple data sets, the `subset_id` argument needs to be supplied, telling the function to work per individual data set.

#### Arguments

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

The `column` argument specifies which column the begin and end values should be taken over. This argument is optional, and if it is missing, the value of `av_state$order_by` will be substituted.

Either the `begin` or the `end` argument need to be specified. The column does not need to be sorted for this function to work. Values are included if they are `>= begin` and `<= end`, if specified. This does not remove `NA` values.

#### Syntax

Example: `av_state <- select_range(av_state,'1',begin=20,end=40)`


Modifying and adding columns
----------------------------


### set_first_timestamp


### impute_missing_values

    av_state <- impute_missing_values(av_state,columns,subset_ids='ALL',type=c('SIMPLE','EM'))

The `impute_missing_values` function can impute data for values that are missing (i.e., for values that are `NA`). It outputs for every subset how many values were imputed (for all columns total) along with a percentage (following `<=`). This percentage is the percentage of the column with the highest percentage of imputed values, i.e., if multiple columns were specified, it is the percentage of values that were imputed of the column that had relatively most `NA` values.

#### Arguments

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

The `columns` argument can be a single column or a list of column names. Besides `av_state`, it is the only argument that is required.

The `subset_ids` argument can be a single subset, a range of subsets (both of which are identified by their indices), or it can be the word `'ALL'` (default). In the latter case, the selected columns of all data subsets are processed.

The `type` argument has two possible values:

* `SIMPLE` - The value of the missing data is determined by up to five values surrounding the value (2 before, 3 after, unless at the start or end of the range). For numeric (scl) columns, the mean of these values is chosen as value. For factor (nom) columns, the mode of these values is chosen as value.
* `EM` - Em imputation. Currently not implemented.

#### Syntax

Example: `av_state <- impute_missing_values(av_state,'norm_bewegen')` or `av_state <- impute_missing_values(av_state,c('norm_bewegen','minuten_woonwerk'),subset_ids=1)`


### add_derived_column

    av_state <- add_derived_column(av_state,name,columns,operation=c('SUM','LN','MINUTES_TO_HOURS'))

The `add_derived_column` function adds a new column, based on existing columns, to all identified groups in the current data set. The `name` argument holds the name of the new column.

#### Arguments

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

The `operation` argument has three possible values:

* `SUM` - The new column is the sum of the columns specified in the `columns` argument. So for this option, the `columns` argument is an array of column names. Values in the summation of columns that are `NA` are treated as if they're zero. Columns that are not numeric are transformed to numeric. For example, `Factor` columns are transformed to numbers starting at 0 for the first factor level.
* `LN` - The new column is the natural logarithm of the specified column in `columns`. Thus, for this option, the `columns` argument is simply the name of a single column. This operation does not work on columns that are not numeric. Values in the original column that are `NA` are left as `NA` in the new column. Note that values are increased if necessary so that the resulting column has no negative values.
* `MINUTES_TO_HOURS` - The new column is the values of the specified column divided by 60. Thus, for this option, the `columns` argument is simply the name of a single column. This operation does nto work on columns that are not numeric. Values in the original column that are `NA` are left as `NA` in the new column.

#### Syntax

Example: `av_state <- add_derived_column(av_state,'SomPHQ',c('PHQ1','PHQ2','PHQ3','PHQ4','PHQ5','PHQ6','PHQ7','PHQ8','PHQ9'),operation='SUM')`, `av_state <- add_derived_column(av_state,'lnSomBewegUur','SomBewegUur',operation='LN')`, or  `av_state <- add_derived_column(av_state,'SomBewegUur','SomBewegen',operation='MINUTES_TO_HOURS')`.


Vector Autoregression
---------------------


### var_main

    av_state <- var_main(av_state,vars,lag_max=7,significance=0.05,exogenous_max_iterations=3,
                         subset=1,log_level=av_state$log_level,include_restricted_models=FALSE,
                         small=FALSE,include_model=NULL)

The `var_main` function generates and tests possible VAR models for the specified variables. Aside from `av_state`, the only required argument is `vars`, which should be a list of variables.

#### Arguments

The first argument to this function has to be an object of class `av_state`. A modified `av_state` object is returned.

The `lag_max` argument limits the highest possible number of lags that will be used in a model. This number sets the maximum limit in the search for optimal lags.

The `significance` argument is the maximum P-value for which results are seen as significant. This argument is used in Granger causality tests, Portmanteau tests, and Jarque-Bera tests.

The `exogenous_max_iterations` argument determines how many times we should try to exclude additional outliers for a variable. The `exogenous_max_iterations` argument should be a number between 1 and 3:

* `1` - When Jarque-Bera tests fail, having `exogenous_max_iterations = 1` will only try with removing 3.5x std. outliers for the residuals of variables using exogenous variables.
* `2` - When `exogenous_max_iterations = 2`, the program will also try removing 3x std. outliers if JB tests still fail.
* `3` - When `exogenous_max_iterations = 3`, the program will also try removing 2.5x std. outliers if JB tests still fail.

The `subset` argument specifies which data subset the VAR analysis should run on. The VAR analysis only runs on one data subset at a time. If not specified, the first subset is used (corresponding to `av_state$data[[1]]`).

The `log_level` argument sets the minimum level of output that should be shown. It should be a number between 0 and 3. `0` = debug, `1` = test detail, `2` = test outcomes, `3` = normal. The default is set to the value of `av_state$log_level` or if that doesn't exist, to `0`. If the `log_level` parameter was specified, the original value of `av_state$log_level` will be restored at the end of `var_main`.

The `include_restricted_models` argument defaults to `FALSE`. When set to `TRUE`, the number of evaluated models is effectively doubled. Each model will be evaluated normally and in a restricted form. The restricted form removes nonsignificant coefficients from the formula. Setting `include_restricted_models` to `TRUE` can thus lead to finding additional solutions at the cost of twice the normal computation time.

The `small` argument defaults to `FALSE`. Its functionality corresponds to the `small` argument of Stata's `var` function. The `small` argument affects the outcome of the Granger causality test. When `small = TRUE`, the Granger causality test uses the F-distribution to gauge the statistic. When `small = FALSE`, the Granger causality test uses the Chi-squared distribution to gauge the statistic.

The `include_model` argument can be used to forcibly include a model in the evaluation. Included models have to be lists, and can specify the parameters `lag`, `exogenous_variables`, and `apply_log_transform`. For example:

    av_state <- var_main(av_state,c('Activity_hours','Depression'),
             log_level=3,
             small=TRUE,
             include_model=list(lag=3,
                                exogenous_variables=data.frame(variable="Depression",iteration=1,stringsAsFactors=FALSE),
                                apply_log_transform=TRUE))
    var_info(av_state$rejected_models[[1]]$varest)

The above example includes a model with `lag=3` (so lags 1, 2, and 3 are included), the model is ran on the logtransformed variables, and includes an exogenous dummy variable that has a 1 where values of `log(Depression)` are more than 3xstd away from the mean (because `iteration=1`, see the description of the `exogenous_max_iterations` parameter above for the meaning of the iterations) and 0 everywhere else. The included model is added at the start of the list, so it can be retrieved (assuming a valid `lag` was specified) with either `av_state$accepted_models[[1]]` if the model was valid or `av_state$rejected_models[[1]]` if it was invalid. In the above example, some info about the included model is printed (assuming it was invalid).

#### Results

The `var_main` function sets the following variables in the `av_state` list:

* `significance` - the `significance` used.
* `lag_max` - the `lag_max` used.
* `exogenous_max_iterations` - the `exogenous_max_iterations` used.
* `vars` - the `vars` used.
* `subset` - the `subset` used.
* `log_level` - the `log_level` used. This setting is restored at the end of `var_main` to its original value.
* `model_queue` - the list of models specified by only parameters, used as the main queue in `var_main`. This is a a list of objects with class `var_model`.
* `accepted_models` - the sorted list of accepted models and their var results. This is a a list of objects with class `var_modelres`. Each accepted model has properties `parameters` to retrieve the model parameters, and `varest` to retrieve the var result.
* `rejected_models` - the list of rejected models and their var results (excluding those from `model_queue` that did not have a specified lag). This is a a list of objects with class `var_modelres`. Each accepted model has properties `parameters` to retrieve the model parameters, and `varest` to retrieve the var result.

#### Syntax

Example: `av_state <- var_main(av_state,c('Activity_hours','Depression'),log_level=2)`


### var_info

    var_info(varest,log_level=0)

The `var_info` function prints a summary and the output of the tests for a var model. Note that its output can be altered by the value of `av_state$log_level`. The tests it shows are the Eigenvalue stability condition, the Portmanteau tests, the Jarque-Bera tests, the Granger causality Wald tests, and estat ic.

The `log_level` argument sets the verbosity of the output shown. It should be a number between 0 and 3. A lower level means more verbosity.

#### Syntax

Example: `var_info(av_state$accepted_models[[1]]$varest)` or `var_info(av_state$rejected_models[[1]]$varest)`


Outputting data
---------------


### visualize

    visualize(av_state,columns,...)

The `visualize` function works with single or multiple columns. When given an array of multiple columns as `columns` argument, all columns have to be of the numeric class. This function creates a combined plot with individual plots for each identified group in the current data set. Any supplied arguments other than the ones described are passed on to the plotting functions.

#### Arguments

The first argument to this function has to be an object of class `av_state`.

When given the name of a single column as `columns` argument, this function behaves differently depending on the class of the column:

* If the class of the column is `factor`, the column is seen as a nominal column, and the following arguments are accepted: `visualize(column,type=c('PIE','BAR','DOT','LINE'),title="",...)`. All plots  also accept the `xlab` argument, e.g., `xlab='minuten'`. Furthermore,  when the type is `BAR`, an additional argument `horiz` can be supplied (`horiz` is `FALSE` by default), which will draw horizontal bar charts instead of vertical ones. To show values over time rather than total values, the `LINE` type can be used. Example: `visualize('PHQ1')`.
* If the class of the column is `numeric`, the column is seen as a scale column, and the following arguments are accepted: `visualize(column,type=c('LINE','BOX'),title="",...)`. Furthermore, when the type is `LINE`, an additional argument `acc` can be supplied (`acc` is `FALSE` by default), which will plot lines of accumulated values rather than the individual values. Example: `visualize('minuten_sport',type='LINE',acc=TRUE)`.

When the `columns` argument is given a list of column names, the sums of the columns are displayed in the plots. For this to work, all columns have to be of the numeric class. When given a list of column names as the `columns` argument, the function accepts the following arguments: `visualize(columns,labels=columns,type=c('PIE','BAR','DOT'),title="",...)`. The arguments of this function work much like the ones described above for individual `factor` columns. The added optional `labels` argument should be a list of the same length as the `columns` argument, specifying custom names for the columns.

#### Syntax

Examples for using visualize with multiple columns: 

    visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar','minuten_vrijetijd','minuten_sport'), labels=c('licht werk','zwaar werk','vrije tijd','sport'),type='BAR',horiz=TRUE)
    visualize(av_state,c('sum_minuten_licht','sum_minuten_zwaar','minuten_vrijetijd','minuten_sport'),type='DOT',xlab='minuten')


### store_file

    store_file(av_state,filename,inline_data,file_type = c('SPSS','STATA'))

**In the web application, this function does not need to be called explicitly. It is appended to the code when the Download button is clicked.**

The `store_file` function will export all groups in the active data set to individual output files. All output files are subsequently packed in a .tar file that can be downloaded.

#### Arguments

The first argument to this function has to be an object of class `av_state`.

All other arguments are optional. When the `filename` argument is missing, the filename of the input file is substituted. The `inline_data` argument determines whether or not the data should be stored inline or in a separate file. If this argument is missing, inline storage is used for data sets with less than 81 columns, and separate storage is used otherwise.

Currently, only the `SPSS` `file_type` is supported. The `.sps` file that comes with the `SPSS` exports may require manual adjusting, as the fully quantified file path to the data set needs to be specified for it to work (relative file paths do not work).

#### Syntax

Example: `store_file(av_state)`


### print(av_state)

    print(av_state)

The `av_state` class supports the `print` command. This shows which parameters were used to construct the `av_state`.


### print_accepted_models(av_state)

    print_accepted_models(av_state)

When `av_state` is the result of a call to `var_main`, the above command can be used to show the list of accepted models.


### print_rejected_models(av_state)

    print_rejected_models(av_state)

When `av_state` is the result of a call to `var_main`, the above command can be used to show the list of rejected models.


