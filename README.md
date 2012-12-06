autovar
=======

A package to automate and simplify the process from raw SPSS or STATA data to VAR models.


Reading data files
------------------

Currently, one data set can be operated on at at time. The data set, along with any metadata associated with it, is stored in the global variable `av_state`. This variable supports the print command (`print av_state`).


### load_file

    load_file(filename,file_type = c('SPSS','STATA'))

The `file_type` argument is optional. When not specified, it is determined from the `filename`, i.e., `.dta` extensions are treated as STATA files and `.sav` extensions are treated as SPSS files.

This function creates the following variables in the `av_state` list:

* `file_name` - The full file name including path. The working directory is prepended if a partial path is supplied.
* `file_type` - The type of the file. Can be `SPSS` or `STATA`. Used for determining how to read the file.
* `raw_data` - The raw file data as it is read, before any added columns, imputations, sorting, or splitting.
* `data` - The current data set. Initially, `data` is a list containing a single item, such that `raw_data` is identical to `data[['multiple']]`.

Example: `load_file("../data/input/RuwedataAngela.sav")`


### group_by

    group_by(id_field)


The `group_by` function splits up the initial data set into multiple sets based on their value for `id_field`.

For example, if we have a data set with a field named `id`  that has values ranging from 11 to 15, calling `group_by(id)` will set `av_state$data` to a list of five items. This list is ordered by the value of the id field. Then, we can use `av_state$data[[1]]` (or equivalently, `av_state$data[['11']]`) to retrieve the rows in the data set that have `id` 11. Likewise, use `av_state$data[[2]]` or `av_state$data[['12']]` for rows with `id` 12.

Other than adjusting `av_state$data`, the `group_by` function creates the following variables in the `av_state` list:

* `group_by` - the `id_field` used for grouping the data.


### order_by

    order_by(id_field,impute_method=c('ONE_MISSING','ADD_MISSING','NONE'))


Modifying and adding columns
----------------------------

