Autovar
=======

Autovar is an R package for automating and simplifying the process from raw data to VAR models. For the actual VAR calculations, Bernhard Pfaff's vars package is used.

To install, type the following:

    install.packages('devtools')
    require('devtools')
    install_github('roqua/autovar')

Documentation for this package can be found [here](http://autovar.nl/docs).

##### Example Use

    library('autovar')
    
    # Example data sets can be found on https://autovar.nl
    av_state <- load_file("/path/to/file.dta")
    
    # Include models with (and without) trends in the search
    av_state <- add_trend(av_state)
    
    # Include models with (and without) day dummies in the search
    av_state <- set_timestamps(av_state,          
                               date_of_first_measurement = "2015-12-31",
                               measurements_per_day = 1)
                               
    # Search for VAR models for the variables Depression and Activity up to lag 3.
    av_state <- var_main(av_state, vars = c("Depression", "Activity"),
                         lag_max = 3,
                         log_level = 3)
    
    # Show the best models found
    print_best_models(av_state)
