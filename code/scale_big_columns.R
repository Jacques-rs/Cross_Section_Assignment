

cols_range <- function(df, constant_features = c("gdp_per_capita", "population_density",
                                                 "median_age", "aged_65_older",
                                                 "extreme_poverty", "cardiovasc_death_rate",
                                                 "diabetes_prevalence", "handwashing_facilities",
                                                 "hosp_beds_1k", "life_expectancy",
                                                 "human_development_index", "smokers")){

    descriptive_stats <- df %>% ungroup() %>%
        select(-c(constant_features, date, location)) %>%
        describe() %>%
        select(-c(median, mad, se, vars, n, skew, kurtosis, trimmed))

    return(descriptive_stats)

}



scale_bigs <- function(df, big_cols = c("icu_patients", "hosp_patients",
                                        "new_tests", "new_vaccinations")){


    df <- df %>%
        ungroup() %>%
        group_by(location) %>%
        mutate(across(big_cols, function(x) cumsum(x))) %>%
        mutate(across(big_cols, function(x) if_else(x == 0,
                                                    0, log(x))))


    return(df)


}


cols_range_constant <- function(df, constant_features = c("gdp_per_capita", "population_density",
                                                          "median_age", "aged_65_older",
                                                          "extreme_poverty",
                                                          "cardiovasc_death_rate",
                                                          "diabetes_prevalence",
                                                          "handwashing_facilities",
                                                          "hosp_beds_1k", "life_expectancy",
                                                          "human_development_index", "smokers")){

    descriptive_stats <- df %>% ungroup() %>%
        select(constant_features) %>%
        describe() %>%
        select(-c(median, mad, se, vars, n, skew, kurtosis, trimmed))

    return(descriptive_stats)

}


scale_bigs_constant <- function(df,
                                constant_features = c("gdp_per_capita", "population_density",
                                                           "median_age", "aged_65_older",
                                                           "extreme_poverty",
                                                      "cardiovasc_death_rate",
                                                           "diabetes_prevalence",
                                                      "handwashing_facilities",
                                                           "hosp_beds_1k", "life_expectancy",
                                                           "human_development_index", "smokers")){

    df <- df %>%
        ungroup() %>%
        mutate(across(constant_features, function(x) if_else(x == 0,
                                                    0, log(x))))

    return(df)
}


