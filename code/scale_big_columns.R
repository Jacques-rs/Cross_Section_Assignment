

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



scale_bigs_cumsum <- function(df, big_cols = c("new_tests", "new_vaccinations")){


    df <- df %>%
        ungroup() %>%
        group_by(location) %>%
        mutate(across(big_cols, function(x) cumsum(x),
                      .names = "{.col}"), .keep = "unused") %>%



    return(df)


}

scale_bigs_scale <- function(df, big_cols = c("new_vaccinations", "new_tests")){


    df <- df %>%
        ungroup() %>%
        mutate(across(big_cols, function(x) scale(x), .names = "{.col}_cum_per_1000"),
               .keep = "unused")


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


scale_bigs_constant <- function(df){


    df <- df %>%
        ungroup() %>%
        mutate(across(c("population_density", "cardiovasc_death_rate"),
                      function(x) scale(x, center = T), .names = "{.col}_norm"),
               across(c("gdp_per_capita"), function(x) if_else(x == 0, 0, log(x)),
                      .names = "{.col}_log"),
               across(c("human_development_index"), function(x) x * 100, .names = "{.col}"),
               .keep = "unused")

    return(df)
}


