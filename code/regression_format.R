

experiment_aggregate_week <- function(df){


    constant_features <- c("gdp_per_capita", "population_density",
                           "median_age", "aged_65_older",
                           "extreme_poverty", "cardiovasc_death_rate",
                           "diabetes_prevalence", "handwashing_facilities",
                           "hosp_beds_1k", "life_expectancy",
                           "human_development_index", "smokers",
                           "population")

    mean_cols = c("reproduction_rate",
                "stringency_index")

    df <- df %>%
        replace(is.na(.), 0) %>%
        select(-excess_mortality) %>%
        mutate(year_quarter = paste(year(date), quarter(date), sep = "-")) %>%
        relocate(year_quarter, .before = date) %>%
        group_by(location, year_quarter) %>%
        # mutate(one_day = n() - sum(is.na(excess_mortality))) %>%
        # relocate(one_day, .after = excess_mortality) %>%
        # group_by(location, year_week) %>%
        mutate(across(-c(constant_features, mean_cols, date), sum),
               across(c(mean_cols), function(x) mean(x))) %>%
        ungroup()

    return(df)

}




experiment_trim <- function(df){

    df <- df %>%
        group_by(location, year_quarter) %>%
        # filter(last(year_quarter)) %>%
        filter(row_number() == n()) %>%
        # slice(tail(row_number(), 1)) %>%
        ungroup() %>%
        select(-c(year_quarter)) %>%
        group_by(location, date) %>%
        # mutate(afflicted_rate = ((new_deaths+ icu_patients
        #                             + hosp_patients)/new_cases)*100) %>%
        mutate(afflicted_rate = ((new_deaths + icu_patients
                                  + hosp_patients)/population * 10000),
               .keep = "unused") %>%
        replace(is.na(.), 0)

        return(df)

}


# aggregate_semester <- function(df){
#
#
#     constant_features <- c("gdp_per_capita", "population_density",
#                            "median_age", "aged_65_older",
#                            "extreme_poverty", "cardiovasc_death_rate",
#                            "diabetes_prevalence", "handwashing_facilities",
#                            "hosp_beds_1k", "life_expectancy",
#                            "human_development_index", "smokers")
#
#     mean_cols = c("reproduction_rate",
#                   "stringency_index")
#
#     df <- df %>%
#         replace(is.na(.), 0) %>%
#         select(-excess_mortality) %>%
#         mutate(year_semester = paste(year(date), semester(date), sep = "-")) %>%
#         relocate(year_semester, .before = date) %>%
#         group_by(location, year_semester) %>%
#         # mutate(one_day = n() - sum(is.na(excess_mortality))) %>%
#         # relocate(one_day, .after = excess_mortality) %>%
#         # group_by(location, year_week) %>%
#         mutate(across(-c(constant_features, mean_cols, date), sum),
#                across(c(mean_cols), function(x) mean(x))) %>%
#         ungroup()
#
#     return(df)
#
# }
#
#
#
# trim_semester <- function(df){
#
#     df <- df %>%
#         group_by(location, year_semester) %>%
#         # filter(last(year_quarter)) %>%
#         filter(row_number() == n()) %>%
#         # slice(tail(row_number(), 1)) %>%
#         ungroup() %>%
#         select(-c(year_semester)) %>%
#         group_by(location, date) %>%
#         # mutate(death_rate = (new_deaths/new_cases)*100) %>%
#         mutate(afflicted_rate = ((new_deaths + icu_patients + hosp_patients)/new_cases)*100,
#                .keep = "unused") %>%
#         replace(is.na(.), 0)
#
#     return(df)
#
# }
