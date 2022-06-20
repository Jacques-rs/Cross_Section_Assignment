


regression_format_country <- function(df){

    df <- df %>%
        mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
        mutate(death_rate = total_deaths/total_cases) %>%
        replace(is.na(.), 0) %>%
        mutate(death_rate = if(any(is.infinite(death_rate))){1}else{death_rate}) %>%
        mutate(year = year(date))


    return(df)
}


regression_format_country_alt <- function(df){

    df <- df %>%
        mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
        mutate(death_rate = new_deaths/new_cases) %>%
        replace(is.na(.), 0) %>%
        mutate(death_rate = if(any(is.infinite(death_rate))){1}else{death_rate}) %>%
        mutate(year = year(date))


    return(df)
}


# regression_format_continent <- function(df){
#
#     cols_mean <- c("stringency_index", "population_density", "aged_65_older", "gdp_per_capita",
#                    "extreme_poverty", "diabetes_prevalence", "smokers", "handwashing_facilities",
#                    "life_expectancy", "human_development_index")
#
#     df <- df %>%
#         group_by(date) %>%
#         mutate(across())
#
#
# }

# experiment <- function(df){
#
#     df <- df %>%
#         group_by(location, year) %>%
#         # Calculate the average values for those metrics that we need average values for
#         mutate() %>%
#         # sum those columns up whose values need to be summed (probably none)
#         aggregate(FUN = ) %>%
#         # Will only need the last values of those columns that are set up as
#         # cumulative totals
#
#
# }