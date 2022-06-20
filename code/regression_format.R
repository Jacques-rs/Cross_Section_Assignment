


regression_format_country <- function(df){

    df <- df %>%
        mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
        mutate(death_rate = total_deaths/total_cases) %>%
        replace(is.na(.), 0) %>%
        mutate(death_rate = ifelse(is.infinite(death_rate), 1, death_rate)) %>%
        mutate(year = year(date))


    return(df)
}


regression_format_country_alt <- function(df){

    df <- df %>%
        mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
        mutate(death_rate = new_deaths/new_cases) %>%
        replace(is.na(.), 0) #%>%
        # mutate(year = year(date))


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

experiment_aggregate <- function(df){


    df <- df %>%
        # select(-c("death_rate")) %>%
        # need to create a col that indicates which quarter of which year it is
        mutate(year_quarter = paste(year(date), quarter(date), sep = "-")) %>%
        # Then I need to groupby this new Quarter_Year variable to aggreagte the
        # df by this metrics
        group_by(location, year_quarter) %>%
        # Calculate the average values for those metrics that we need average values for
        mutate(across(names(df[, grepl(x = names(df), pattern = "new.+")]), sum),
               across(c("stringency_index", "excess_mortality"), mean)) %>%


        return(df)
}


experiment_trim <- function(df){

    df <- df %>%
        mutate(year_quarter = lubridate::yq(year_quarter)) %>%
        group_by(location, year_quarter) %>%
        # filter(last(year_quarter)) %>%
        slice(tail(row_number(), 1)) %>%
        select(-c(year_quarter, death_rate)) %>%
        mutate(death_rate = new_deaths/new_cases)



}

# experiment_scale <- function(df){
#
#
#
# }





