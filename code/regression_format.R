


# regression_format_country <- function(df){
#
#     df <- df %>%
#         mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
#         mutate(death_rate = total_deaths/total_cases) %>%
#         replace(is.na(.), 0) %>%
#         mutate(death_rate = ifelse(is.infinite(death_rate), 1, death_rate)) %>%
#         mutate(year = year(date))
#
#
#     return(df)
# }


# regression_format_country_alt <- function(df){
#
#     df <- df %>%
#         mutate(day_of_week = lubridate::wday(date, label = T, week_start = 1)) %>%
#         # mutate(death_rate = new_deaths/new_cases) %>%
#         replace(is.na(.), 0) #%>%
#         # mutate(year = year(date))
#
#
#     return(df)
# }




# First need to aggregate data by year_week, then by
# year_quarter

experiment_aggregate_week <- function(df){

    # mean_or_max <- function(df, col){
    #     if(sum(is.na(col)) == 6){
    #         df %>% replace(is.na(.), 0) %>% mutate(across(c(col), max))
    #     }else{
    #         df %>% replace(is.na(.), 0) %>% mutate(across(c(col), mean))
    #     }
    # return(df)
    # }


    constant_features <- c("gdp_per_capita", "population_density",
                           "median_age", "aged_65_older",
                           "extreme_poverty", "cardiovasc_death_rate",
                           "diabetes_prevalence", "handwashing_facilities",
                           "hosp_beds_1k", "life_expectancy",
                           "human_development_index", "smokers")

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
               across(mean_cols, mean))


}




experiment_aggregate <- function(df){

    change <- c()



    df <- df %>%
        # select(-c("death_rate")) %>%
        # need to create a col that indicates which quarter of which year it is
        mutate(year_quarter = paste(year(date), quarter(date), sep = "-")) %>%
        # Then I need to groupby this new Quarter_Year variable to aggreagte the
        # df by this metrics
        group_by(location) %>%
        # take care of the fact that certain features are only recorded on a weekly basis
        fill() %>%
        group_by(location, year_quarter) %>%
        # Calculate the average values for those metrics that we need average values for
        mutate(across(names(df[, grepl(x = names(df), pattern = "new.+")]), sum),
               across(c("stringency_index", "excess_mortality"), mean)) #%>%
        # # We might want to calculate the avg change in the variables to
        # # make the regression estimates more accurate
        # mutate(across())

        return(df)
}


experiment_trim <- function(df){

    df <- df %>%
        mutate(year_quarter = lubridate::yq(year_quarter)) %>%
        group_by(location, year_quarter) %>%
        # filter(last(year_quarter)) %>%
        filter(row_number() == n()) %>%
        # slice(tail(row_number(), 1)) %>%
        ungroup() %>%
        select(-c(year_quarter)) %>%
        group_by(location, date) %>%
        mutate(death_rate = (new_deaths/new_cases)*100, .keep = "unused") %>%
        replace(is.na(.), 0) %>%

        return(df)

}

# experiment_scale <- function(df){
#
#
#
# }



# africa_df_alt %>%
#     mutate(across(names(df[, grepl(x = names(df), pattern = "new.+")]), sum),
#            across(c("stringency_index", "excess_mortality"), mean))



