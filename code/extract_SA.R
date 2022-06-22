
extract_sa <- function(path = "./data/owid-covid-data.csv",
                       country = NULL, cont = NULL){
    # This function selects the relevant features that we want to work
    # with and takes as input the country/region we desire to isolate;



    # Store a variable that will be used to remove the extra appearances
    # of the same variable with different names
    # EDIT: might remove "new.*" from the following list because it allows for the
    # calculation of a weekly death rate for example
    names1 <- c(".+smoothed.*", ".+per_million", ".+per_thousand", ".+per_hundred",
               ".*cumulative.*", ".*weekly.*", "new.*")
     if(!is.null(country)){

         df <- read_csv(file = path, show_col_types = F) %>%
             # Retrieve our specific location
             filter(location == country) %>%
             # Remove undesired features
             select(-c(iso_code, continent, tests_units)) %>%
             # Remove the extra features as mentioned above `names1`
             .[, !grepl(names(.), pattern = paste(names1, collapse = "|"))]

         # # Wanted to groupby month to get more aggregated data but this may
         # # actually remove more information/variation so we will stick to
         # # daily values
         # mutate(YearMonth = substring(date, 1,7)) %>%
         # group_by(YearMonth) #%>%
         # # mutate(across(grep()))

     } else {if(!is.null(cont)){

         df <- read_csv(file = path, show_col_types = F) %>%
             # Retrieve our specific location
             filter(continent == cont) %>%
             # Remove undesired features
             select(-c(iso_code, tests_units)) %>%
             # Remove the extra features as mentioned above `names1`
             .[, !grepl(names(.), pattern = paste(names1, collapse = "|"))]

         # # Wanted to groupby month to get more aggregated data but this may
         # # actually remove more information/variation so we will stick to
         # # daily values
         # mutate(YearMonth = substring(date, 1,7)) %>%
         # group_by(YearMonth) #%>%
         # # mutate(across(grep()))

     } else {df = "Must provide a valid `country` or `continent` as an argument"}}

    # names2 <- c(".*total.*", )
    # Retrieve the entire data frame



    return(df)
}


feature_adj <- function(df){

    df %<>% select(-c(aged_70_older, total_boosters, people_fully_vaccinated)) %>%
        replace(is.na(.), 0) %>%
        mutate(smokers = mean(c(female_smokers, male_smokers)), .keep = "unused")

    return(df)

}

extract_sa_alt <- function(path = "./data/owid-covid-data.csv",
                       country = NULL, cont = NULL){
    # This function selects the relevant features that we want to work
    # with and takes as input the country/region we desire to isolate;



    # Store a variable that will be used to remove the extra appearances
    # of the same variable with different names
    # EDIT: might remove "new.*" from the following list because it allows for the
    # calculation of a weekly death rate for example
    names1 <- c(".+smoothed.*", ".+per_million", ".+per_thousand", ".+per_hundred",
                ".*cumulative.*", ".*weekly.*", "total.+")
    if(!is.null(country)){

        df <- read_csv(file = path, show_col_types = F) %>%
            # Retrieve our specific location
            filter(location == country) %>%
            # Remove undesired features
            select(-c(iso_code, continent, tests_units)) %>%
            rename(hosp_beds_1k = hospital_beds_per_thousand) %>%
            # Remove the extra features as mentioned above `names1`
            .[, !grepl(names(.), pattern = paste(names1, collapse = "|"))]

        # # Wanted to groupby month to get more aggregated data but this may
        # # actually remove more information/variation so we will stick to
        # # daily values
        # mutate(YearMonth = substring(date, 1,7)) %>%
        # group_by(YearMonth) #%>%
        # # mutate(across(grep()))

    } else {if(!is.null(cont)){

        df <- read_csv(file = path, show_col_types = F) %>%
            # Retrieve our specific location
            filter(continent == cont) %>%
            # Remove undesired features
            select(-c(iso_code, tests_units)) %>%
            rename(hosp_beds_1k = hospital_beds_per_thousand,
                   cum_excess_mortality = excess_mortality_cumulative) %>%
            # Remove the extra features as mentioned above `names1`
            .[, !grepl(names(.), pattern = paste(names1, collapse = "|"))]

        # # Wanted to groupby month to get more aggregated data but this may
        # # actually remove more information/variation so we will stick to
        # # daily values
        # mutate(YearMonth = substring(date, 1,7)) %>%
        # group_by(YearMonth) #%>%
        # # mutate(across(grep()))

    } else {df = "Must provide a valid `country` or `continent` as an argument"}}

    # names2 <- c(".*total.*", )
    # Retrieve the entire data frame



    return(df)
}

feature_adj_alt <- function(df){

    df %<>% select(-c(aged_70_older, people_fully_vaccinated, people_vaccinated,
                      tests_per_case, positive_rate, population)) %>%
        replace(is.na(.), 0) %>%
        mutate(smokers = mean(c(female_smokers, male_smokers)), .keep = "unused")

    return(df)

}


extract_all <- function(path = "./data/owid-covid-data.csv"){

    names1 <- c(".+smoothed.*", ".+per_million", ".+per_thousand", ".+per_hundred",
                ".*cumulative.*", ".*weekly.*", "total.+")

    continents <- extract_continents()$continent

    df <- read_csv(file = path, show_col_types = F) %>%
        filter(!location %in% c(continents)) %>%
        filter(!is.na(continent)) %>%
        group_by(location) %>%
        filter(first(date) <= lubridate::ymd(20200430)) %>%
        ungroup() %>%
        # Remove undesired features
        select(-c(iso_code, continent, tests_units)) %>%
        rename(hosp_beds_1k = hospital_beds_per_thousand) %>%
        # Remove the extra features as mentioned above `names1`
        .[, !grepl(names(.), pattern = paste(names1, collapse = "|"))]

    return(df)

}

feature_adj_all <- function(df){

    df %<>%
        mutate(new_vaccinations = (new_vaccinations/population) * 1000) %>%
        mutate(new_tests = (new_tests/population) * 1000) %>%
        # mutate(hosp_patients = (hosp_patients/population) * 1000000) %>%
        # mutate(icu_patients = (icu_patients/population) * 1000000) %>%

        # mutate(new_vaccinations = new_vaccinations) %>%
        # mutate(new_tests = new_tests) %>%
        # mutate(hosp_patients = hosp_patients) %>%
        # mutate(icu_patients = icu_patients) %>%

        select(-c(aged_70_older, people_fully_vaccinated, people_vaccinated,
                      tests_per_case, positive_rate, population)) %>%
        group_by(location) %>%
        # replace(is.na(.), 0) %>%
        mutate(smokers = mean(c(female_smokers, male_smokers)), .keep = "unused")

    return(df)

}