


extract_continents <- function(path = "./data/owid-covid-data.csv"){

    continents <- read_csv(file = path, show_col_types = F) %>%
        select(continent) %>% unique()

    return(continents)

}


