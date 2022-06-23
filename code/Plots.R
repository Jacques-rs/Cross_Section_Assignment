
# function to check the distribution of first date observations
# per location
start_date <- function(){

    plot <- read_csv(file = "./data/owid-covid-data.csv",
                     show_col_types = F) %>%
        group_by(location) %>%
        filter(date == first(date)) %>%
        ggplot() +
        geom_point(aes(x = location, y = date)) +
        geom_hline(yintercept = lubridate::ymd(20200430), color = "red") +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_date("First Date Observation")

    return(plot)

}


afflicted_plot <- function(df){

    plot <- df %>% ggplot(aes(fill=location, y = afflicted_rate/206,
                              x = date)) +
        geom_area(position = "stack", stat = "identity", alpha = 0.5) +
        theme(legend.position="none") +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("Afflicated Rate")

    return(plot)
}

stringency_plot <- function(df){

    plot <- df %>% ggplot(aes(fill=location, y = stringency_index/206,
                              x = date)) +
        geom_area(position = "stack", stat = "identity", alpha = 0.5) +
        theme(legend.position="none") +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("Covid Stringency Index")

    return(plot)
}


cumulative_test_plot <- function(df){

    plot <- world_df %>% ungroup() %>% group_by(location) %>%
        mutate(label = if_else(date == last(date), as.character(location),
                               NA_character_)) %>%
        # filter(date == last(date)) %>%
        ggplot(aes(x = date, y = new_tests, group = location, col = location)) +
        geom_line() +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("cumulative Vaccines per Thousand") +
        geom_label_repel(aes(label = label),
                         nudge_x = 1,
                         na.rm = TRUE) +
        theme(legend.position="none")

    return(plot)
}


cumulative_vax_plot <- function(df){

    plot <- df %>% ungroup() %>% group_by(location) %>%
        mutate(label = if_else(date == last(date), as.character(location),
                               NA_character_)) %>%
        # filter(date == last(date)) %>%
        ggplot(aes(x = date, y = new_tests, group = location, col = location)) +
        geom_line() +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("cumulative Tests per Thousand") +
        geom_label_repel(aes(label = label),
                         nudge_x = 1,
                         na.rm = TRUE) +
        theme(legend.position="none")

    return(plot)

}


cardio_plot <- function(df){

    plot <- df %>% ungroup() %>% group_by(location) %>%
        filter(date == last(date)) %>%
        ggplot() +
        geom_point(aes(x = reorder(location, cardiovasc_death_rate, mean),
                       y = cardiovasc_death_rate)) +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("Cardiovascular Death Rate") +
        scale_x_discrete("Country") +
        labs(title = "Cardiovascular Death Rate")

    return(plot)
}


gdp_plot <- function(df){

    plot <- df %>% ungroup() %>% group_by(location) %>%
        filter(date == last(date)) %>%
        ggplot() +
        geom_point(aes(x = reorder(location, gdp_per_capita, mean),
                       y = gdp_per_capita)) +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("GDP per Capita") +
        scale_x_discrete("Country") +
        labs(title = "GDP per capita")

    return(plot)
}


pop_density_plot <- function(df){

    plot <- df %>% ungroup() %>% group_by(location) %>%
        filter(date == last(date)) %>%
        ggplot() +
        geom_point(aes(x = reorder(location, population_density, mean),
                       y = population_density)) +
        theme(axis.text.x = element_blank(),
              axis.title = element_text(face = "bold"),
              axis.line = element_line(colour = "grey50", size = 1)) +
        scale_y_continuous("Population Density") +
        scale_x_discrete("Country") +
        labs(title = "Population Density")

    return(plot)
}


cor_plot1 <- function(df){

    plot1 <- df %>% ungroup() %>% select(-c(location, date,
                                         gdp_per_capita_log,
                                         population_density_norm,
                                         cardiovasc_death_rate_norm,
                                         new_tests_cum_per_1000,
                                         new_vaccinations_cum_per_1000,
                                         median_age, extreme_poverty,
                                         diabetes_prevalence,
                                         new_cases, reproduction_rate,
                                         new_cases)) %>%
        cor(.)
    plot2 <- plot1 %>%
        corrplot(., method = "color", order = "hclust", tl.srt=0, diag = F,
                 tl.col = "black", addCoef.col = "black",
                 tl.pos = "l",
                 tl.cex = 0.8,
                 number.font = 8)

    return(plot2)
}

cor_plot2 <- function(df){

    plot1 <- df %>% ungroup() %>% select(c(gdp_per_capita_log,
                                          population_density_norm,
                                           cardiovasc_death_rate_norm,
                                          new_vaccinations_cum_per_1000,
                                           median_age, extreme_poverty,
                                          diabetes_prevalence,
                                          afflicted_rate)) %>%
        cor(.)

    plot2 <- plot1 %>%
            corrplot(., method = "color", order = "hclust", tl.srt=0, diag = F,
                     tl.col = "black", addCoef.col = "black",
                     tl.pos = "l",
                     tl.cex = 0.8,
                     number.font = 8)

    return(plot2)

}

