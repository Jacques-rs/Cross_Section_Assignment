#
#
# change_calculator <- function(df, cols_mean = NULL,
#                               cols_sum = NULL){
#
#     # This function calculates the change in the calue for each variable
#     # that will be used in the Regression
#
#     # Not sure if this will be necessary if we are using a fixed
#     # effects regression model - Which will likely be the case
#
#     change <- function(col){
#
#         new_col <- col/lag(col)
#     }
#
#     df <- df %>% mutate(across(cols, ), across(cols_sum, sum))
#
#     return(df)
#
# }
#
