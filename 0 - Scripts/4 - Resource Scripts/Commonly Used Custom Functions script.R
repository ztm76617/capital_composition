#-----------------------------------------------------------
# Round function
round_function <- function(x){
  round(x, digits = 2)
}
#-----------------------------------------------------------
# Times 100
x100_func <- function(x){
  (x*100)
}
#-----------------------------------------------------------
# annual growth function
pct_change_function <- function(x){
  ((x - dplyr::lag(x))/x) * 100
}
#-----------------------------------------------------------
# 4 year moving average function
four_yr_moving_avg_func <- function(x){
  rollmean(x, k = 4, fill = NA)
}
#-----------------------------------------------------------
# 5 year moving average function
five_yr_moving_avg_func <- function(x){
  rollmean(x, k = 5, fill = NA)
}
#-----------------------------------------------------------
# 8 year moving average function
eight_yr_moving_avg_func <- function(x){
  rollmean(x, k = 8, fill = NA)
}
#-----------------------------------------------------------
# 10 year moving average function
ten_yr_moving_avg_func <- function(x){
  rollmean(x, k = 10, fill = NA)
  }
#------------------------------------------------------------
# times a million (usually for OECD data)
times_one_million_function <- function(x){(x*1e6)}
#-----------------------------------------------------------------------------------
# Deflate LCU at current prices and then convert to 2019 PPPs
deflate_plus_ppp_convert_func <- function(x){
  (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019
}
#-----------------------------------------------------------------------------------
# Deflate LCU at current prices and then convert to USD using 2019 market exchange rates
deflate_plus_market_exch_convert_func <- function(x){
  (x/mega_combined_vars_df_edited$WID_national_income_price_index)/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019
}
#-----------------------------------------------------------------------------------
# Convert LCU at constant prices to USD using 2019 market exchange rates
market_exch_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_market_exchange_rate_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------
# Convert LCU at constant prices to USD using 2019 PPP conversion rates
ppp_only_convert_func <- function(x){(x/mega_combined_vars_df_edited$WID_ppp_LCU_per_USD_2019)}
#-----------------------------------------------------------------------------------
# cumulative average function
cumulative_avg_func <- function(x){
  cumsum(x)/seq_along(x) 
}
#-----------------------------------------------------------------------------------
base_2019_convert_plus_deflate_function <- function(data,
                                                    x,
                                                    market_exch_rate_2019,
                                                    ppp_conversion_rate_2019,
                                                    deflator_2019,
                                                    x2){
  data %>%
    mutate(x_2019_price_2019_USD = (x/market_exch_rate_2019)/deflator_2019,
           x_2019_price_2019_ppp = (x/ppp_conversion_rate_2019)/deflator_2019) %>%
    rename_at(c('x_2019_price_2019_USD', 'x_2019_price_2019_ppp'), funs(str_replace_all(., 'x_', x2)))
}
# Example -----------------
base_2019_convert_plus_deflate_function(mega_combined_vars_df_SUBSET_added_vars,
                              mega_combined_vars_df_SUBSET_added_vars$pwt_gdp_current_LCU,
                              mega_combined_vars_df_SUBSET_added_vars$WID_market_exchange_rate_LCU_per_USD_2019,
                              mega_combined_vars_df_SUBSET_added_vars$WID_ppp_LCU_per_USD_2019,
                              mega_combined_vars_df_SUBSET_added_vars$WID_national_income_price_index,
                              'gdp_') %>%
  view()
#-----------------------------------------------------------------------------------
base_2017_convert_plus_deflate_function <- function(data,
                                                    x,
                                                    market_exch_rate_2017,
                                                    ppp_conversion_rate_2017,
                                                    deflator_2017,
                                                    x2){
  data %>%
    mutate(x_2017_price_2017_USD = (x/market_exch_rate_2017)/deflator_2017,
           x_2017_price_2017_ppp = (x/ppp_conversion_rate_2017)/deflator_2017) %>%
    rename_at(c('x_2017_price_2017_USD', 'x_2017_price_2017_ppp'), funs(str_replace_all(., 'x_', x2)))
}
#-----------------------------------------------------------------------------------
hc1_function <- function(x){
  cov <- vcovHC(x, type = "HC1")
  sqrt(diag(cov))
}
#-----------------------------------------------------------------------------------
pcse_function <- function(x){
  cov <- vcovPC(x, cluster = ~iso3)
  sqrt(diag(cov))
}
#-----------------------------------------------------------------------------------



































