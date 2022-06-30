library(tidyverse)
library(stargazer)
library(plm)
library(lmtest)
library(sandwich)
library(nlme)
library(estimatr)
library(panelAR)
library(kableExtra)
#-----------------------------------------------------------------------------------------------
# Load data
plm_df <- mega_df %>%
  select(iso3, year,
         irr,
         marx_rop,
         unemployment_rate,
         inflation_rate,
         pct_gdp_govt_subsidies,
         pct_gdp_international_trade,
         pct_gdp_corporate_financial_assets,
         pct_gdp_national_financial_assets,
         pct_total_investment_GFCF,
         oecd_gfcf_pct_gdp) %>%
  mutate(year = as.numeric(year)) %>%
  filter(year %in% c(1980:2017))

regression_df_pdata <- pdata.frame(plm_df, index = c('iso3', 'year'))
#-----------------------------------------------------------------------------------------------
# Finding vars
regression_df %>%
  select(contains('private_socspend')) %>%
  names()

summary(regression_df$pr)
#-----------------------------------------------------------------------------------------------
# Creatomg PLM form
plm_form_m1 <- pct_total_investment_GFCF ~
  lag(marx_rop) +
  lag(unemployment_rate) +
  lag(inflation_rate) +
  lag(pct_total_investment_GFCF)

plm_form_m2 <- pct_total_investment_GFCF ~
  (irr) +
  unemployment_rate +
  inflation_rate
#-----------------------------------------------------------------------------------------------
# Fixed Effects Model 1
#-----------------------------------------------------------------------------------------------
plm_m1 <- plm(plm_form_m1,
              data = regression_df_pdata,
              model = "pooling")
#-----------------------------------------------------------------------------------------------
# Fixed effects model 2
#-----------------------------------------------------------------------------------------------
plm_m1.1 <- plm(plm_form_m1,
              data = regression_df_pdata,
              effect = "individual",
              model = "within")
#-----------------------------------------------------------------------------------------------
# Fixed Effects Model 3
#-----------------------------------------------------------------------------------------------
plm_m1.2 <- plm(plm_form_m1,
              data = regression_df_pdata,
              effect = "time",
              method = "within")
#-----------------------------------------------------------------------------------------------
# Twoway Fixed Effects Model 4
#-----------------------------------------------------------------------------------------------
plm_m1.3 <- plm(plm_form_m1,
                data = regression_df_pdata,
                effect = "twoways",
                method = "within")
#-----------------------------------------------------------------------------------------------
# Twoway Fixed Effects Model 5
#-----------------------------------------------------------------------------------------------
plm_m1.4 <- plm(plm_form_m1,
                data = regression_df_pdata,
                method = "random")
#-----------------------------------------------------------------------------------------------
# PCSE regression results via stargazer package
stargazer(plm_m1,
          plm_m1.1,
          plm_m1.2,
          plm_m1.3,
          plm_m1.4,
          type = "text",
          dep.var.labels.include = FALSE,
          model.names = FALSE,
          header = FALSE,
          no.space = TRUE,
          omit.stat = c("f", "rsq"))
#-----------------------------------------------------------------------------------------------


summary(plm_m1)
