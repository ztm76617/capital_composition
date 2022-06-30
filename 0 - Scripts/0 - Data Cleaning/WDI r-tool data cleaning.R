library(tidyverse)
library(ggrepel)
library(ggpubr)
library(gplots)   
library(car)
library(zoo)
library(rio)
library(readxl)
library(haven)
library(lubridate)
library(scales)
library(IndexNumR)
library(wid)
library(OECD)
library(WDI)
library(scales)
library(datawrangling)
library(data.table)
#--------------------------------------------------------------------------
WDI::WDIsearch(string = "poverty headcount")
#--------------------------------------------------------------------------
full_OECD_country_list_df_iso2 <- (full_OECD_country_list_df$country_code_ISO2)

misc_WDI_vars_df <- WDI(country = c(full_OECD_country_list_df_iso2),
    indicator = c('NE.GDI.FPRV.CN',
                  'GC.NFN.TOTL.CN',
                  'GC.NFN.TOTL.GD.ZS',
                  'SM.POP.TOTL.ZS',
                  'NY.ADJ.DKAP.CD',
                  'NY.ADJ.DKAP.GN.ZS',
                  'GC.XPN.COMP.CN',
                  '8.0.LIPI',
                  'SL.UEM.TOTL.NE.ZS',
                  'FP.CPI.TOTL.ZG',
                  'FS.AST.PRVT.GD.ZS',
                  'NY.GDP.TOTL.RT.ZS',
                  'SI.POV.UMIC'),
    start = 1960,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en") %>%
  select(-country) %>%
  rename(country_code_ISO2 = iso2c,
         GFCF_private_sector_current_LCU = NE.GDI.FPRV.CN,
         net_invest_nonfin_assets_current_LCU = GC.NFN.TOTL.CN,
         pct_gdp_net_invest_nonfin_assets = GC.NFN.TOTL.GD.ZS,
         intl_migrant_pct_total_population = SM.POP.TOTL.ZS,
         consumption_fixed_capital_current_USD = NY.ADJ.DKAP.CD,
         pct_GNI_consumption_fixed_capital = NY.ADJ.DKAP.GN.ZS,
         compensation_employees_current_LCU = GC.XPN.COMP.CN,
         labor_income_poverty_idx = `8.0.LIPI`,
         unemployment_rate_national_estimate = SL.UEM.TOTL.NE.ZS,
         inflation_CPI_pct_annual = FP.CPI.TOTL.ZG,
         pct_gdp_domestic_credit_provided_to_private_sector = FS.AST.PRVT.GD.ZS,
         pct_gdp_natural_resource_rents = NY.GDP.TOTL.RT.ZS,
         poverty_headcount_550ppp = SI.POV.UMIC) %>%
  rename_with( ~ paste("WB_WDI", .x, sep = "_")) %>%
  rename(country_code_ISO2 = WB_WDI_country_code_ISO2,
         year = WB_WDI_year) %>%
  mutate(year = as.numeric(year))
#-----------------------------------------------------------------------------------------------------------------
write_rds(misc_WDI_vars_df, "1 - Data/misc_WDI_vars_df.rds")
#-----------------------------------------------------------------------------------------------------------------

