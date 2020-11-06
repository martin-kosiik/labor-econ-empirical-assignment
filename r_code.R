
setwd("C:/Users/marti/OneDrive/Plocha/skola/cerge/labor_econ/labor-econ-empirical-assignment")
library(haven)
library(readxl)
library(tidyverse)

main_data<- read_dta("RQdata.dta")
ur95 <- read_excel("ur95.xls")

main_data <- main_data %>% 
  left_join(ur95, by = "code") 