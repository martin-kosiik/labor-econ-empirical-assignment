
setwd("C:/Users/marti/OneDrive/Plocha/skola/cerge/labor_econ/labor-econ-empirical-assignment")
library(haven)
library(readxl)
library(tidyverse)
library(janitor)

main_data<- read_dta("RQdata.dta")
ur95 <- read_excel("ur95.xls")

main_data <- main_data %>% 
  left_join(ur95, by = "code") 

# Check number of missing values for each column
main_data %>% 
  map_int(~ sum(is.na(.)))

# Variable c14 has 101 missing values, c13 22, and c06 18
