clear all
cd "C:/Users/marti/OneDrive/Plocha/skola/cerge/labor_econ/labor-econ-empirical-assignment"


import delimited "data/main_data.csv"

oaxaca ln_y schooling_years exper exper_sq, by(female) pooled noisily

estimates store test
esttab test using tables/oaxaca.tex, se replace
