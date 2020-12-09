cd "C:/Users/marti/OneDrive/Plocha/skola/cerge/labor_econ/labor-econ-empirical-assignment/data"


import delimited "main_data.csv"

oaxaca ln_y schooling_years exper exper_sq, by(female) pooled noisily

estimates store test
esttab test using oaxaca.tex, se

estout test using example.txt, cells(b)
estout test, cells(b0)




oaxaca ln_y schooling_years exper exper_sq, by(female) pooled noisily eform