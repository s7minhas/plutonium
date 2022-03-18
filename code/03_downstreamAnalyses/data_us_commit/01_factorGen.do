clear
use "C:\Users\herme\Research\plutonium\code\03_downstreamAnalyses\data_us_commit\main_constraint_1.dta"

factor def_per_gdp asia_per westerneurope_per easterneurope_per formersovietunion_per middleeast_per africa_per perforeignmil f_l_stock_crises f_l_price_crises  emp_rate gdp_growth std_defense_budget std_totalworld std_totalformil std_hostiledeaths, factor(3)

scree

predict ff1 ff2 ff3

summarize

cor  f1_USdeaths_MIDEAST ff1

cor  f2_DEFspend_FORcommits ff2

cor  f3_UE ff3


factor f_l_stock_crises f_l_price_crises  emp_rate gdp_growth, factor(3)
scree
