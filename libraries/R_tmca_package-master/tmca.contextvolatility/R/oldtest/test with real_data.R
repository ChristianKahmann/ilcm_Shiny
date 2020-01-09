load('./R/test_vola_method.RData')

vola3=calculate_context_volatility(binDTM = dtm,dates = dates,memory = 6,intervall = "month",cooc_measure = "DICE",
                                  significanceThreshold = 0,minCoocFreq = 1,measure = "rank_no_zero",terms = c("deutschland","merkel","flucht","europa"))


