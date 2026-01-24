sst19 <- SS_output(
    'c:/ss/thornyheads/SST_2019_catch-only_update/SST_2019_models/SST_2019/'
)
sst23 <- SS_output(
    'c:/ss/thornyheads/SST_2023/shortspine_thornyhead_2023/2_base_model'
)
SStableComparisons(SSsummarize(list(sst19, sst23)), likenames = NULL, names = c("Bratio_2013", "_SPR", "_MSY"))
#                 Label      model1      model2
# 1         Bratio_2013 7.41723e-01 4.34680e-01
# 2          SSB_SPRtgt 7.59061e+04          NA
# 3         Fstd_SPRtgt 1.52922e-02          NA
# 4     TotYield_SPRtgt 2.03380e+03          NA
# 5             SSB_SPR          NA 9.88021e+03
# 6            annF_SPR          NA 1.01977e-02
# 7      Dead_Catch_SPR          NA 1.10842e+03
# 8             SSB_MSY 6.45998e+04 6.15491e+03
# 9             SPR_MSY 4.50349e-01 3.48134e-01
# 10           Fstd_MSY 1.81645e-02          NA
# 11       TotYield_MSY 2.06219e+03          NA
# 12       RetYield_MSY 2.01500e+03          NA
# 13           annF_MSY          NA 1.67175e-02
# 14     Dead_Catch_MSY          NA 1.22655e+03
# 15      Ret_Catch_MSY          NA 1.15352e+03
# 16 B_MSY/SSB_unfished          NA 2.77933e-01

