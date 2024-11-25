test_that("long prevalence", {
  testdata <- random_datset(100)
  res <- prevalence_long_format(
    indicators = list(
      indicator_iodine(),
      indicator_ferritin(),
      indicator_ferritin(adjustment_ferritin_implausible()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = testdata$age_years,
    sex = testdata$sex,
    pregnancy_status = testdata$pregnancy_status,
    lactating_status = testdata$lactating_status,
    ferritin = testdata$ferritin_measurement,
    iodine = testdata$iodine,
    CRP = testdata$crp_measurement,
    AGP = testdata$agp_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    is_smoker = testdata$is_smoker,
    altitude = testdata$altitude,
    smokes_cigarettes_per_day = testdata$smokes_cigarettes_per_day,
    pregnancyweeks = testdata$pregnancyweeks,
    pregnancymonths = testdata$pregnancymonths,
    cluster = 1:nrow(testdata),
    strata = rep.int(1, nrow(testdata))
  )
  expect_true(is.data.frame(res))
  expect_true(nrow(res) > 0)
  expect_setequal(
    colnames(res),
    c(
      "Group", "age_start", "age_end", "age_unit", "ferritin_unadj_pop",
      "ferritin_unadj_unwpop", "ferritin_unadj_mean", "ferritin_unadj_mean_sd",
      "ferritin_unadj_mean_ll", "ferritin_unadj_mean_ul", "ferritin_unadj_geomean",
      "ferritin_unadj_geomean_ll", "ferritin_unadj_geomean_ul", "ferritin_unadj_10percentile",
      "ferritin_unadj_25percentile", "ferritin_unadj_50percentile",
      "ferritin_unadj_75percentile", "ferritin_unadj_90percentile",
      "ferritin_unadj_p5_r", "ferritin_unadj_p5_se", "ferritin_unadj_p5_ll",
      "ferritin_unadj_p5_ul", "ferritin_unadj_p10_r", "ferritin_unadj_p10_se",
      "ferritin_unadj_p10_ll", "ferritin_unadj_p10_ul", "ferritin_unadj_p12_r",
      "ferritin_unadj_p12_se", "ferritin_unadj_p12_ll", "ferritin_unadj_p12_ul",
      "ferritin_unadj_p15_r", "ferritin_unadj_p15_se", "ferritin_unadj_p15_ll",
      "ferritin_unadj_p15_ul", "ferritin_unadj_p30_r", "ferritin_unadj_p30_se",
      "ferritin_unadj_p30_ll", "ferritin_unadj_p30_ul", "ferritin_unadj_p70_r",
      "ferritin_unadj_p70_se", "ferritin_unadj_p70_ll", "ferritin_unadj_p70_ul",
      "ferritin_unadj_p100_r", "ferritin_unadj_p100_se", "ferritin_unadj_p100_ll",
      "ferritin_unadj_p100_ul", "ferritin_unadj_p150_r", "ferritin_unadj_p150_se",
      "ferritin_unadj_p150_ll", "ferritin_unadj_p150_ul", "ferritin_unadj_p200_r",
      "ferritin_unadj_p200_se", "ferritin_unadj_p200_ll", "ferritin_unadj_p200_ul",
      "ferritin_unadj_p500_r", "ferritin_unadj_p500_se", "ferritin_unadj_p500_ll",
      "ferritin_unadj_p500_ul", "ferritin_adj_pop", "ferritin_adj_unwpop",
      "ferritin_adj_mean", "ferritin_adj_mean_sd", "ferritin_adj_mean_ll",
      "ferritin_adj_mean_ul", "ferritin_adj_geomean", "ferritin_adj_geomean_ll",
      "ferritin_adj_geomean_ul", "ferritin_adj_10percentile", "ferritin_adj_25percentile",
      "ferritin_adj_50percentile", "ferritin_adj_75percentile", "ferritin_adj_90percentile",
      "ferritin_adj_p5_r", "ferritin_adj_p5_se", "ferritin_adj_p5_ll",
      "ferritin_adj_p5_ul", "ferritin_adj_p10_r", "ferritin_adj_p10_se",
      "ferritin_adj_p10_ll", "ferritin_adj_p10_ul", "ferritin_adj_p12_r",
      "ferritin_adj_p12_se", "ferritin_adj_p12_ll", "ferritin_adj_p12_ul",
      "ferritin_adj_p15_r", "ferritin_adj_p15_se", "ferritin_adj_p15_ll",
      "ferritin_adj_p15_ul", "ferritin_adj_p30_r", "ferritin_adj_p30_se",
      "ferritin_adj_p30_ll", "ferritin_adj_p30_ul", "ferritin_adj_p70_r",
      "ferritin_adj_p70_se", "ferritin_adj_p70_ll", "ferritin_adj_p70_ul",
      "ferritin_adj_p100_r", "ferritin_adj_p100_se", "ferritin_adj_p100_ll",
      "ferritin_adj_p100_ul", "ferritin_adj_p150_r", "ferritin_adj_p150_se",
      "ferritin_adj_p150_ll", "ferritin_adj_p150_ul", "ferritin_adj_p200_r",
      "ferritin_adj_p200_se", "ferritin_adj_p200_ll", "ferritin_adj_p200_ul",
      "ferritin_adj_p500_r", "ferritin_adj_p500_se", "ferritin_adj_p500_ll",
      "ferritin_adj_p500_ul", "hgb_pop", "hgb_unwpop", "hgb_mean",
      "hgb_mean_sd", "hgb_mean_ll", "hgb_mean_ul", "hgb_geomean", "hgb_geomean_ll",
      "hgb_geomean_ul", "hgb_10percentile", "hgb_25percentile", "hgb_50percentile",
      "hgb_75percentile", "hgb_90percentile", "hgb_p180_r", "hgb_p180_se",
      "hgb_p180_ll", "hgb_p180_ul", "hgb_p175_r", "hgb_p175_se", "hgb_p175_ll",
      "hgb_p175_ul", "hgb_p170_r", "hgb_p170_se", "hgb_p170_ll", "hgb_p170_ul",
      "hgb_p165_r", "hgb_p165_se", "hgb_p165_ll", "hgb_p165_ul", "hgb_p160_r",
      "hgb_p160_se", "hgb_p160_ll", "hgb_p160_ul", "hgb_p155_r", "hgb_p155_se",
      "hgb_p155_ll", "hgb_p155_ul", "hgb_p150_r", "hgb_p150_se", "hgb_p150_ll",
      "hgb_p150_ul", "hgb_p145_r", "hgb_p145_se", "hgb_p145_ll", "hgb_p145_ul",
      "hgb_p140_r", "hgb_p140_se", "hgb_p140_ll", "hgb_p140_ul", "hgb_p135_r",
      "hgb_p135_se", "hgb_p135_ll", "hgb_p135_ul", "hgb_p130_r", "hgb_p130_se",
      "hgb_p130_ll", "hgb_p130_ul", "hgb_p125_r", "hgb_p125_se", "hgb_p125_ll",
      "hgb_p125_ul", "hgb_p120_r", "hgb_p120_se", "hgb_p120_ll", "hgb_p120_ul",
      "hgb_p115_r", "hgb_p115_se", "hgb_p115_ll", "hgb_p115_ul", "hgb_p110_r",
      "hgb_p110_se", "hgb_p110_ll", "hgb_p110_ul", "hgb_p105_r", "hgb_p105_se",
      "hgb_p105_ll", "hgb_p105_ul", "hgb_p100_r", "hgb_p100_se", "hgb_p100_ll",
      "hgb_p100_ul", "hgb_p95_r", "hgb_p95_se", "hgb_p95_ll", "hgb_p95_ul",
      "hgb_p90_r", "hgb_p90_se", "hgb_p90_ll", "hgb_p90_ul", "hgb_p85_r",
      "hgb_p85_se", "hgb_p85_ll", "hgb_p85_ul", "hgb_p80_r", "hgb_p80_se",
      "hgb_p80_ll", "hgb_p80_ul", "hgb_p75_r", "hgb_p75_se", "hgb_p75_ll",
      "hgb_p75_ul", "hgb_p70_r", "hgb_p70_se", "hgb_p70_ll", "hgb_p70_ul",
      "hgb_p65_r", "hgb_p65_se", "hgb_p65_ll", "hgb_p65_ul", "hgb_p60_r",
      "hgb_p60_se", "hgb_p60_ll", "hgb_p60_ul", "hgb_p55_r", "hgb_p55_se",
      "hgb_p55_ll", "hgb_p55_ul", "hgb_p50_r", "hgb_p50_se", "hgb_p50_ll",
      "hgb_p50_ul", "hgb_p45_r", "hgb_p45_se", "hgb_p45_ll", "hgb_p45_ul",
      "hgb_p40_r", "hgb_p40_se", "hgb_p40_ll", "hgb_p40_ul"
    )
  )
})

test_that("short prevalence", {
  testdata <- random_datset(100)
  res <- prevalence_short_format(
    indicators = list(
      indicator_iodine(),
      indicator_ferritin(),
      indicator_ferritin(adjustment_ferritin_implausible()),
      indicator_anaemia(),
      indicator_ida()
    ),
    age = testdata$age_years,
    sex = testdata$sex,
    pregnancy_status = testdata$pregnancy_status,
    lactating_status = testdata$lactating_status,
    ferritin = testdata$ferritin_measurement,
    iodine = testdata$iodine,
    CRP = testdata$crp_measurement,
    AGP = testdata$agp_measurement,
    haemoglobin = testdata$haemoglobin_measurement,
    is_smoker = testdata$is_smoker,
    altitude = testdata$altitude,
    smokes_cigarettes_per_day = testdata$smokes_cigarettes_per_day,
    pregnancyweeks = testdata$pregnancyweeks,
    pregnancymonths = testdata$pregnancymonths,
    cluster = 1:nrow(testdata),
    strata = rep.int(1, nrow(testdata))
  )
  expect_true(is.data.frame(res))
  expect_true(nrow(res) > 0)
  expect_setequal(
    colnames(res),
    c(
      "Group", "age_start", "age_end", "age_unit", "iodine_pop",
      "iodine_unwpop", "iodine_median_r", "iodine_25percentile", "iodine_75percentile",
      "ferritin_unadj_pop", "ferritin_unadj_unwpop", "ferritin_unadj_mean",
      "ferritin_unadj_mean_sd", "ferritin_unadj_mean_ll", "ferritin_unadj_mean_ul",
      "ferritin_unadj_25percentile", "ferritin_unadj_50percentile",
      "ferritin_unadj_75percentile", "depletedironstores_unadj_r",
      "depletedironstores_unadj_ll", "depletedironstores_unadj_ul",
      "riskofironoverload_unadj_r", "riskofironoverload_unadj_ll",
      "riskofironoverload_unadj_ul", "ferritin_adj_pop", "ferritin_adj_unwpop",
      "ferritin_adj_mean", "ferritin_adj_mean_sd", "ferritin_adj_mean_ll",
      "ferritin_adj_mean_ul", "ferritin_adj_25percentile", "ferritin_adj_50percentile",
      "ferritin_adj_75percentile", "depletedironstores_adj_r", "depletedironstores_adj_se",
      "depletedironstores_adj_ll", "depletedironstores_adj_ul", "riskofironoverload_adj_r",
      "riskofironoverload_adj_se", "riskofironoverload_adj_ll", "riskofironoverload_adj_ul",
      "hgb_pop", "hgb_unwpop", "hgb_mean", "hgb_mean_sd", "hgb_mean_ll",
      "hgb_mean_ul", "hgb_25percentile", "hgb_50percentile", "hgb_75percentile",
      "mildanaemia_r", "mildanaemia_se", "mildanaemia_ll", "mildanaemia_ul",
      "moderateanaemia_r", "moderateanaemia_se", "moderateanaemia_ll",
      "moderateanaemia_ul", "severeanaemia_r", "severeanaemia_se",
      "severeanaemia_ll", "severeanaemia_ul", "totalanaemia_r", "totalanaemia_se",
      "totalanaemia_ll", "totalanaemia_ul", "ida_unadj_pop", "ida_unadj_unwpop",
      "ida_unadj_r", "ida_unadj_se", "ida_unadj_ll", "ida_unadj_ul"
    )
  )
})
