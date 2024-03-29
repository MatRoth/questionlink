


test_that("Replication of Kolen & Brennan 2014, chapter 2.5 table 2.2",{
  cur_target_ftab<- tibble::tribble(
    ~ response, ~cum_rel_freq,
    0, 0.1,
    1, 0.3,
    2, 0.5,
    3, 0.8,
    4, 1)

  cur_source_ftab <- tibble::tribble(
    ~ response, ~cum_rel_freq,
    0, 0.2,
    1, 0.5,
    2, 0.7,
    3, 0.9,
    4, 1)

  result <- tibble::tribble(
    ~source,~target,
    0,0.5,
    1,1.75,
    2,2.8333,
    3,3.5,
    4,4.25
  )
  expect_equal(object = harmonize_equip_R(x = cur_source_ftab$response-min(cur_source_ftab$response), #built in standardization to zero
                     src_cum_rel_freq = cur_source_ftab$cum_rel_freq,
                     trgt_response = cur_target_ftab$response-min(cur_target_ftab$response),
                     trgt_cum_rel_freq = cur_target_ftab$cum_rel_freq)+min(cur_target_ftab$response),
               expected = result$target,
               tolerance = 1e-4)})

test_that("Replication of Kolen & Brennan 2014, chapter 2.7",{

  act_data<-tibble::tribble(
    ~"Raw score",~"ˆng(y)",~"ˆnG(y)",~"ˆg(y)",~"ˆG(y)",~"ˆQ(y)",~"ˆnf,(x)",~"ˆnF(x)",~"ˆf,(x)",~"ˆF(x)",~"ˆP(x)",
    0,0,0,.0000,.0000,.00,0,0,.0000,.0000,.00,
    1,1,1,.0002,.0002,.01,1,1,.0002,.0002,.01,
    2,3,4,.0007,.0010,.06,1,2,.0002,.0005,.03,
    3,13,17,.0031,.0041,.25,3,5,.0007,.0012,.08,
    4,42,59,.0101,.0142,.92,9,14,.0021,.0032,.22,
    5,59,118,.0142,.0284,2.13,18,32,.0042,.0074,.53,
    6,95,213,.0229,.0513,3.99,59,91,.0136,.0210,1.42,
    7,131,344,.0316,.0829,6.71,67,158,.0155,.0365,2.88,
    8,158,502,.0381,.1209,10.19,91,249,.0210,.0575,4.70,
    9,161,663,.0388,.1597,14.03,144,393,.0333,.0908,7.42,
    10,194,857,.0467,.2064,18.30,149,542,.0344,.1252,10.80,
    11,164,1021,.0395,.2459,22.62,192,734,.0444,.1696,14.74,
    12,166,1187,.0400,.2859,26.59,192,926,.0444,.2139,19.17,
    13,197,384,.0474,.3333,30.96,192,1118,.0444,.2583,23.61,
    14,177,561,.0426,.3760,35.46,201,1319,.0464,.3047,28.15,
    15,158,1719,.0381,.4140,39.50,204,1523,.0471,.3518,32.83,
    16,169,1888,.0407,.4547,43.44,217,1740,.0501,.4019,37.69,
    17,132,2020,.0318,.4865,47.06,181,1921,.0418,.4438,42.28,
    18,158,2178,.0381,.5246,50.55,184,2105,.0425,.4863,46.50,
    19,151,2329,.0364,.5609,54.28,170,2275,.0393,.5255,50.59,
    20,134,2463,.0323,.5932,57.71,201,2476,.0464,.5720,54.87,
    21,137,2600,.0330,.6262,60.97,147,2623,.0340,.6059,58.89,
    22,122,2722,.0294,.6556,64.09,163,2786,.0377,.6436,62.47,
    23,110,2832,.0265,.6821,66.88,147,2933,.0340,.6775,66.05,
    24,116,2948,.0279,.7100,69.61,140,3073,.0323,.7099,69.37,
    25,132,3080,.0318,.7418,72.59,147,3220,.0340,.7438,72.68,
    26,104,3184,.0250,.7669,75.43,126,3346,.0291,.7729,75.84,
    27,104,3288,.0250,.7919,77.94,113,3459,.0261,.7990,78.60,
    28,114,3402,.0275,.8194,80.56,100,3559,.0231,.8221,81.06,
    29,97,3499,.0234,.8427,83.10,106,3665,.0245,.8466,83.44,
    30,107,3606,.0258,.8685,85.56,107,3772,.0247,.8713,85.90,
    31,88,3694,.0212,.8897,87.91,91,3863,.0210,.8924,88.18,
    32,80,3774,.0193,.9090,89.93,83,3946,.0192,.9115,90.19,
    33,79,3853,.0190,.9280,91.85,73,4019,.0169,.9284,92.00,
    34,70,3923,.0169,.9448,93.64,72,4091,.0166,.9450,93.67,
    35,61,3984,.0147,.9595,95.22,75,4166,.0173,.9623,95.37,
    36,48,4032,.0116,.9711,96.53,50,4216,.0116,.9739,96.81,
    37,47,4079,.0113,.9824,97.68,37,4253,.0085,.9824,97.82,
    38,29,4108,.0070,.9894,98.59,38,4291,.0088,.9912,98.68,
    39,32,4140,.0077,.9971,99.33,23,4314,.0053,.9965,99.39,
    40,12,4152,.0029,1.0000,99.86,15,4329,.0035,1.000,99.83)

  act_equating_results <- tibble::tribble(
    ~"Score",~"Mean",~"Linear",~"Equipercentile",
    0,-.8726,-2.6319,.0000,
    1,.1274,-1.5432,.9796,
    2,1.1274,-.4546,1.6462,
    3,2.1274,.6340,2.2856,
    4,3.1274,1.7226,2.8932,
    5,4.1274,2.8112,3.6205,
    6,5.1274,3.8998,4.4997,
    7,6.1274,4.9884,5.5148,
    8,7.1274,6.0771,6.3124,
    9,8.1274,7.1657,7.2242,
    10,9.1274,8.2543,8.1607,
    11,10.1274,9.3429,9.1827,
    12,11.1274,10.4315,10.1859,
    13,12.1274,11.5201,11.2513,
    14,13.1274,12.6088,12.3896,
    15,14.1274,13.6974,13.3929,
    16,15.1274,14.7860,14.5240,
    17,16.1274,15.8746,15.7169,
    18,17.1274,16.9632,16.8234,
    19,18.1274,18.0518,18.0092,
    20,19.1274,19.1405,19.1647,
    21,20.1274,20.2291,20.3676,
    22,21.1274,21.3177,21.4556,
    23,22.1274,22.4063,22.6871,
    24,23.1274,23.4949,23.9157,
    25,24.1274,24.5835,25.0292,
    26,25.1274,25.6722,26.1612,
    27,26.1274,26.7608,27.2633,
    28,27.1274,27.8494,28.1801,
    29,28.1274,28.9380,29.1424,
    30,29.1274,30.0266,30.1305,
    31,30.1274,31.1152,31.1297,
    32,31.1274,32.2039,32.1357,
    33,32.1274,33.2925,33.0781,
    34,33.1274,34.3811,34.0172,
    35,34.1274,35.4697,35.1016,
    36,35.1274,36.5583,36.2426,
    37,36.1274,37.6469,37.1248,
    38,37.1274,38.7355,38.1321,
    39,38.1274,39.8242,39.0807,
    40,39.1274,40.9128,39.9006)

  cur_source_data <- tibble::tibble(question = "a",
                                    year = 1,
                                    population = "test",
                                    weight = 1,
                                    response = c(0,
                                                 rep(act_data$`Raw score`,
                                                     act_data$`ˆnf,(x)`)))
  cur_target_data <- tibble::tibble(question = "b",
                                    year = 1,
                                    population = "test",
                                    weight = 1,
                                    response = c(0,
                                                 rep(act_data$`Raw score`,
                                                     act_data$`ˆng(y)`)))

  cur_source_ftab <- cur_source_data |>
    get_frequency_tables() |>
    dplyr::mutate(data = purrr::map(data,
                                    enforce_full,
                                    "a",
                                    list("a" = c(0,40)))) |>
    dplyr::pull(data) |>
    purrr::flatten()
  cur_target_ftab <- cur_target_data |>
    get_frequency_tables() |>
    dplyr::mutate(data = purrr::map(data,
                                    enforce_full,
                                    "b",
                                    list("b" = c(0,40)))) |>
    dplyr::pull(data) |>
    purrr::flatten()

  result <- act_equating_results$Equipercentile
  expect_equal(
    object = harmonize_equip_R(x = cur_source_ftab$response-min(cur_source_ftab$response), #built in standardization to zero
                                   src_cum_rel_freq = cur_source_ftab$cum_rel_freq,
                                   trgt_response = cur_target_ftab$response-min(cur_target_ftab$response),
                                   trgt_cum_rel_freq = cur_target_ftab$cum_rel_freq)+min(cur_target_ftab$response),
               expected = result,
               tolerance = 1e-3)}) # tolerance is lower because Kolen & Brennan
                                   # use a variable minimum jump value for zero frequency responses
                                   # while we use a fixed 1e-6 as we expect scales with max 11 response options
                                   # to be harmonized while educational attainment test potentially have longer scales.
                                   # Harmonizations with common social science scales are unlikely to be impacted.



