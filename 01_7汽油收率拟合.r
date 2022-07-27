library(styleer)

# 价格数据
ld(refiner_petroleum_price_monthly, force = T)
refiner_gdj_price_monthly <- refiner_petroleum_price_monthly[str_detect(name, "Total Gasoline|No 2 Distillate|Jet") & str_detect(name, "U.S.|PADD") & str_detect(name, "Resale") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, date), .SD
    ][, name := fcase(str_detect(name, "Total Gasoline"), "gasoline_price",
                    str_detect(name, "Distillate"), "distillate_price",
                    str_detect(name, "Jet"), "kerosene_price"
    )
    ][, area := fifelse(area == "U.S.", area, str_sub(area, start = -7L, end = -2L))
    ][, f := NULL]

# 收率数据
ld(refiner_yield, force = T)
refiner_gdj_yield <- refiner_yield[str_detect(name, "Finished Motor Gasoline|Distillate|Jet") & str_detect(name, "U.S.|PADD") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, date), .SD
    ][, area := fcase(str_detect(name, "U.S."), "U.S.",
        str_detect(name, "PADD 1"), "PADD 1",
        str_detect(name, "PADD 2"), "PADD 2",        
        str_detect(name, "PADD 3"), "PADD 3",
        str_detect(name, "PADD 4"), "PADD 4",
        str_detect(name, "PADD 5"), "PADD 5"
    )
    ][, name := fcase(str_detect(name, "Finished Motor Gasoline"), "gasoline_yield",
        str_detect(name, "Distillate"), "distillate_yield",
        str_detect(name, "Jet"), "kerosene_yield"
    )][, type := NULL
    ][, f := NULL]

# API数据
ld(refiner_input_crude_value_monthly)
refiner_gdj_api <- refiner_input_crude_value_monthly[str_detect(name, "U.S.|PADD") & !str_detect(name, "1A|1B|1C") & str_detect(name, "API"), .SD
    ][order(name, date), .SD
    ][, name := "api"]

refiner_gdj_fit <- list(refiner_gdj_api, refiner_gdj_price_monthly, refiner_gdj_yield) %>% rbindlist(use.names = T) %>% dcast(area + date + month + year ~ name, value.var = "value")

refiner_gdj_fit <- refiner_gdj_fit[year >= 1993, .SD]

# # 计算季节性指数
# refiner_gdj_fit[, gasoline_yield_12_aver := frollmean(gasoline_yield, 12, align = "right"), by = .(area)
#     ][, gasoline_yield_12_2_aver := frollmean(gasoline_yield_12_aver, 2, align = "right"), by = .(area)
#     ][, S_I := gasoline_yield/gasoline_yield_12_2_aver
#     ][, S_aver := mean(S_I, na.rm = T), by = .( area, month)
#     ][, S := 12/sum(S_aver), by = .(area, year)
#     ]

# 回归模型 全美
refiner_gdj_fit <- refiner_gdj_fit[, ':='(g_d_price = gasoline_price - distillate_price, g_k_price = gasoline_price - kerosene_price)
    ][, str_c(c("api_lag"), 1:6) := shift(api, 1:6), by = .(area)
    ][, str_c(c("g_d_price_lag"), 1:6) := shift(g_d_price, 1:6), by = .(area)
    ][, str_c(c("g_k_price_lag"), 1:6) := shift(g_k_price, 1:6), by = .(area)
    ][order(area, month, year), .SD
    ][, gasoline_yield_month := gasoline_yield/shift(gasoline_yield)-1, by = .(area)
    ][, api_month := api/shift(api) - 1, by = .(area)
    # ][, gasoline_yield_did2 := gasoline_yield_did - shift(gasoline_yield_did), by = .(area)
    # ][, gasoline_yield_did_month := gasoline_yield/shift(gasoline_yield), by = .(area, month)
    ][, g_d_price_month := g_d_price/shift(g_d_price)-1, by = .(area, month)
    ][, g_k_price_month := g_k_price/shift(g_k_price)-1, by = .(area, month)
    ][order(area, date), .SD
    ][, g_d_price_month_lag1 := shift(g_d_price_month), by = .(area)
    ][, g_k_price_month_lag1 := shift(g_k_price_month), by = .(area)
    ][, api_month_lag1 := shift(api_month), by = .(area)]
refiner_gdj_fit[is.infinite(g_d_price_month_lag1)] <- NA
refiner_gdj_fit[is.infinite(g_k_price_month_lag1)] <- NA

# 一阶滞后
# fit1_lag1 <- refiner_gdj_fit[area != "U.S.",  .(lm(gasoline_yield ~ api_lag1 + g_d_price_lag1 + g_k_price_lag1 + factor(area) + factor(month) )%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield, api_lag1, g_d_price_lag1, g_k_price_lag1)] 

# fit2_lag1 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~ api_lag1 + g_d_price_lag1 + g_k_price_lag1 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag1, g_d_price_lag1, g_k_price_lag1)]

fit3_lag1 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_month ~ api_month_lag1 + g_d_price_month_lag1 + g_k_price_month_lag1 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_month, api_month_lag1, g_d_price_month_lag1, g_k_price_month_lag1)]

fit3_lag1 <- refiner_gdj_fit[area == "U.S.", lm(gasoline_yield_month ~ api_month_lag1 + g_d_price_month_lag1 + g_k_price_month_lag1 + factor(month))] %>% summary()

fit4_lag1 <- refiner_gdj_fit[area != "U.S.", .(lm(gasoline_yield_month ~ api_month_lag1 + g_d_price_month_lag1 + g_k_price_month_lag1 + factor(area) + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_month, api_month_lag1, g_d_price_month_lag1, g_k_price_month_lag1)]

fit4_lag1 <- refiner_gdj_fit[area != "U.S.", lm(gasoline_yield ~ api_lag1 + g_d_price_lag1 + g_k_price_lag1 + factor(area)+ factor(month))] %>% summary()

# 二阶滞后
fit1_lag2 <- refiner_gdj_fit[area != "U.S.", .(lm(gasoline_yield ~ api_lag2 + g_d_price_lag2 + g_k_price_lag2+ factor(area) + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield, api_lag2, g_d_price_lag2, g_k_price_lag2)]

fit2_lag2 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~ api_lag2 + g_d_price_lag2 + g_k_price_lag2 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag2, g_d_price_lag2, g_k_price_lag2)]

# 三阶滞后
# fit1 <- refiner_gdj_fit[area != "U.S.", lm(gasoline_yield ~ shift(api, 3) + shift(I(gasoline_price - distillate_price), 3) + shift(I(gasoline_price - kerosene_price), 3) + factor(area) + factor(month))] %>% summary()

fit2_lag3 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~ api_lag3 +  g_d_price_lag3 + g_k_price_lag3 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag3, g_d_price_lag3, g_k_price_lag3)]

# 四阶滞后
fit2_lag4 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~ api_lag4 + g_d_price_lag4 +  g_k_price_lag4 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag4, g_d_price_lag4, g_k_price_lag4)]

# 五阶滞后
fit2_lag5 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~ api_lag5 + g_d_price_lag5 + g_k_price_lag5 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag5, g_d_price_lag5, g_k_price_lag5)]

# 六阶滞后
fit2_lag6 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield_did2 ~  api_lag6 +  g_d_price_lag6 + g_k_price_lag6 + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield_did2, api_lag6, g_d_price_lag6, g_k_price_lag6)]

# 拟合
# fit1 <- refiner_gdj_fit[area != "U.S.", .(lm(gasoline_yield ~ api + I(gasoline_price - distillate_price) + I(gasoline_price - kerosene_price) + factor(area) + factor(month)) %>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield, api, gasoline_price, distillate_price, kerosene_price)] 

# fit2 <- refiner_gdj_fit[area == "U.S.", .(lm(gasoline_yield ~ shift(api) + shift(I(gasoline_price - distillate_price)) + shift(I(gasoline_price - kerosene_price)) + factor(month))%>% coef() %>% as.list() %>% as.data.table(), area, date, month, year, gasoline_yield, api, gasoline_price, distillate_price, kerosene_price)]

# 各地区
fit1_lag1[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
    ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
    ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
    ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
    ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
    ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
    ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
    ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
    ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
    ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
    ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
    ][, season := rowSums(.SD), .SDcols = 5:15
    ][, factor.area.PADD.2 := fifelse(area == "PADD 2", factor.area.PADD.2, 0)
    ][, factor.area.PADD.3 := fifelse(area == "PADD 3", factor.area.PADD.3, 0)
    ][, factor.area.PADD.4 := fifelse(area == "PADD 4", factor.area.PADD.4, 0)
    ][, factor.area.PADD.5 := fifelse(area == "PADD 5", factor.area.PADD.5, 0)
    ][, section := rowSums(.SD), .SDcols = 5:8
    ][, gasoline_yield_fit := X.Intercept. + api_lag1*api_lag1.1 + g_d_price_lag1*g_d_price_lag1.1 + g_k_price_lag1*g_k_price_lag1.1  + season + section]

gasoline_yield_fit_section <- fit1_lag1[, .(date, area, gasoline_yield, gasoline_yield_fit)
    ][, dcast(.SD, date ~ area, value.var = c("gasoline_yield", "gasoline_yield_fit"))]
writexl::write_xlsx(gasoline_yield_fit_section, 'gasoline_yield_fit_section.xlsx')

fit4_lag1[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
    ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
    ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
    ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
    ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
    ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
    ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
    ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
    ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
    ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
    ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
    ][, season := rowSums(.SD), .SDcols = 5:15
    ][, factor.area.PADD.2 := fifelse(area == "PADD 2", factor.area.PADD.2, 0)
    ][, factor.area.PADD.3 := fifelse(area == "PADD 3", factor.area.PADD.3, 0)
    ][, factor.area.PADD.4 := fifelse(area == "PADD 4", factor.area.PADD.4, 0)
    ][, factor.area.PADD.5 := fifelse(area == "PADD 5", factor.area.PADD.5, 0)
    ][, section := rowSums(.SD), .SDcols = 5:8
    ][, gasoline_yield_fit := X.Intercept. + api_month_lag1*api_month_lag1.1 + g_d_price_month_lag1*g_d_price_month_lag1.1 + g_k_price_month_lag1*g_k_price_month_lag1.1  + season + section]
gasoline_yield_fit_section_month <- fit4_lag1[, .(date, area, gasoline_yield_month, gasoline_yield_fit)
    ][, dcast(.SD, date ~ area, value.var = c("gasoline_yield_month", "gasoline_yield_fit"))]
writexl::write_xlsx(gasoline_yield_fit_section_month, 'gasoline_yield_fit_section_month.xlsx')

# 全美
fit3_lag1[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
    ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
    ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
    ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
    ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
    ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
    ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
    ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
    ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
    ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
    ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
    ][, season := rowSums(.SD), .SDcols = 5:15
    ][, gasoline_yield_fit := X.Intercept. + api_month_lag1*api_month_lag1.1 + g_d_price_month_lag1*g_d_price_month_lag1.1 + g_k_price_month_lag1*g_k_price_month_lag1.1 + season]

gasoline_yield_fit_us_month_lag1 <- fit3_lag1[, .(date, gasoline_yield_month, gasoline_yield_fit)]
writexl::write_xlsx(gasoline_yield_fit_us_month_lag1, 'gasoline_yield_fit_us_month_lag1.xlsx')    


# fit2_lag1[, factor.month.2 := fifelse(month == 2, factomonthr.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag1*api_lag1.1 + g_d_price_lag1*g_d_price_lag1.1 + g_k_price_lag1*g_k_price_lag1.1 + season]

# fit2_lag2[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag2*api_lag2.1 + g_d_price_lag2*g_d_price_lag2.1 + g_k_price_lag2*g_k_price_lag2.1 + season]

# fit2_lag3[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag3*api_lag3.1 + g_d_price_lag3*g_d_price_lag3.1 + g_k_price_lag3*g_k_price_lag3.1 + season]

# fit2_lag4[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag4*api_lag4.1 + g_d_price_lag4*g_d_price_lag4.1 + g_k_price_lag4*g_k_price_lag4.1 + season]

# fit2_lag5[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag5*api_lag5.1 + g_d_price_lag5*g_d_price_lag5.1 + g_k_price_lag5*g_k_price_lag5.1 + season]

# fit2_lag6[, factor.month.2 := fifelse(month == 2, factor.month.2, 0)
#     ][, factor.month.3 := fifelse(month == 3, factor.month.3, 0)
#     ][, factor.month.4 := fifelse(month == 4, factor.month.4, 0)
#     ][, factor.month.5 := fifelse(month == 5, factor.month.5, 0)
#     ][, factor.month.6 := fifelse(month == 6, factor.month.6, 0)
#     ][, factor.month.7 := fifelse(month == 7, factor.month.7, 0)
#     ][, factor.month.8 := fifelse(month == 8, factor.month.8, 0)
#     ][, factor.month.9 := fifelse(month == 9, factor.month.9, 0)
#     ][, factor.month.10 := fifelse(month == 10, factor.month.10, 0)
#     ][, factor.month.11 := fifelse(month == 11, factor.month.11, 0)
#     ][, factor.month.12 := fifelse(month == 12, factor.month.12, 0)
#     ][, season := rowSums(.SD), .SDcols = 5:15
#     ][, gasoline_yield_fit := X.Intercept. + api_lag6*api_lag6.1 + g_d_price_lag6*g_d_price_lag6.1 + g_k_price_lag6*g_k_price_lag6.1 + season]

# gasoline_yield_fit_us_lag1 <- fit2_lag1[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag1, 'gasoline_yield_fit_us_lag1.xlsx')
# gasoline_yield_fit_us_lag2 <- fit2_lag2[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag2, 'gasoline_yield_fit_us_lag2.xlsx')
# gasoline_yield_fit_us_lag3 <- fit2_lag3[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag3, 'gasoline_yield_fit_us_lag3.xlsx')
# gasoline_yield_fit_us_lag4 <- fit2_lag4[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag4, 'gasoline_yield_fit_us_lag4.xlsx')
# gasoline_yield_fit_us_lag5 <- fit2_lag5[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag5, 'gasoline_yield_fit_us_lag5.xlsx')
# gasoline_yield_fit_us_lag6 <- fit2_lag6[, .(date, gasoline_yield_did2, gasoline_yield_fit)]
# writexl::write_xlsx(gasoline_yield_fit_us_lag6, 'gasoline_yield_fit_us_lag6.xlsx')
