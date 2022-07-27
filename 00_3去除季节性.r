library(styleer)
ld(refiner_petroleum_price_monthly)
# 汽柴油价格去除季节性：利用去除季节性方法
refiner_gdj_price_monthly <- refiner_petroleum_price_monthly[str_detect(name, "Total Gasoline|No 2 Distillate|Jet") & str_detect(name, "U.S.|PADD") & str_detect(name, "Resale") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, date), .SD
    ][, name := fcase(str_detect(name, "Total Gasoline"), "gasoline",
                    str_detect(name, "Distillate"), "distillate",
                    str_detect(name, "Jet"), "kerosene"
    )
    ][, value_12_aver := frollmean(value, 12, align = "right"), by = .(name, area)
    ][, value_12_2_aver := frollmean(value_12_aver, 2, align = "right"), by = .(name, area)
    ][, S_I := value/value_12_2_aver
    ][, S := mean(S_I, na.rm = T), by = .(month, name, area)
    ][, S := 12/sum(S), by = .(year, name, area)
    ][, value_without_season := value/S
    ][ year >= 2011, .(name, value_without_season, date, area)
    ][, dcast(.SD, date + area ~ name, value.var = "value_without_season")
    ][, ":="(g_d = gasoline-distillate, g_k = gasoline-kerosene)
    ][, melt(.SD, id.vars = c("date", "area"), measure.vars = c("distillate", "gasoline", "kerosene", "g_d", "g_k"))
    ][, dcast(.SD, area + variable ~ date, value.var = "value")]

writexl::write_xlsx(refiner_gdj_price_monthly, "refiner_gdj_price_monthly.xlsx")

# 汽柴油价格去除季节性：利用每年每月同比方法
refiner_yoy_price_monthly <- refiner_petroleum_price_monthly[str_detect(name, "Total Gasoline|No 2 Distillate|Jet") & str_detect(name, "U.S.|PADD") & str_detect(name, "Resale") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, month, year), .SD
    ][, name := fcase(str_detect(name, "Total Gasoline"), "gasoline",
                    str_detect(name, "Distillate"), "distillate",
                    str_detect(name, "Jet"), "kerosene"
    )
    ][, value_yoy := (value-shift(value))/shift(value), by = .(name, area, month)
    ][order(name, date), .SD
    ][year >= 2011, .(value_yoy, date, name, area)
    ][, dcast(.SD, date + area ~ name, value.var = "value_yoy")
    ][, ":="(g_d = gasoline-distillate, g_k = gasoline-kerosene)
    ][, melt(.SD, id.vars = c("date", "area"), measure.vars = c("distillate", "gasoline", "kerosene", "g_d", "g_k"))
    ][, dcast(.SD, area + variable ~ date, value.var = "value")]

writexl::write_xlsx(refiner_yoy_price_monthly, "refiner_yoy_price_monthly.xlsx")

# 汽柴油收率去除季节性：利用去除季节性方法
ld(refiner_yield)
refiner_gdj_yield <- refiner_yield[str_detect(name, "Finished Motor Gasoline|Distillate|Jet") & str_detect(name, "U.S.|PADD") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, date), .SD
    ][, area := fcase(str_detect(name, "U.S."), "U.S.",
        str_detect(name, "PADD 1"), "PADD 1",
        str_detect(name, "PADD 2"), "PADD 2",        
        str_detect(name, "PADD 3"), "PADD 3",
        str_detect(name, "PADD 4"), "PADD 4",
        str_detect(name, "PADD 5"), "PADD 5"
    )
    ][, value_12_aver := frollmean(value, 12, align = "right"), by = .(name, area)
    ][, value_12_2_aver := frollmean(value_12_aver, 2, align = "right"), by = .(name, area)
    ][, S_I := value/value_12_2_aver
    ][, S := mean(S_I, na.rm = T), by = .(month, name, area)
    ][, S := 12/sum(S), by = .(year, name, area)
    ][, value_without_season := value/S
    ][ year >= 2011, .(name, value = value_without_season, date, area)
    ][, dcast(.SD, area + name ~ date, value.var = "value")]

writexl::write_xlsx(refiner_gdj_yield, "refiner_gdj_yield.xlsx")

# 汽柴油收率去除季节性：利用每年每月同比方法
refiner_yoy_yield <- refiner_yield[str_detect(name, "Finished Motor Gasoline|Distillate|Jet") & str_detect(name, "U.S.|PADD") & !str_detect(name, "1A|1B|1C"), .SD
    ][order(name, month, year), .SD
    ][, area := fcase(str_detect(name, "U.S."), "U.S.",
        str_detect(name, "PADD 1"), "PADD 1",
        str_detect(name, "PADD 2"), "PADD 2",        
        str_detect(name, "PADD 3"), "PADD 3",
        str_detect(name, "PADD 4"), "PADD 4",
        str_detect(name, "PADD 5"), "PADD 5"
    )
    ][, name := fcase(str_detect(name, "Finished Motor Gasoline"), "gasoline",
                    str_detect(name, "Distillate"), "distillate",
                    str_detect(name, "Jet"), "kerosene"
    )
    ][, value_yoy := (value-shift(value))/shift(value), by = .(name, area, month)
    ][order(name, date), .SD
    ][year >= 2011, .(value_yoy, date, name, area)
    ][, dcast(.SD, date + area ~ name, value.var = "value_yoy")
    ][, ":="(g_d = gasoline-distillate, g_k = gasoline-kerosene)
    ][, melt(.SD, id.vars = c("date", "area"), measure.vars = c("distillate", "gasoline", "kerosene", "g_d", "g_k"))
    ][, dcast(.SD, area + variable ~ date, value.var = "value")
    ]

writexl::write_xlsx(refiner_yoy_yield, "refiner_yoy_yield.xlsx")
