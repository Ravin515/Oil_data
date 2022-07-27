library(styleer)
library(writexl)
ld(refiner_input_crude_value_monthly)
# 导出API值
refiner_input_crude_api <- refiner_input_crude_value_monthly[str_detect(name, "API")&str_detect(name, "U.S.|PADD"), .SD
    ][, name := "API"
    ][order(name, month, year), .SD
    ][, value_yoy := (value-shift(value))/shift(value), by = .(name, area, month)
    ][order(name, date), .SD
    ][year >= 2011, .(area, date, value, value_yoy)
    ][, dcast(.SD, date ~ area, value.var = c("value", "value_yoy"))
    ]
write_xlsx(refiner_input_crude_api, "refiner_input_crude_api.xlsx")

# 各区域API值季节性图