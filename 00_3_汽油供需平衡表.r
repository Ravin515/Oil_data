library(styleer)
library(xlsx)
ld(net_production_weekly, force = T)
ld(gasoline_sales_weekly, force = T)
ld(gasoline_stock_weekly, force = T)
ld(export_import_weekly, force = T)

# 成品汽油产量
gasoline_net_production_weekly_table <- net_production_weekly[f == "W"& str_detect(name, "Finished Motor Gasoline") &(str_detect(name, "PADD|Adjusted")), .SD
    ][order(area, -date), .SD # 提取汽油以及各地区和Adjusted美国总产量
    ][, last_value := shift(value, type = "lead"), by = .(area) # 前一周的数据
    ][, delta_value_week := value - last_value# 周度变化量
    # ][, delta_value_month := value - shift(value, type = 'lead', n = 4L), by = .(area)
    ][, value_aver_4weeks := frollmean(value, 4, align = "left"), by = .(area)
    ][, delta_value_year := value_aver_4weeks - shift(value_aver_4weeks, type = "lead", n = 52L), by = .(area)
    ][, value_aver_4weeks := NULL
    ][, .SD[1], by = .(area)
    ][order(area), .SD
    ][, tag := "production"
    ]

# 成品汽油及组分进口
gasoline_import_weekly_table <- export_import_weekly[f == "W" & str_detect(name, "Total Gasoline| Gasoline Blending Components|Finished Motor Gasoline"), .SD
    ][str_detect(name, "PADD 1|PADD 2|PADD 3|PADD 4|PADD 5|U.S."), .SD
    ][!str_detect(name, "Conventional|Reformulated|RBOB"), .SD
    ][!(str_detect(name, "PADD") & str_detect(name, "Blending Components|Finished")), .SD
    ][!str_detect(name, 'Exports'), .SD
    ][, area := gsub("(.+) Imports (.+)", "\\1", name)
    ][, area := fifelse(area == "U.S.", area, str_extract(area, "\\(.+\\)") %>% str_replace_all("\\(|\\)", ""))
    ][order(area, type, -date), .SD
    ][, type := NULL
    ][, last_value := shift(value, type = 'lead'), by = .(name, area)
    ][, delta_value_week := value - last_value# 周度变化量
    # ][, delta_value_month := value - shift(value, type = 'lead', n = 4L), by = .(area, name)
    ][, value_aver_4weeks := frollmean(value, 4, align = "left"), by = .(name, area)
    ][, delta_value_year := value_aver_4weeks - shift(value_aver_4weeks, type = "lead", n = 52L), by = .(area)
    ][, value_aver_4weeks := NULL
    ][, .SD[1], by = .(area, name)
    ][order(area), .SD
    ][, tag := "imports"
    ]

# 成品油出口
gasoline_export_weekly_table <- export_import_weekly[f == "W" & str_detect(type, "Exports") & str_detect(name, "Gasoline"), .SD
    ][order(type, name, -date), .SD
    ][, type := NULL
    ][, last_value := shift(value, type = 'lead'), by = .(name)
    ][, delta_value_week := value - last_value# 周度变化量
    # ][, delta_value_month := value - shift(value, type = 'lead', n = 4L), by = .(name)
    ][, value_aver_4weeks := frollmean(value, 4, align = "left"), by = .(name)
    ][, delta_value_year := value_aver_4weeks - shift(value_aver_4weeks, type = "lead", n = 52L), by = .(name)
    ][, value_aver_4weeks := NULL
    ][, .SD[1], by = .(name)
    ][order(name), .SD
    ][, tag := "exports"
    ]

# 成品油消费
gasoline_sales_weekly_table <- gasoline_sales_weekly[f == "W" & str_detect(name, "Finished Motor Gasoline"), .SD
    ][order(name, -date), .SD
    ][, last_value := shift(value, type = 'lead'), by = .(name)
    ][, delta_value_week := value - last_value# 周度变化量
    # ][, delta_value_month := value - shift(value, type = 'lead', n = 4L), by = .(name)
    ][, value_aver_4weeks := frollmean(value, 4, align = "left"), by = .(name)
    ][, delta_value_year := value_aver_4weeks - shift(value_aver_4weeks, type = "lead", n = 52L), by = .(name)
    ][, value_aver_4weeks := NULL
    ][, .SD[1], by = .(name)
    ][order(name), .SD
    ][, tag := "sales"
    ]

# 成品油及组分库存(去除各州的调油组分库存)
gasoline_stock_weekly_table <- gasoline_stock_weekly[f == "W" & str_detect(name, "Total Gasoline| Gasoline Blending Components|Finished Motor Gasoline"), .SD
    ][str_detect(area, "PADD 1|PADD 2|PADD 3|PADD 4|PADD 5|U.S."), .SD
    ][!str_detect(name, "Conventional|Reformulated|RBOB"), .SD
    ][!(str_detect(name, "PADD") & str_detect(name, "Finished|Blending")), .SD
    ][order(name, -date), .SD
    ][, last_value := shift(value, type = 'lead'), by = .(name)
    ][, delta_value_week := value - last_value# 周度变化量
    # ][, delta_value_month := value - shift(value, type = 'lead', n = 4L), by = .(name)
    ][, value_aver_4weeks := frollmean(value, 4, align = "left"), by = .(name)
    ][, delta_value_year := value_aver_4weeks - shift(value_aver_4weeks, type = "lead", n = 52L), by = .(name)
    ][, value_aver_4weeks := NULL
    ][, .SD[1], by = .(name)
    ][order(area), .SD
    ][, tag := "stocks"
    ]

gasoline_balance_table <- list(gasoline_net_production_weekly_table, gasoline_import_weekly_table, gasoline_export_weekly_table, gasoline_sales_weekly_table, gasoline_stock_weekly_table) %>% rbindlist(fill = T, use.names = T)

gasoline_balance_table <- gasoline_balance_table[, .(tag,area, date, name, value, last_value, delta_value_week, delta_value_year)
    ][, setnames(.SD, c("value", "last_value"), c(unique(date)%>% as.character, (unique(date) - 7)%>% as.character))
    ]

fwrite(gasoline_balance_table, "gasoline_balance_table.csv")

# # 库存覆盖天数计算
# cover_days <- function(x) x[area == "U.S." & str_detect(name, "Stocks of Total Gasoline")]/x[tag == "sales"]
# gasoline_balance_table[, value[area == "U.S." & str_detect(name, "Stocks of Total Gasoline")]/value[tag == "sales"]]