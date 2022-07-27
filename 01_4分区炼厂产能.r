library(styleer)
ld(refiner_capacity_annual, force = T)

# 分区域CDU产能
refiner_capacity_CDU_annual <- refiner_capacity_annual[str_detect(name, "PADD")&str_detect(name, "Distillation Capacity")&str_detect(name, "Operating")&units == "Barrels per Calendar Day"
    ][, ':='(value = nafill(value, type = "locf"), id = 1:.N), by = .(area)
    ][, .(area, date, value = value/10000, name)
    ][, dcast(.SD, area ~ date, value.var = "value")]

fwrite(refiner_capacity_CDU_annual, "refiner_capacity_CDU_annual.csv")

# 分区域CDU装置数量
refiner_num_CDU_annual <- refiner_capacity_annual[str_detect(name, "Number")&str_detect(name, "PADD") &str_detect(name, "Operating|Operable"), .SD
    ][, tag := fifelse(str_detect(name, "Operable"), "可运行CDU装置", "在运行CDU装置")
    ][, ':='(value = nafill(value, type = "locf")), by = .(area)
    ][, .(area, date, value, tag)
    ][tag == "在运行CDU装置", dcast(.SD, area ~ date, value.var = "value")]
fwrite(refiner_num_CDU_annual, "refiner_num_CDU_annual.csv")

# 分区域汽油收率
ld(refiner_yield)
refiner_gasoline_yield <- refiner_yield[str_detect(name, "PADD|U.S.")&type == "Finished Motor Gasoline" |type == "Distillate Fuel Oil"|type == "Kerosene", .SD
    ][, area := fcase(str_detect(name, "1"), "PADD 1", str_detect(name, "2"), "PADD 2", str_detect(name, "3"), "PADD 3", str_detect(name, "4"), "PADD 4", str_detect(name, "5"), "PADD 5",  default = "U.S.")
    ][year == 2021, .(value = mean(value, na.rm = T)), by = .(year, area, type)
    ][, dcast(.SD, area ~ type, value.var = "value")
    ]
fwrite(refiner_gasoline_yield, "refiner_gasoline_yield.csv")

# PADD3中汽柴收率
ld(refiner_yield)
refiner_gasoline_PADD3_yield <- refiner_yield[str_detect(name, "PADD|U.S.|Texas Inland|Texas Gulf Coast|Louisiana Gulf Coast|North Louisiana-Arkansas|New Mexico")&type == "Finished Motor Gasoline" |type == "Distillate Fuel Oil"|type == "Kerosene", .SD
    ][, area := fcase(str_detect(name, "Texas Inland"), "德州内陆", str_detect(name, "Texas Gulf Coast"), "德州湾区", str_detect(name, "3"), "PADD 3", str_detect(name, "Louisiana Gulf Coast"), "路易斯安纳州湾区", str_detect(name, "North Louisiana-Arkansas"), "北路易斯安纳-阿肯色",  str_detect(name, "New Mexico"), "新墨西哥州",default = "U.S.")
    ][year == 2021, .(value = mean(value, na.rm = T)), by = .(year, area, type)
    ][, dcast(.SD, area ~ type, value.var = "value")
    ]
fwrite(refiner_gasoline_PADD3_yield, "refiner_gasoline_PADD3_yield.csv")

# PADD3中炼厂原油处理能力
ld(refiner_utilization_capacity_monthly)
refiner_utilization_capacity_PADD3_monthly <- refiner_utilization_capacity_monthly[str_detect(name, "PADD 3|U.S.|Texas|Louisiana Gulf Coast|North Louisiana-Arkansas|New Mexico")&str_detect(name, "Gross"), .SD
    ][, area := fcase(str_detect(name, "Texas Inland"), "德州内陆", str_detect(name, "Texas Gulf Coast"), "德州湾区", str_detect(name, "3"), "PADD 3", str_detect(name, "Louisiana Gulf Coast"), "路易斯安纳州湾区", str_detect(name, "North Louisiana-Arkansas"), "北路易斯安纳-阿肯色",  str_detect(name, "New Mexico"), "新墨西哥州",default = "U.S.")
    ][year %in% 2020:2022, .(value = mean(value, na.rm = T)), by = .(year, area)
    ][, dcast(.SD, area ~ year, value.var = "value")
    ]
fwrite(refiner_utilization_capacity_PADD3_monthly, "refiner_utilization_capacity_PADD3_monthly.csv")

# 各区炼厂原油加工能力
ld(refiner_utilization_capacity_monthly)
refiner_uc_monthly <- refiner_utilization_capacity_monthly[str_detect(area, "U.S.|PADD"), type := fcase(str_detect(name, "Gross"), "原油加工量",
    str_detect(name, "Idle"), "闲置产能",
    str_detect(name, "Operating"), "运转产能",
    str_detect(name, "Operable Crude"), "可运行产能",
    str_detect(name, "Percent"), "开工率"
)
# ][order(area, type, year, month), .SD
# ][, value_yoy := fifelse(type == "开工率",value-shift(value), value), by = .(name, area, month) # 去除季节性
# ][order(area, type, date), .SD
][!is.na(type) & year >= 2011, dcast(.SD, area + type ~ date, value.var = "value")]

writexl::write_xlsx(refiner_uc_monthly, "refiner_uc_monthly.xlsx")