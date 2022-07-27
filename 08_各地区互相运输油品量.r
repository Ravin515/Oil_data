library(styleer)
ld(transport_between_padd_monthly, force = T)
transport_between_padd_monthly <- transport_between_padd_monthly[, area := str_replace_all(area, "&nbsp;", "")
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][str_detect(name, "Crude|Finished Motor Gasoline|Blending Components|Jet|Distillate"), .SD
    ][, type := fcase(str_detect(name, "Crude"), "Crude",
        str_detect(name, "Finished Motor Gasoline"), "Finished Motor Gasoline",
        str_detect(name, "Blending Components"), "Blending Components",
        str_detect(name, "Jet"), "Kerosene",
        str_detect(name, "Distillate"), "Distillate"
    )
    ][, from_all := sum(value, na.rm = T), by = .(from, date, type) 
    ][, to_all := sum(value, na.rm = T), by = .(to, date, type)]

transport_from_all_monthly <- transport_between_padd_monthly[, .(from, from_all, date, type, year)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, from + type ~ date, value.var = "from_all")]
writexl::write_xlsx(transport_from_all_monthly, "transport_from_all_monthly.xlsx")

transport_to_all_monthly <- transport_between_padd_monthly[, .(to, to_all, date, type, year)
    ][, unique(.SD)
    ][year >= 2011, dcast(.SD, to + type ~ date, value.var = "to_all")]
writexl::write_xlsx(transport_to_all_monthly, "transport_to_all_monthly.xlsx")