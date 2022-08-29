library(styleer)
ld(net_production_weekly)

# 导出总体CBOB和RBOB的产量
CR_production <- net_production_weekly[str_detect(name, "Conventional|Reformulated") & str_detect(name, "U.S."), .SD
    ][f == "W"& !is.na(value), .SD
    ][, .(date, value, name)
    ][, dcast(.SD, name ~ date, var.value = "value")
    ]
fwrite(CR_production, "CR_production.csv")

# 导出整体产量四周平均和周产量
total_production <- net_production_weekly[str_detect(name, "Conventional|Reformulated") & str_detect(name, "U.S.") & !str_detect(name, "Finished"), .SD
    ][!is.na(value), .SD
    ][, .(date, value, name, f)
    ][, .(value = sum(value)), by = .(date, f)
    ][, dcast(.SD, f ~ date, var.value = "value")
    ]
fwrite(total_production, "total_production.csv")

# 导出区域产量四周平均和周产量
padd_gasoline_production <- net_production_weekly[str_detect(name, "PADD")&str_detect(name, "Finished Motor Gasoline|Conventional|Reformulated")&!str_detect(name, "Other|Ethanol|Ed"), .SD
    ][year >= 1994, dcast(.SD, area + name ~ date, value.var = "value")]
writexl::write_xlsx(padd_gasoline_production, "padd_gasoline_production.xlsx")

# 导出分区域CBOB和RBOB的产量
padd_cr <- net_production_weekly[str_detect(name, "Conventional|Reformulated") & str_detect(name, "PADD") & !str_detect(name, "Finished"), .SD
    ][f == "W" & year >= 2005, .(area, date, value, name)
    ][, dcast(.SD, name ~ date, var.value = "value")
    ]


# 炼厂与调油商CBOB和RBOB的产量
ld(refiner_net_production_weekly)
refiner_padd_cr_finished <- refiner_net_production_weekly[str_detect(name, "PADD") & str_detect(name, "Finished"), .SD    
    ][f == "W" & year >= 2005, .(date, value, name)
    ][, dcast(.SD, name ~ date, var.value = "value")
    ]
fwrite(refiner_padd_cr_finished, "refiner_padd_cr_finished.csv")

ld(blender_net_production_weekly)
blender_padd_cr_finished <- blender_net_production_weekly[str_detect(name, "PADD") & str_detect(name, "Finished"), .SD    
    ][f == "W" & year >= 2005, .(date, value, name)
    ][, dcast(.SD, name ~ date, var.value = "value")
    ]
fwrite(blender_padd_cr_finished, "blender_padd_cr_finished.csv")