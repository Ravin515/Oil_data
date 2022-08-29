library(styleer)

# 美国炼厂总体价格
ld(refiner_gasoline_price_monthly, force = T) # 导入炼厂出售汽油价格
refiner_gasoline_price_monthly_us <- refiner_gasoline_price_monthly[area == "U.S.",.SD
# ][, average_value := mean(value), by = .(date)
    ][, category := gsub("U\\.S\\. (.+) by.*", "\\1", name)
    ] # 提取全美数据
# retail sales = End Users(average) 零售 
# wholesale/Resale = Resale(average) 批发

ggplot(refiner_gasoline_price_monthly_us[str_detect(category, "Total Gasoline Retail Sales|Total Gasoline Wholesale/Resale"), .SD 
    ][year > 1989, .SD
    ][, value := value * 42], 
    aes(x = date, y = value, colour = category, fill = category, order = category)) +
  geom_line(size = 1) +
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂汽油总体价格") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

ggsave("美国炼厂总体价格.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

ld(refiner_oil_cost_monthly, force = T) # 导入炼厂原油获得价格
refiner_oil_cost_monthly_us <- refiner_oil_cost_monthly[, area := gsub("(.+) Crude .+", "\\1", name)
    ][area == "U.S.", .SD
    ] # 提取全美数据
# composite 自身合成
# domestic 国内原油
# imported 国外原油

ggplot(refiner_oil_cost_monthly_us[year > 1989, .SD], 
    aes(x = date, y = value, colour = channel, fill = channel, order = channel)) +
  geom_line(size = 1) +
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂原油成本价格") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

ggsave("美国炼厂原油成本价格.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# 竖着拼
refiner_sales_cost <- list(refiner_oil_cost_monthly_us [, setnames(.SD, "channel", "category")
    ][, .(value = mean(value), category = "average_cost"), by = .(date, year, month)
    ][year > 1989, .SD
    ], 
refiner_gasoline_price_monthly_us[str_detect(category, "Total Gasoline Retail Sales|Total Gasoline Wholesale/Resale"), .SD 
    ][year > 1989, .SD
    ][, value := value * 42
    ][, .(value = mean(value), category = "average_price"), by = .(date, year, month)
    ]
    ) %>% rbindlist(fill = T, use.names = T)

refiner_sales_cost <- list(refiner_sales_cost[,.(value = value[category == "average_price"]-value[category == "average_cost"], category = "return") , by = .(date, year, month)], refiner_sales_cost) %>% rbindlist(fill = T, use.names = T)

ld(refiner_petroleum_price_monthly, force = T)
# refiner_petroleum_price[str_detect(sale_type, "No. 2 Fuel Oil"), unique(sale_type)]
refiner_heating_price <- refiner_petroleum_price_monthly[str_detect(sale_type, "No. 2 Distillate")
    ][, .(value = mean(value, na.rm = T)*42, category = "No.2 Distillate"), by = .(date, year, month)]

refiner_sales_cost <- rbindlist(list(refiner_heating_price, refiner_sales_cost), use.names = T, fill = T) 

# 横着拼
# refiner_sales_cost <- refiner_oil_cost_monthly_us [, setnames(.SD, "channel", "category")
#     ][, .(average_cost = mean(value)), by = .(date, year)
#     ][year > 1989, .SD
#     ][refiner_gasoline_price_monthly_us[str_detect(category, "Total Gasoline Retail Sales|Total Gasoline Wholesale/Resale"), .SD 
#     ][year > 1989, .SD
#     ][, value := value * 42
#     ][, .(average_price = mean(value)), by = .(date, year)
#     ], on = .(date, year)
#     ][, return := average_price - average_cost
#     ]

# 成本-价格
ggplot(refiner_sales_cost[year > 1989, .SD], 
    aes(x = date, y = value, colour = category, fill = category, order = category)) +
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂汽油成本价格") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

ggsave("美国炼厂汽油成本-价格.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# 汽油裂解价差季节性图
ggplot(refiner_sales_cost[year > 2011 & category == "return", .SD], 
    aes(x = month, y = value, colour = factor(year), fill = year, order = year)) +
# ggplot(refiner_sales_cost[year > 2019 & category == "return", .SD], aes(x = date, y = value))+
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂汽油裂解价差") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

ggsave("美国炼厂汽油裂解价差.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# 美国炼厂3:2:1裂解价差
ggplot(refiner_sales_cost[year > 2011, .SD
    ][, .(value = value[category == "No.2 Distillate"]+ value[category == "average_price"]*2 - value[category == "average_cost"]*3), by = .(date, month, year)
    ], 
    aes(x = month, y = value, colour = factor(year), fill = year, order = year)) +
# ggplot(refiner_sales_cost[year > 2019 & category == "return", .SD], aes(x = date, y = value))+
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂3:2:1裂解价差") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

  ggsave("美国炼厂裂解价差.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# NYMEX盘面3：2：1裂解价差
ld(NYMEX_future_price_daily, force = T)
NYMEX_future_price_daily <- NYMEX_future_price_daily[year > 2011 & str_detect(name, "Contract 1"), .SD
    ][, value := fifelse(units == "Dollars per Gallon", value*42, value)
    ][, category := fcase(
        str_detect(name, "Heating Oil"),"Diesel",
        str_detect(name, "Gasoline"), "Gasoline",
        str_detect(name, "Crude Oil"), "Crude Oil"
    )
    ][, month_day := format(date, "%m-%d")
    ]


ggplot(NYMEX_future_price_daily[, .(value = value[category == "Diesel"] + value[category == "Gasoline"]*2 - value[category == "Crude Oil"]*3), by = .(date, month_day, year)], 
    aes(x = month_day, group = factor(year),y = value, colour = factor(year), fill = year, order = year)) +
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "NYMEX3:2:1裂解价差") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("NYMEX裂解价差.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# NYMEX汽油裂解价差季节图
ggplot(NYMEX_future_price_daily[, .(value = value[category == "Gasoline"] - value[category == "Crude Oil"]), by = .(date, month_day, year)], 
    aes(x = month_day, group = factor(year),y = value, colour = factor(year), fill = year, order = year)) +
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "NYMEX汽油裂解价差") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("NYMEX汽油裂解价差.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

  # NYMEX柴油裂解价差季节图
ggplot(NYMEX_future_price_daily[, .(value = value[category == "Diesel"] - value[category == "Crude Oil"]), by = .(date, month_day, year)], 
    aes(x = month_day, group = factor(year),y = value, colour = factor(year), fill = year, order = year)) +
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "NYMEX柴油裂解价差") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("NYMEX汽油裂解价差.tiff", device = "tiff", dpi = 300, width = 5.5, height = 4)

# 备成品油切换
# 各产品价格趋势
ld(refiner_gasoline_price_monthly, force = T)
ld(refiner_petroleum_price_monthly, force = T)

# 1.Gasoline部分
# retail sales = End Users(average) 零售 
# wholesale/Resale = Resale(average) 批发
refiner_gasoline_price_monthly_us <- refiner_gasoline_price_monthly[area == "U.S." & str_detect(name, "Total Gasoline Wholesale/Resale"), .SD
    ][, .(value = mean(value), category = "Gasoline"), by = .(date, name, year, month)
    ]

# 2.其他部分
# End Users(零售)
# Resale(批发)
refiner_petroleum_price_monthly_us <- refiner_petroleum_price_monthly[str_detect(name, "U.S."), .SD
    ][, category := gsub(".+ - (.+)", "\\1", sale_type)
    ][str_detect(sale_type, "Resale"), .(value = mean(value, na.rm = T)), by = .(date, category, name, year, month)
    ]

refiner_product_price_us <- rbindlist(list(refiner_gasoline_price_monthly_us, refiner_petroleum_price_monthly_us), use.name = T, fill = T)

ggplot(refiner_product_price_us[year > 2011 & !str_detect(category, "Residual|Aviation Gasoline|No. 4|Jet|No. 1|Distillate|Sulfur"), .SD
    ][, value := value * 42], 
    aes(x = date, group = factor(category),y = value, colour = factor(category), fill = category, order = category)) +
  geom_line(size = 0.8)+
  theme_grey() +
  labs(x = "时间", y = "美元/桶", title = "美国炼厂产品价格变化") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

  ggsave("美国炼厂产品价格变化.tiff", device = "tiff", dpi = 300, width = 10, height = 6)


# 收率
ld(refiner_yield, force = T)
refiner_yield_us <- refiner_yield[!str_detect(type, "Processing Gain"), .SD
    ][str_detect(name, "U.S."), .SD
    ][value > 3 & type != "Residual Fuel Oil", .SD
    ]

ggplot(refiner_yield_us[year > 2011, .SD], 
    aes(x = date, group = factor(type),y = value, colour = factor(type), fill = type, order = type)) +
  geom_line(size = 1)+
  theme_grey() +
  labs(x = "时间", y = "%", title = "美国炼厂收率") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "horizontal",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )

  ggsave("美国炼厂收率.tiff", device = "tiff", dpi = 300, width = 7, height = 4)

# 美国各地区及全美汽油收率
# 全美
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "U.S."), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-全美") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——全美.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

# PADD1汽油收率
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 1"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-PADD1") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——PADD1.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

# PADD2汽油收率
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 2"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-PADD2") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——PADD2.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # PADD3汽油收率
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 3"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-PADD3") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——PADD3.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

# PADD4汽油收率
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 4"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-PADD4") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——PADD4.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # PADD5汽油收率
ggplot(refiner_yield[year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 5"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(month)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 3, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = month, y = value, colour = year)) +
  geom_ribbon(aes(x = month, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂汽油收率-PADD5") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂汽油收率——PADD5.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # 美国炼厂开工
ld(refiner_utilization_weekly, force = T)
refiner_utilization_weekly <- refiner_utilization_weekly[str_detect(name, "Utilization") & f == "W", unique(.SD)]

# 全美
ggplot(refiner_utilization_weekly[year > 2011 & area == "U.S.", .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  # ][, week := format(date, format = "%b-%d")
  ][year >= max(year) - 2, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-全美") +
  # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂开工率——全美.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

# PADD1
ggplot(refiner_utilization_weekly[year > 2011 & str_detect(area, "PADD 1"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  ][year >= max(year) - 2 , .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-PADD1(占全美总炼油能力5%)") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂开工率——PADD1.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # PADD2
ggplot(refiner_utilization_weekly[year > 2011 & str_detect(area, "PADD 2"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  ][year >= max(year) - 2, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-PADD2(占全美总炼油能力23%)") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
ggsave("炼厂开工率——PADD2.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # PADD3
ggplot(refiner_utilization_weekly[year > 2011 & str_detect(area, "PADD 3"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  ][year >= max(year - 2), .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-PADD3(占全美总炼油能力53%)") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
ggsave("炼厂开工率——PADD3.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # PADD4
ggplot(refiner_utilization_weekly[year > 2011 & str_detect(area, "PADD 4"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  ][year >= max(year) - 2, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-PADD4(占全美总炼油能力4%)") +
  theme(
    plot.title = element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
ggsave("炼厂开工率——PADD4.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

# PADD5
ggplot(refiner_utilization_weekly[year > 2011 & str_detect(area, "PADD 5"), .SD
  ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week)
  ][year >= max(year) - 2, .SD
  ][, year := as.factor(year)]) +
  geom_line(size = 1, aes(x = week, y = value, colour = year)) +
  geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "%", title = "炼厂开工率-PADD5(占全美总炼油能力15%)") +
  theme(
    plot.title =  element_text(size = rel(1.3), hjust = 0.5),
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    # legend.box = "horizontal",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.5, 'cm')
  )
  ggsave("炼厂开工率——PADD5.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

  # 收率与API的分布(汽油+航煤)
  ld(refiner_yield)
  ld(refiner_input_crude_value_monthly)
  refiner_yield_gj <- refiner_yield[str_detect(name, "PADD") & str_detect(name, "Finished Motor Gasoline|Jet"), .SD
  ][, area := str_extract(name, "\\(.+\\)")
  ][, area := str_replace_all(area, "\\(|\\)", "")
  ][, yield := sum(value), by = .(area, date)
  ][, .(area, type = "yield", date, value = yield, units)
  ][, unique(.SD)]

  refiner_crude_api <- refiner_input_crude_value_monthly[str_detect(name, "PADD") & str_detect(name, "API"), .SD
  ][, type := "API"
  ][, .(area, type, date, value, units)]

refiner_yield_api <- list(refiner_yield_gj, refiner_crude_api) %>% rbindlist() %>% dcast(area + type ~ date, var.value = "value")

writexl::write_xlsx(refiner_yield_api, "refiner_yield_api.xlsx")

