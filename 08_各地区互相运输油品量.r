library(styleer)

# 1. 总体转运量
ld(transport_between_padd_monthly, force = T)
transport_between_padd_clean_monthly <- transport_between_padd_monthly[, area := str_replace_all(area, "&nbsp;", "")
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][str_detect(name, "Crude|Finished Motor Gasoline|Jet|Distillate"), .SD
    ][, type := fcase(str_detect(name, "Crude"), "Crude",
        str_detect(name, "Finished Motor Gasoline"), "Finished Motor Gasoline",
        str_detect(name, "Jet"), "Kerosene",
        str_detect(name, "Distillate"), "Distillate"
    )
    ][, from_all := sum(value, na.rm = T), by = .(from, date, type) 
    ][, to_all := sum(value, na.rm = T), by = .(to, date, type)]

transport_from_all_monthly <- transport_between_padd_clean_monthly[, .(from, from_all, date, type, year)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, from + type ~ date, value.var = "from_all")]
writexl::write_xlsx(transport_from_all_monthly, "transport_from_all_monthly.xlsx")

transport_to_all_monthly <- transport_between_padd_clean_monthly[, .(to, to_all, date, type, year)
    ][, unique(.SD)
    ][year >= 2011, dcast(.SD, to + type ~ date, value.var = "to_all")]
writexl::write_xlsx(transport_to_all_monthly, "transport_to_all_monthly.xlsx")

# 将相关调油料、RBOB和CBOB取出
blending_components <- transport_between_padd_monthly[str_detect(name, "Blending Components"), .SD
    ][, type := fcase(
        str_detect(name, "Reformulated Gasoline Blending Components"), "RBOB",
        str_detect(name, "Conventional Gasoline Blending Components"), "CBOB",
        str_detect(name, "of Gasoline Blending Components"), "Blending Components"
    )
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][!is.na(type), .SD
    ][, from_all := sum(value, na.rm = T), by = .(from, date, type) 
    ][, to_all := sum(value, na.rm = T), by = .(to, date, type)
    ]

transport_from_components_monthly <- blending_components[, .(from, from_all, date, type, year)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, from + type ~ date, value.var = "from_all")]

transport_to_components_monthly <- blending_components[, .(to, to_all, date, type, year)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, to + type ~ date, value.var = "to_all")]

writexl::write_xlsx(transport_from_components_monthly, "transport_from_components_monthly.xlsx")
writexl::write_xlsx(transport_to_components_monthly, "transport_to_components_monthly.xlsx")
# name <- blending_components[, .(name = unique(name))]

# 2. 通过管道运输调油料量
ld(transport_between_padd_pipeline_monthly)
blending_components_pipeline <- transport_between_padd_pipeline_monthly[, area := str_replace_all(area, "&nbsp;", "")
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][str_detect(name, "Blending Components"), .SD
    ][, type := fcase(
        str_detect(name, "Reformulated Gasoline Blending Components"), "RBOB",
        str_detect(name, "Conventional Gasoline Blending Components"), "CBOB",
        str_detect(name, "of Gasoline Blending Components"), "Blending Components"
    )
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][!is.na(type), .SD
    ][, from_all := sum(value, na.rm = T), by = .(from, date, type) 
    ][, to_all := sum(value, na.rm = T), by = .(to, date, type)
    ][, tag := "pipeline"]

# 3.通过油罐车和平底船运输调油料
ld(transport_between_padd_tank_barge_monthly)
blending_components_tb <- transport_between_padd_tank_barge_monthly[, area := str_replace_all(area, "&nbsp;", "")
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][str_detect(name, "Blending Components"), .SD
    ][, type := fcase(
        str_detect(name, "Reformulated Gasoline Blending Components"), "RBOB",
        str_detect(name, "Conventional Gasoline Blending Components"), "CBOB",
        str_detect(name, "of Gasoline Blending Components"), "Blending Components"
    )
    ][, from := str_sub(area, start = 1L, end = 6L)
    ][, to := str_sub(area, start = -6L, end = -1L)
    ][!is.na(type), .SD
    ][, from_all := sum(value, na.rm = T), by = .(from, date, type) 
    ][, to_all := sum(value, na.rm = T), by = .(to, date, type)
    ][, tag := "tank_barge"]

blending_components_ways <- list(blending_components_pipeline, blending_components_tb) %>% rbindlist()

transport_from_components_ways_monthly <- blending_components_ways[, .(from, from_all, date, type, year, tag)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, from + type + tag ~ date, value.var = "from_all")]


transport_to_components_ways_monthly <- blending_components_ways[, .(to, to_all, date, type, year, tag)
    ][year >= 2011, unique(.SD)
    ][, dcast(.SD, to + type + tag ~ date, value.var = "to_all")]

writexl::write_xlsx(transport_from_components_ways_monthly, "transport_from_components_ways_monthly.xlsx")
writexl::write_xlsx(transport_to_components_ways_monthly, "transport_to_components_ways_monthly.xlsx")

# Colonial-RBOB & buckeye-RBOB
price_diff <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/colonial_RBOB.xlsx", sheet = "Sheet1") %>% as.data.table() %>% setnames(1:3, c("date", "colo_rbob", "buck_rbob"))
price_diff[, date := as.Date(date)
    ][, year := year(date) %>% as.factor()
    ][, day := 1:.N, by = .(year)]

ggplot(price_diff[, .(date, year, day, colo_rbob)]) +
  geom_line(size = 1, aes(x = day, y = colo_rbob, colour = year)) +
#   geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "美元/加仑", title = "colonial-RBOB价差") +
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

  ggsave("colonial-RBOB.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)

ggplot(price_diff[, .(date, year, day, buck_rbob)]) +
  geom_line(size = 1, aes(x = day, y = buck_rbob, colour = year)) +
#   geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
  theme_grey() +
  labs(x = "时间", y = "美元/加仑", title = "buckeye-RBOB价差") +
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

  ggsave("buck-RBOB.tiff", device = "tiff", dpi = 1000, width = 6, height = 4)