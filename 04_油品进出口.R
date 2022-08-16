library(styleer)
library(ggalt)
ld(export_import_weekly)
# Finished Motor Gasoline
gasoline_export_import <- export_import_weekly[str_detect(type, "Finished Motor Gasoline"), .SD
  ][, category := str_extract(name, "\\of(.+)\\,")
  ][, category := str_replace_all(category, "of", "")
  ][, category := str_replace_all(category, ",", " ")
  ][, category := str_trim(category)
  ][, area := str_extract(name, ".+\\(PADD.+\\)")
  ]
# 进口与出口
gasoline_import <- gasoline_export_import[is.na(area)&str_detect(name, "Import"), .SD]
gasoline_export <- gasoline_export_import[is.na(area)&str_detect(name, "Export"), .SD]

# 总体汽油进口
ggplot(gasoline_import[(!is.na(value))& f == "W" & year > 2011][, year := factor(year)]) +
  # geom_xspline(size=0.8) +
  geom_smooth(size = 2, aes(x = week, y = value, colour = year), span = 0.1, se = F) +
  labs(title = "美国汽油进口量（周度）")+
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间（周）", y = "千桶/天") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
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

# 总体汽油出口
ggplot(gasoline_export[(!is.na(value))& f == "W" & year > 2011][, year := factor(year)], aes(x = week, y = value, colour = year, fill = year, order = year)) +
  # geom_line(size = 0.8) +
  geom_smooth(size = 2, aes(x = week, y = value, colour = year), span = 0.1, se = F) +
  labs(title = "美国汽油出口量（周度）") +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间（周）", y = "千桶/天") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
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

# 各地区汽油进口变化
gasoline_import_district <- gasoline_export_import[!is.na(area)&str_detect(name, "Import"), .SD]
ggplot(gasoline_import_district[(!is.na(value))& f == "4" & year > 2011
                                ][, year := factor(year)
                                ][, area := str_replace(area, "\\(.+\\)", "")], 
       aes(x = date, y = value, colour = area, fill = area, order = area)) +
  geom_line(stat = "identity",size = 2) +
  theme_grey() +
  labs(x = "时间（周）", y = "千桶/天", title = "各大区汽油进口量（周度）") +
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

# 各大地区主要进口地
ld(district_import_gasoline)
district_import_gasoline[, country := str_extract(name, "from (.+) of")
                         ][, country := str_replace_all(country, "from|of", "")]

# PADD1
ggplot(district_import_gasoline[(!is.na(country)) & str_detect(area, "East") & (!is.na(value))& f == "M" & year > 2015
][, year := factor(year)
][, area := str_replace(area, "\\(.+\\)", "")
][, import_all := sum(value), by = .(country)
][import_all %in% sort(unique(import_all))[1:10], .SD], 
aes(x = date, y = value, colour = country, fill = country, order = country)) +
  geom_line(size = 0.8) +
  theme_grey() +
  labs(x = "时间（周）", y = "千桶/天", title = "PADD1进口国进口量（周度）") +
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