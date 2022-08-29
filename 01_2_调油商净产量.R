library(styleer)
ld(blender_net_production)
# 提取汽油数据
blender_gasoline_production <- blender_net_production[str_detect(name, "Gasoline"), .SD]
blender_gasoline_production[, category := str_extract(name, "\\of(.+)\\,")
  ][, category := str_replace_all(category, "of", "")
  ][, category := str_replace_all(category, ",", " ")
  ][, category := str_trim(category)
  ][, area := str_extract(name, ".+\\(PADD.+\\)")
  ]

# 调油商Finished gasoline季节图
ggplot(blender_gasoline_production[is.na(area)& f == "W"&(!is.na(value)) & category == "Finished Motor Gasoline"&year > 2011 ][, year := factor(year)], aes(x = week, y = value, colour = year, fill = year, order = year)) +
  geom_smooth(size = 2, aes(x = week, y = value, colour = year), span = 0.1, se = F) +
  # geom_line(size = 0.8) +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间（周）", y = "千桶/天", title = "美国贸易商汽油产量（周度）") +
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

# 调油商和炼厂的总和
b_r_gasoline_production <- rbindlist(list(blender_gasoline_production[is.na(area)& f == "W"&(!is.na(value)) & category == "Finished Motor Gasoline"&year > 2011 ][, year := factor(year)],
refiner_gasoline_production[is.na(area)& f == "W"&(!is.na(value)) & category == "Finished Motor Gasoline"&year > 2011 ][, year := factor(year)]))

ggplot(b_r_gasoline_production[, .(value = sum(value), year, week), by = .(date)], aes(x = week, y = value, colour = year, fill = year, order = year)) +
  geom_smooth(size = 2, aes(x = week, y = value, colour = year), span = 0.1, se = F) +
  # geom_line(size = 0.8) +
  labs(title = "美国汽油总产量（周度）") +
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