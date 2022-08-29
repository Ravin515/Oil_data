library(styleer)
ld(refiner_net_production, force = T)
# 提取汽油数据
refiner_gasoline_production <- refiner_net_production[str_detect(name, "Gasoline"), .SD
                                                    ][order(name, date), .SD]
refiner_gasoline_production[, category := str_extract(name, "\\of(.+)\\,")
                          ][, category := str_replace_all(category, "of", "")
                          ][, category := str_replace_all(category, ",", " ")
                          ][, category := str_trim(category)
                          ][, area := str_extract(name, ".+\\(PADD.+\\)")
                          ]

# 1. 所有炼厂产品的产量变化图 (Conventional and Formulated)----
ggplot(refiner_gasoline_production[is.na(area)&f == "W"&(!is.na(value)) & category %in% c("Conventional Motor Gasoline", "Reformulated Motor Gasoline")], aes(x = date, y = value, group = factor(category), colour = category, fill = category, order = category)) +
  geom_bar(stat = "identity") +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "净产量（千桶/天）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "vertical",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.2, 'cm')
  )

# 2.所有炼厂产品的产量变化图（Conventional and Formulated 四周平均）----
ggplot(refiner_gasoline_production[is.na(area) & f == "4"& (!is.na(value)) & category %in% c("Conventional Motor Gasoline", "Reformulated Motor Gasoline")], aes(x = date, y = value, group = factor(category), colour = category, fill = category, order = category)) +
  geom_bar(stat = "identity") +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "净产量（千桶/天）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "vertical",
    legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.2, 'cm')
  )

# 3. 各地区炼厂汽油总产量变化图（五大地区）----
ggplot(refiner_gasoline_production[!is.na(area) & f == "W"& (!is.na(value)) & year>2010, .SD][, area := str_replace_all(area, "\\(.+\\)", "")], aes(x = date, y = value, group = factor(area), colour = area, fill = area, order = area)) +
  geom_bar(stat = "identity") +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "净产量（千桶/天）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "vertical",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.2, 'cm')
  )

# 4. 各地区炼厂汽油总产量变化图（五大地区 4周平均）----
ggplot(refiner_gasoline_production[!is.na(area) & f == "4"& (!is.na(value)) & year>2010, .SD][, area := str_replace_all(area, "\\(.+\\)", "")], aes(x = date, y = value, group = factor(area), colour = area, fill = area, order = area)) +
  geom_bar(stat = "identity") +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "净产量（千桶/天）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
    axis.line = element_line(linetype = 1),
    legend.title = element_blank(),
    #panel.border = element_rect(linetype = 1, fill = NA),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, 'cm'),
    legend.spacing.y = unit(2, 'cm'),
    legend.box = "vertical",
    # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
    legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
    legend.key.size = unit(0.2, 'cm')
  )

# 5. 汽油产量总量季节性图
ggplot(refiner_gasoline_production[is.na(area)& f == "W"&(!is.na(value)) & category == "Finished Motor Gasoline"&year > 2011 ][, year := factor(year)], aes(x = week, y = value, colour = year, fill = year, order = year)) +
  geom_smooth(size = 2, aes(x = week, y = value, colour = year), span = 0.1, se = F) +
  # geom_line(size = 0.8) +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "千桶/天", title = "美国炼厂汽油产量（周度）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
    plot.title = element_text(size = rel(1.3), hjust = 0.5),
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

# 6. 汽油产量总量季节性图（四周平均）
ggplot(refiner_gasoline_production[is.na(area)& f == "W"&(!is.na(value)) & category == "Finished Motor Gasoline"&year > 2016 ][, year := factor(year)], aes(x = week, y = value, colour = year, fill = year, order = year)) +
  geom_line(size = 1) +
  theme_grey() +
  # scale_colour_manual(values = c("#9999CC", "#CC6666", "#66CC99")) +
  labs(x = "时间", y = "净产量（千桶/天）") +
  # scale_fill_manual(values = c("#9999CC", "#CC6666", "#66CC99"),
  #                   name = "variable",
  #                   breaks = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）"),
  #                   labels = c("沪深300", "风险-收益率策略", "风险-收益率策略（考虑交易成本）")) +
  theme(
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