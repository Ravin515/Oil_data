library(styleer)
ld(gasoline_sales)
gasoline_sales[, unique(name)]

# 全美汽油销售量季节图
ggplot(gasoline_sales[str_detect(name, "Total Gasoline") & area == "U.S." & (!is.na(value)) & year > 2011
                      ][, year := factor(year)
                      ][, .(value = sum(value)/42, month = as.integer(month), year), by = .(date)]) +
  # geom_smooth(size = 2, aes(x = month, y = value, colour = year), span = 0.1, se = F) +
  geom_line(size = 2, aes(x = month, y = value, colour = year)) +
  theme_grey() +
  labs(x = "时间", y = "千桶/天", title = "美国汽油销量") +
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

# 全美各地汽油销售趋势图
ggplot(gasoline_sales[str_detect(area, "East Coast|Midwest|Rocky Mountain|Gulf Coast|West Coast") & year > 2011 & !is.na(value) & str_detect(name, "Total Gasoline")][, year := factor(year)
][, .(value = value/42, month = as.integer(month), year), by = .(date, area)
  ][, area := str_replace(area, "\\(.+\\)", "")] # 有许多缺失值，不能删除
, aes(x = date, y = value, colour = area, fill = area, order = area)) +
  geom_line(size = 2) +
  theme_grey() +
  labs(x = "时间", y = "千桶/天", title = "美国各地区汽油销量") +
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

