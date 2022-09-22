library(styleer)
library(eia)

# 1. 获取流量或者存量（eia_series函数的argument选项n = NULL或1）
petroleum_categories <- eia_cats(714755)$childcategories %>% as.data.table() 
petroleum_categories <- petroleum_categories[, eia_cats(category_id)$childcategories %>% list() %>% rbindlist(), by = .(parent_name = name)
    ][str_detect(name, "Week")]


# 1.1 一层嵌套
petroleum_categories_1 <- petroleum_categories[, eia_cats(category_id)$childseries %>% list() %>% rbindlist(), by = .(grandp_name = parent_name, parent_name = name)
    ][, eia_series(series_id)[["data"]] %>% rbindlist(), by = .(grandp_name, parent_name, name, f, units, updated)
    ] 

petroleum_categories_1[, area := fcase(
    str_detect(name, "U.S."), "U.S.",
    str_detect(name, "PADD 1"), "PADD 1",
    str_detect(name, "PADD 2"), "PADD 2",
    str_detect(name, "PADD 3"), "PADD 3",
    str_detect(name, "PADD 4"), "PADD 4",
    str_detect(name, "PADD 5"), "PADD 5"
)
][, type := fifelse(area == "U.S.", str_extract(name, "\\.(.+)\\,"), str_extract(name, "\\)(.+)\\,"))
][, type := str_replace(type, "\\.S\\.|\\)", "")
][, type := str_replace(type, ",", "") %>% str_trim()]

# 1.2 两层嵌套
# summary与其他分开
petroleum_categories_2 <- petroleum_categories[, eia_cats(category_id)$childcategories %>% list() %>% rbindlist(), by = .(grandp_name = parent_name, parent_name = name)
    ][name == "by Data Series", eia_cats(category_id)$childcategories %>% list() %>% rbindlist(), by = .(grandp_name, parent_name)
    ][, eia_cats(category_id)$childseries %>% list() %>% rbindlist(), by = .(grandp_name, parent_name, type = name)
    ][f== "4"|f=="W", eia_series(series_id)[["data"]] %>% rbindlist(fill = T), by = .(grandp_name, parent_name, name, f, units, updated)]

petroleum_categories_2[, area := fcase(
    str_detect(name, "U.S."), "U.S.",
    str_detect(name, "PADD 1"), "PADD 1",
    str_detect(name, "PADD 2"), "PADD 2",
    str_detect(name, "PADD 3"), "PADD 3",
    str_detect(name, "PADD 4"), "PADD 4",
    str_detect(name, "PADD 5"), "PADD 5"
)
][, type := fifelse(area == "U.S.", str_extract(name, "\\.(.+)\\,"), str_extract(name, "\\)(.+)\\,"))
][, type := str_replace(type, "\\.S\\.|\\)", "")
][, type := str_replace(type, ",", "") %>% str_trim()
]

# 2. 整体更新
petroleum_updated_weekly <- list(petroleum_categories_1, petroleum_categories_2) %>% rbindlist()

## 2. 与存量数据整合在一起
## 有一些type和area存在NA的情况，没有进行处理
# ld(petroleum_updated_weekly)

## 流量
# petroleum_updated_flow_weekly <- list(petroleum_categories_1, petroleum_categories_2) %>% rbindlist()
# petroleum_updated_weekly <- list(petroleum_updated_weekly, petroleum_updated_flow_weekly) %>% rbindlist() %>% unique()

sv(petroleum_updated_weekly, svname = petroleum_updated_weekly)

# name_list <- petroleum_updated_weekly[year >= 2017 & f == "W", .(name_list = unique(name))]

# writexl::write_xlsx(name_list, "name_list.xlsx")

# 3. 画图
# ld(petroleum_updated_weekly)
# eia_pic_weekly <- petroleum_updated_weekly[!is.na(area) & str_detect(name, "Crude|Finished Motor Gasoline|Jet|Distillate|Residual") & grandp_name == "Summary" & f == "W" & str_detect(name, "U.S.") & !str_detect(name, "ppm"), .SD
#     # ][!is.na(area) & str_detect(name, "Crude|Finished Motor Gasoline|Jet|Distillate|Residual") & grandp_name == "Summary" & f == "W" & !str_detect(name, "1A|1B|1C") & !str_detect(name, "ppm"), .SD
#     # ][year > 2011 & str_detect(name, "Finished Motor Gasoline") & str_detect(name, "PADD 1"), .SD
#     ][, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week, name)
#   # ][, week := format(date, format = "%b-%d")
#     ][year >= max(year) - 4, .SD
#     ][, year := as.factor(year)
#     ][, .(pic = (ggplot(.SD) +
#           geom_line(size = 1, aes(x = week, y = value, colour = year)) +
#           geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
#           theme_grey() +
#           labs(x = "周数", y = unique(units), title = unique(name)) +
#           # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
#           theme(
#             plot.title =  element_text(size = rel(1.3), hjust = 0.5),
#             axis.line = element_line(linetype = 1),
#             legend.title = element_blank(),
#             #panel.border = element_rect(linetype = 1, fill = NA),
#             legend.position = "bottom",
#             legend.spacing.x = unit(0.1, 'cm'),
#             legend.spacing.y = unit(2, 'cm'),
#             # legend.box = "horizontal",
#             # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
#             legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
#             legend.key.size = unit(0.5, 'cm')
#           )) %>% list()), by = .(name)
#     ]

# # ggsave(plot = eia_pic_weekly[["pic"]][[1]], "1.jpg", device = "jpg", dpi = 300, width = 10, height = 5)
# # ggsave(plot = eia_pic_weekly[["pic"]][[139]], "1.tiff", device = "tiff", dpi = 300, width = 10, height = 5)

# eia_pic_weekly[, {
#   for (i in 1:.N) {
#     ggsave(plot = pic[[i]], str_c("./pic/", as.character(name[[i]]), ".jpg"), device = "jpg", dpi = 300, width = 10, height = 5)
#   }
# }
# ]

