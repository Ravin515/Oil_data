library(styleer)
library(rmarkdown)
library(gt)
library(webshot2)
library(dplyr)

# 1.周度变化表
eia_updated <- fread("https://ir.eia.gov/wpsr/table9.csv")
d <- colnames(eia_updated)[3] %>% as.Date("%m/%d/%y")
eia_updated_tab_weekly <- eia_updated[!str_detect(STUB_2, "PADD")
    ][str_detect(STUB_2, "Domestic Production|Percent Utilization|Finished Motor Gasoline|Distillate Fuel Oil|Kerosene-Type Jet Fuel|Residual Fuel Oil|Commercial|Cushing|SPR|Total Stocks|Crude Oil|Operable Capacity"), .SD
    ][
        (str_detect(STUB_1, "Crude")) |
        (str_detect(STUB_1, "Refiner Inputs")) |
        (str_detect(STUB_1, "Refiner and Blender") & !str_detect(STUB_2, "Adjustment|Commercial")) |
        (str_detect(STUB_1, "Stocks") & !str_detect(STUB_2, "Crude")) |
        (str_detect(STUB_1, "Imports") & str_detect(STUB_2, "Finished Motor Gasoline|Distillate Fuel Oil|Kerosene-Type Jet Fuel|Residual Fuel Oil|Crude")) |
        (str_detect(STUB_1, "Exports")) |
        (str_detect(STUB_1, "Supplied"))
    
    ][!str_detect(STUB_1, "Net Imports") & !str_detect(STUB_2, "Crude Oil Inputs")
    ][, name := c(
            "美国原油产量（千桶/天）", 
            "美国炼厂运营炼能（千桶/天）", 
            "美国炼厂产能利用率（%）", 
            "美国汽油产量（千桶/天）", 
            "美国航煤产量（千桶/天）", 
            "美国柴油产量（千桶/天）", 
            "美国燃料油产量（千桶/天）", 
            "美国商业原油库存（百万桶）", 
            "美国库欣原油库存（百万桶）", 
            "美国战略原油储备（百万桶）", 
            "美国汽油库存（百万桶）", 
            "美国航煤库存（百万桶）", 
            "美国柴油库存（百万桶）", 
            "美国燃料油库存（百万桶）", 
            "美国原油成品油总库存去除战略储备（百万桶）", 
            "美国原油成品油总库存包含战略储备（百万桶）", 
            "美国原油进口（千桶/天）", 
            "美国汽油进口（千桶/天）", 
            "美国航煤进口（千桶/天）", 
            "美国柴油进口（千桶/天）", 
            "美国燃料油进口（千桶/天）", 
            "美国原油出口（千桶/天）", 
            "美国汽油出口（千桶/天）", 
            "美国航煤出口（千桶/天）", 
            "美国柴油出口（千桶/天）",
            "美国燃料油出口（千桶/天）",
            "美国汽油需求（千桶/天）",
            "美国航煤需求（千桶/天）",
            "美国柴油需求（千桶/天）",
            "美国燃料油需求（千桶/天）"
            )
    ][, sub_title := fcase(
      str_detect(name, "汽油"), "汽油",
      str_detect(name, "柴油"), "柴油",
      str_detect(name, "航煤"), "航煤",
      str_detect(name, "燃料油"), "燃料油",
      default = "原油"
    )  
    ][, .SD, .SDcols = c(10, 9, 3, 4)
    ][order(sub_title, name), .SD
    ][, setnames(.SD, 2:4, c(str_c("EIA周度数据", " ", d), "本期", "上期"))
    ][, ':='(`本期` = str_replace_all(`本期`, ",", "") %>% as.numeric(),
            `上期` = str_replace_all(`上期`, ",", "") %>%  as.numeric())
    ][, `变化` := (`本期` - `上期`) %>% round(digits = 2)]


# 调整表格颜色
pal <- function(x) {
  f_neg <- scales::col_numeric(
    palette = c("#69cfd5"),
    domain = c(-10e6, 0)
  )
  f_pos <- scales::col_numeric(
    palette = c("#feb8cd"),
    domain = c(0, 10e6)
  )
  f_zero <- scales::col_numeric(
    palette = c("#ffffff"),
    domain = c(0)
  )
  fcase(x < 0, f_neg(x),
        x > 0, f_pos(x),
        x == 0, f_zero(x))
}

eia_updated_tab_weekly %>% 
  group_by(sub_title) %>%
  gt() %>% 
  tab_options(
    column_labels.font.size = 18,
    column_labels.font.weight = "bold",
    
  ) %>%
  data_color(columns = `变化`,
    colors = pal
    ) %>% gtsave("tab_1.png", expand = 30)         

# 2. 25张季节性图
# U.S. Ending Stocks excluding SPR of Crude Oil, Weekly 为商业原油库存
ld(petroleum_updated_weekly, force = T)
index <- petroleum_updated_weekly[grandp_name == "Summary"& f == "W"
   ][!str_detect(name, "PADD")
   ][str_detect(name, "Percent Utilization|Finished Motor Gasoline|Distillate|Kerosene-Type Jet Fuel|Residual Fuel Oil|Commercial|Cushing|SPR|Total Stocks|Crude Oil|Operable Capacity|Total Crude Oil and Petroleum Products"), .SD
   ][!str_detect(name, "Net Imports")
   ][!str_detect(name, "Refiner Net Input")
   ][!str_detect(name, "Sulfur|Military|48|Alaska|Adjusted|Adjustment")
   ][!str_detect(name, "Exports of Crude Oil and Petroleum|Commercial Kerosene-Type")
   ][!str_detect(name, "Imports by SPR|Imports for SPR|Imports Excluding SPR")
   ][!str_detect(name, "Days")
   ][!str_detect(name, "Imports of Crude Oil and Petroleum Products")
   ][!str_detect(name, "Ending Stocks of Crude Oil, Weekly")
   ][, .(name = unique(name))
   ][, cn_name := c(
            "美国原油产量（千桶/天）", 
            "美国炼厂运营炼能（千桶/天）", 
            "美国炼厂产能利用率（%）", 
            "美国汽油产量（千桶/天）", 
            "美国航煤产量（千桶/天）", 
            "美国柴油产量（千桶/天）", 
            "美国燃料油产量（千桶/天）", 
            "美国原油成品油总库存包含战略储备（百万桶）", 
            "美国原油成品油总库存去除战略储备（百万桶）", 
            "美国商业原油库存（百万桶）", 
            "美国库欣原油库存（百万桶）", 
            "美国战略原油储备（百万桶）", 
            "美国汽油库存（百万桶）", 
            "美国航煤库存（百万桶）", 
            "美国柴油库存（百万桶）", 
            "美国燃料油库存（百万桶）", 
            "美国原油进口（千桶/天）", 
            "美国汽油进口（千桶/天）", 
            "美国航煤进口（千桶/天）", 
            "美国柴油进口（千桶/天）", 
            "美国燃料油进口（千桶/天）", 
            "美国原油出口（千桶/天）", 
            "美国汽油出口（千桶/天）", 
            "美国航煤出口（千桶/天）", 
            "美国柴油出口（千桶/天）",
            "美国燃料油出口（千桶/天）",
            "美国汽油需求（千桶/天）",
            "美国航煤需求（千桶/天）",
            "美国柴油需求（千桶/天）",
            "美国燃料油需求（千桶/天）"
            )]

eia_updated_old_weekly <- index[petroleum_updated_weekly[grandp_name == "Summary"& f == "W", .SD], on = .(name), nomatch = 0
  ][, .(name = cn_name, date, year, month, value)
  ][, value := fifelse(str_detect(name, "百万桶"), value/1000, value)] # 库存中新数据与老数据的量纲不一致

# 新数据更新到老数据中
eia_updated_new_weekly <- eia_updated_tab_weekly[, setnames(.SD, 2:3, c("name", as.character(d)))
  ][, .SD, .SDcols = 2:3
  ][, melt(.SD, id = 1, measure = 2, value.factor = F)
  ][, setnames(.SD, 2, "date")
  ][, date := as.character(date) %>% as.Date()
  ][, ':='(year = year(date), month = month(date))]

eia_updated_pic_weekly <- rbindlist(list(eia_updated_new_weekly, eia_updated_old_weekly), use.names = T) %>% unique()
eia_updated_pic_weekly <- eia_updated_pic_weekly[, .SD[1], keyby = .(name, date)
  ][, week := 1:.N, by = .(name, year)]

# 批量画图
eia_pic_weekly <- eia_updated_pic_weekly[, ':='(y_max = max(value[year <= max(year-1) & year >= max(year - 5)]), y_min = min(value[year <= max(year-1) & year >= max(year - 5)])), by = .(week, name)
  # ][, week := format(date, format = "%b-%d")
    ][year >= max(year) - 4, .SD
    ][, year := as.factor(year)
    ][, .(pic = (ggplot(.SD) +
          geom_line(size = 1, aes(x = week, y = value, colour = year)) +
          geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3) + 
          theme_grey() +
          labs(x = "周数", title = unique(name)) +
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
          )) %>% list()), by = .(name)
    ]

eia_pic_weekly[, {
  for (i in 1:.N) {
    ggsave(plot = pic[[i]], str_c("./pic/", i, ".jpg"), device = "jpg", dpi = 300, width = 10, height = 5)
  }
}
]

