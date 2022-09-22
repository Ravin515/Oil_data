library(styleer)
library(gt)
library(webshot2)
library(dplyr)
library(patchwork)
library(gtsummary)
library(bstfun)

# 1.周度变化表
eia_updated <- fread("https://ir.eia.gov/wpsr/table9.csv")
d <- colnames(eia_updated)[3] %>% as.Date("%m/%d/%y")
eia_updated_tab_weekly <- eia_updated[!str_detect(STUB_2, "PADD")
    ][str_detect(STUB_2, "Domestic Production|Percent Utilization|Finished Motor Gasoline|Distillate Fuel Oil|Kerosene-Type Jet Fuel|Residual Fuel Oil|Commercial|Cushing|SPR|Total Stocks|Crude Oil|Operable Capacity|Total Motor Gasoline"), .SD
    ][
        (str_detect(STUB_1, "Crude")) |
        (str_detect(STUB_1, "Refiner Inputs")) |
        (str_detect(STUB_1, "Refiner and Blender") & !str_detect(STUB_2, "Adjustment|Commercial")) |
        (str_detect(STUB_1, "Stocks") & !str_detect(STUB_2, "Crude")) |
        (str_detect(STUB_1, "Imports") & str_detect(STUB_2, "Total Motor Gasoline|Distillate Fuel Oil|Kerosene-Type Jet Fuel|Residual Fuel Oil|Crude")) |
        (str_detect(STUB_1, "Exports")) |
        (str_detect(STUB_1, "Supplied"))
    
    ][!str_detect(STUB_1, "Net Imports") & !str_detect(STUB_2, "Crude Oil Inputs")
    ][!(str_detect(STUB_1, "Stock") & str_detect(STUB_2, "Finished Motor Gasoline"))
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

tab <- eia_updated_tab_weekly %>% 
  group_by(sub_title) %>%
  gt() %>% 
  tab_options(
    column_labels.font.size = 18,
    column_labels.font.weight = "bold") %>% 
    data_color(columns = `变化`,
    colors = pal) %>% 
    tab_footnote("数据来源：EIA，浙商期货研究中心油品组") %>%
    as_ggplot()

# ggsave(plot = tab, "tab.jpg", device = "jpg")
    # gtsave("./pic/tab.png", expand = 30)         

# 2. 30张季节性图
# U.S. Ending Stocks excluding SPR of Crude Oil, Weekly 为商业原油库存
ld(petroleum_updated_weekly, force = T)
index <- petroleum_updated_weekly[grandp_name == "Summary"& f == "W"
   ][!str_detect(name, "PADD")
   ][str_detect(name, "Percent Utilization|Finished Motor Gasoline|Distillate|Kerosene-Type Jet Fuel|Residual Fuel Oil|Commercial|Cushing|SPR|Total Stocks|Crude Oil|Operable Capacity|Total Crude Oil and Petroleum Products|Total Gasoline"), .SD
   ][!str_detect(name, "Net Imports")
   ][!str_detect(name, "Refiner Net Input")
   ][!str_detect(name, "Sulfur|Military|48|Alaska|Adjustment")
   ][!str_detect(name, "Blender Net Production of Finished")
   ][!str_detect(name, "Exports of Crude Oil and Petroleum|Commercial Kerosene-Type")
   ][!str_detect(name, "Imports by SPR|Imports for SPR|Imports Excluding SPR")
   ][!str_detect(name, "Days")
   ][!str_detect(name, "Imports of Crude Oil and Petroleum Products")
   ][!str_detect(name, "Ending Stocks of Crude Oil, Weekly")
   ][!str_detect(name, "Stocks of Finished")
   ][!str_detect(name, "Imports of Finished")
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
# 将5 Year Mean当作Observation插入
eia_updated_pic_weekly[,tag := week[year == max(year)] %>% max(),
    by = .(name) # 有些week在最新年份还没到，需要加一个tag
  ][, ':='(
    y_max = fifelse(week <= tag, max(value[year <= (max(year) - 1) & year >= (max(year) - 5)], na.rm = T), max(value[year <= (max(year)) & year >= (max(year) - 4)], na.rm = T)), 
    y_min = fifelse(week <= tag, min(value[year <= (max(year) - 1) & year >= (max(year) - 5)], na.rm = T), min(value[year <= (max(year)) & year >= (max(year) - 4)], na.rm = T))
    ), 
    by = .(name, week) # 针对不同情况ifelse操作
  ][, year_tag := year]

eia_updated_pic_weekly <- list(eia_updated_pic_weekly[,
    .(value = fifelse(week <= tag, mean(value[year <= (max(year) - 1) & year >= (max(year) - 5)], na.rm = T), mean(value[year <= (max(year)) & year >= (max(year) - 4)], na.rm = T)), year_tag = "5 Year Mean"), by = .(name, week) 
  ][, unique(.SD)], eia_updated_pic_weekly) %>% rbindlist(use.names = T, fill = T)

eia_pic_weekly <- eia_updated_pic_weekly[, sub_title := fcase(
      str_detect(name, "汽油"), "汽油",
      str_detect(name, "柴油"), "柴油",
      str_detect(name, "航煤"), "航煤",
      str_detect(name, "燃料油"), "燃料油",
      default = "原油"
    ) 
  # ][, max_y := value[year == max(year)], by = .(name, week)
  ][year >= max(year, na.rm = T) - 3 | is.na(year), .SD
  ][, year := as.factor(year)
  ][, .(pic = (ggplot(.SD) +
          geom_line(aes(x = week, y = value, colour = year_tag, size = year_tag, linetype = year_tag)) +
          geom_ribbon(aes(x = week, ymin = y_min, ymax = y_max, fill = "5 Year Range"), alpha = 0.3, na.rm = T) + 
          # geom_line(linetype = 2, aes(x = week, y = y_mean, size = NA, colour = "5 Year Mean")) +
          labs(x = "周数", y = NULL, title = unique(name)) +
          scale_colour_manual(values = c("#1E90FF", "#191970", "#008000", "#DC143C", "#FFA500")) +
          scale_size_manual(values = c(1, 1, 1, 2, 1)) +
          scale_linetype_manual(values = c(1, 1, 1, 1, 2)) +
          # scale_fill_manual(values = "#69cfd5") +
          # scale_x_date(date_breaks = "1 month", date_labels = "%b") +
          theme(
            plot.title =  element_text(face = "bold", size = rel(2), hjust = 0.5),
            axis.line = element_line(linetype = 1),
            legend.title = element_blank(),
            # panel.border = element_rect(linetype = 1, fill = NA),
            legend.position = "bottom",
            legend.spacing.x = unit(0.5, 'cm'),
            legend.spacing.y = unit(0.5, 'cm'),
            legend.text = element_text(size = rel(1)),
            # legend.box = "horizontal",
            # legend.box.background = element_rect(size = 1, colour = "black", fill = "white"),
            legend.key = element_rect(size = 0.5, colour = "black", fill = "white"),
            legend.key.size = unit(1, 'cm'),
            # panel.spacing = margin(t = 100, r = 100, b = 100, l = 100, unit = "pt")
          )) %>% list()), keyby = .(sub_title, name)
  ]

# 拼图
pic <- eia_pic_weekly[, wrap_plots(pic, ncol = 5, nrow = 6)]
#ggsave("./pic/pic.png", device = "png", dpi = 500, width = 30, height = 30)

# 3. 把表和图都拼起来
eia <- tab + pic
ggsave("./pic/eia.jpg", plot = eia, device = "jpg", dpi = 200, width = 90, height = 40, limitsize = F)
