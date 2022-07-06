library(styleer)
library(eia)
eia_set_key("BBUpoBxsPxnPe9A3dlLW44S6aLDfNuN7dvK0HgBW")

eia_cats(714755) #汽油数据


# 0.总览---
eia_cats(714756)
oil_summary <- eia_cats(714756)$childcategories %>% as.data.table()
eia_cats(229975) # 供给与处置
eia_cats(235079) # 周度供给统计
eia_cats(236392) # 美国原油供给和处置
eia_cats(236418) # 各州价格、售卖量和储备


# 1. 汽油价格----
price <- eia_cats(714757)$childcategories %>% as.data.table()
# 1.1 汽油零售价格
eia_cats(714757)
oil_price <- eia_cats(714757)$childcategories %>% as.data.table()
oil_price
eia_cats(240690) # 零售汽柴油价格

## 1.1.1 以产品分类导出 
eia_cats(241020, cache = F) # 以所有产品进行分类
oil_price_retail_by_product <- eia_cats(241020, cache = F)$childcategories$category_id
oil_price_retail_by_product <- data.table(category_id = oil_price_retail_by_product)
oil_price_retail_by_product <- oil_price_retail_by_product[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id)
    ][, tag := fifelse(str_detect(name, "Weekly"), 1, 0)] #标记出weekly的数据
### 导出所有周度数据
oil_price_retail_by_product_weekly <- oil_price_retail_by_product[tag == 1, .SD
    ][, rbindlist(list(eia_series(series_id)[["data"]] %>% as.data.table())), by = .(category_id, name, f, units, updated)]

## 1.1.2 以地区导出 
eia_cats(240691, cache = F) # 以所有区域进行分类
oil_price_retail_by_area <- eia_cats(240691, cache = F)$childcategories$category_id
oil_price_retail_by_area <- data.table(category_id = oil_price_retail_by_area)
oil_price_retail_by_area <- oil_price_retail_by_area[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id)
    ][, tag := fifelse(str_detect(name, "Weekly"), 1, 0)] #标记出weekly的数据

## 无论以地区或者产品导出，结果都是一样的

## 1.2 美国各地炼厂汽油产出价格
eia_cats(241528) # 美国炼厂价格 Refiner Gasoline Prices by Sale Type
refiner_gasoline_price <- eia_cats(241529)$childcategories %>% as.data.table() 
refiner_gasoline_price <- refiner_gasoline_price[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(area = name)]
refiner_gasoline_price_monthly <- refiner_gasoline_price[f == "M", rbindlist(eia_series(series_id)[['data']]), by = .(area, name, f, units, updated)]
sv(refiner_gasoline_price_monthly, svname = "refiner_gasoline_price_monthly")

## 1.3 炼厂获得的原油成本
eia_cats(293660) # Refiner Acquisitiaon Cost of Crude Oil
refiner_oil_cost <- eia_cats(293661)$childcategories %>% as.data.table() # 通过获得渠道导出
refiner_oil_cost <- refiner_oil_cost[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(channel = name)]
refiner_oil_cost_monthly <- refiner_oil_cost[f == "M", rbindlist(eia_series(series_id)[["data"]]), by = .(channel, name, f, units, updated)] # 月度数据
sv(refiner_oil_cost_monthly, svname = "refiner_oil_cost_monthly")

## 1.4 NYMEX Future Prices
NYMEX_future_price <- eia_cats(241347)$childseries %>% as.data.table()
NYMEX_future_price_daily <- NYMEX_future_price[f == "D", rbindlist(eia_series(series_id)[['data']]), by = .(name, f, units, updated)] # 日度数据
sv(NYMEX_future_price_daily, svname = "NYMEX_future_price_daily")

## 1.5 美国炼厂各类产品价格(除汽油)
eia_cats(243458) # Refiner Petroleum Product Prices by Sales Type
refiner_petroleum_price <- eia_cats(244316)$childcategories %>% as.data.table() # 按售卖形式导出
refiner_petroleum_price <- refiner_petroleum_price[!str_detect(name, "Motor Gasoline"), rbindlist(list(eia_cats(category_id)$childseries)), by = .(sale_type = name)] # 除去汽油数据

refiner_petroleum_price_monthly <- refiner_petroleum_price[f == "M", rbindlist(eia_series(series_id)[['data']]), by = .(sale_type, name, f, units, updated)] # 月度数据导出

sv(refiner_petroleum_price_monthly, svname = "refiner_petroleum_price_monthly")


# 2. 炼化过程及产量 ----
refining_processing <- eia_cats(714759)$childcategories %>% as.data.table() # 炼化过程及产量
eia_cats(297218) # 炼厂周度净产量
eia_cats(297352) # 贸易商周度净产量
eia_cats(296961) # 炼厂与贸易商周度总净产量

# 2.0 炼厂与调油商总净产量（周度）
net_production_weekly <- eia_cats(296962)$childcategories %>% as.data.table() # 根据地区展开
net_production_weekly <- net_production[, rbindlist(eia_cats(category_id)$childseries %>% list()), by = .(area = name)]
net_production_weekly <- net_production[, rbindlist(eia_series(series_id)[['data']]), by = .(area, name, f, units, updated)]
sv(net_production_weekly, svname = "net_production_weekly")

# 2.1 炼厂（refiner）周度净产量
refiner_net_production_weekly <- eia_cats(297218)$childseries %>% as.data.table()
refiner_net_production_weekly <- refiner_net_production_weekly[, rbindlist(eia_series(series_id, cache = F)[['data']]), by = .(name, f, units, updated)]
sv(refiner_net_production_weekly, svname = "refiner_net_production_weekly")

# 2.2 贸易商（blender）周度净产量
blender_net_production_weekly <- eia_cats(297352)$childseries %>% as.data.table()
blender_net_production_weekly <- blender_net_production_weekly[, rbindlist(eia_series(series_id, cache = F)[['data']]), by = .(name, f, units, updated)]
sv(blender_net_production_weekly, svname = "blender_net_production_weekly")

# # 2.1  炼油厂（Refinery）净产量 ----
# eia_cats(301517)$childcategories %>% as.data.table()
# eia_cats(301518) #分区域
# eia_cats(302278) #分产品
# 
# ## 2.1.1 分地区
# oil_refiner_net_production_by_area <- eia_cats(301518)$childcategories %>% as.data.table()
# oil_refiner_net_production_by_area <- oil_refiner_net_production_by_area[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id, area_name = name)]
# 
# ## 2.1.2 分产品
# oil_refiner_net_production_by_product <- eia_cats(302278)$childcategories %>% as.data.table()
# oil_refiner_net_production_by_product <- oil_refiner_net_production_by_product[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id, product_name = name)]

# 以area进行分类的数据相比于product进行分类的数据更多
# 多出来的数据为LPG、Hydrocarbon Gas Liquids和Olefins气体等数据

# more <- oil_refiner_net_production_by_area[!(series_id %in% oil_refiner_net_production_by_product$series_id), .SD]

# 2.1.3 收率
eia_cats(304181) 
eia_cats(304460) # 按产成品分类
refiner_yield <- eia_cats(304460)$childcategories %>% as.data.table()
refiner_yield <- refiner_yield[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(type = name)]
refiner_yield <- refiner_yield[f == "M", rbindlist(eia_series(series_id)[["data"]]), by = .(type, name,f, units, updated )] # 月度数据导出
sv(refiner_yield, svname = "refiner_yield")

# 2.1.4 美国炼厂开工(周度)
eia_cats(296822)
# 以区域导出
refiner_utilization_weekly <- eia_cats(296823)$childcategories %>% as.data.table()
refiner_utilization_weekly <- refiner_utilization_weekly[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(area = name)]
refiner_utilization_weekly <- refiner_utilization_weekly[, rbindlist(eia_series(series_id)[['data']]), by = .(area, name, f, units, updated)]
sv(refiner_utilization_weekly, svname = "refiner_utilization_weekly")

# 2.1.5 美国炼厂分区产能（年度）
refiner_capacity <- eia_cats(304740)$childcategories %>% as.data.table()
refiner_capacity_annual <- refiner_capacity[, rbindlist(eia_cats(category_id)$childseries %>% list()), by = .(category_id, area = name)]
refiner_capacity_annual <- refiner_capacity_annual[, rbindlist(eia_series(series_id)[["data"]]), by = .(area, name, f, units, updated)]
sv(refiner_capacity_annual, svname = refiner_capacity_annual)

# 2.1.6 美国炼厂利用率与产量
refiner_utilization_capacity <- eia_cats(303762)$childcategories %>% as.data.table() # 按区域导出
refiner_utilization_capacity <- refiner_utilization_capacity[, eia_cats(category_id)$childseries %>% list() %>% rbindlist(), by = .(area = name)]
# 按月度导出
refiner_utilization_capacity_monthly <- refiner_utilization_capacity[f == "M", eia_series(series_id)[["data"]] %>% rbindlist(), by = .(area, name, f, units, updated)]
sv(refiner_utilization_capacity_monthly, svname = refiner_utilization_capacity_monthly)

# 3. 原油保存和产量 ----

# 4. 进出口及流动 ----
eia_cats(714760)
export_import <- eia_cats(714760)$childcategories %>% as.data.table()
# 4.1 出口和进口
eia_cats(314097) # 出口和进口（周度数据）
export_import <- eia_cats(314097)$childcategories %>% as.data.table()
# 4.1.1 按不同品类导出
export_import_weekly <- eia_cats(314300)$childcategories %>% as.data.table()
export_import_weekly <- export_import_weekly[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id, type = name)]
export_import_weekly <- export_import_weekly[, rbindlist(eia_series(series_id)[["data"]]), by = .(type, name, f, units, updated)]
sv(export_import_weekly, svname = export_import_weekly)

# 4.1.2 各州进口汽油及调油料----
# 汽油月度数据
district_import_gasoline <- eia_cats(380309)$childcategories %>% as.data.table()
district_import_gasoline <- district_import_gasoline[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(area = name)]
district_import_gasoline <- district_import_gasoline[f=="M", rbindlist(eia_series(series_id)[['data']]), by = .(area, name, f, units, updated)]
sv(district_import_gasoline, svname = district_import_gasoline)

# 调油料月度数据
district_import_gasoline_components <- eia_cats(381365)$childcategories %>% as.data.table()
district_import_gasoline_components <- district_import_gasoline_components[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(area = name)]
district_import_gasoline_components <- district_import_gasoline_components[f=="M", rbindlist(eia_series(series_id)[['data']]), by = .(area, name, f, units, updated)]
sv(district_import_gasoline_components, svname = district_import_gasoline_components)

# 5. 各类油品库存 ----
## 5.1 汽油库存
gasoline_stock_weekly <- eia_cats(387913)$childcategories %>% as.data.table()
gasoline_stock_weekly <- gasoline_stock_weekly[, rbindlist(eia_cats(category_id)$childseries %>% list()), by = .(area = name)]
gasoline_stock_weekly <- gasoline_stock_weekly[f == "W", rbindlist(eia_series(series_id)[["data"]]), by = .(area, name, f, units, updated)] # 导出周度数据
sv(gasoline_stock_weekly, svname = "gasoline_stock_weekly")


# 6. 消费及出售----
eia_cats(714803)
# 6.1 主要油品销量周度
eia_cats(401676) # 全美周度销量数据
gasoline_sales_weekly <- eia_cats(401676)$childseries %>% as.data.table()
gasoline_sales_weekly <- gasoline_sales_weekly[, rbindlist(eia_series(series_id)[['data']]), by = .(name, f)]
sv(gasoline_sales_weekly, svname = "gasoline_sales_weekly")

# 6.2 主要油品销售量
eia_cats(402211) # 油品销售量
gasoline_sales <- eia_cats(402212)$childcategories %>% as.data.table()
gasoline_sales <- gasoline_sales[, rbindlist(list(eia_cats(category_id)$childseries)), by = .(area = name)]
gasoline_sales <- gasoline_sales[str_detect(area, "U.S.|East Coast|Midwest|Rocky Mountain|Gulf Coast|West Coast") & f == "M", rbindlist(eia_series(series_id)[["data"]]), by = .(area, name, f, units, updated)] #月度
sv(gasoline_sales, svname = gasoline_sales)

# # 6.1.1 炼厂汽油销售量按产品分类
# eia_cats(413349)# 炼厂汽油销售量按产品分类
# refiner_gasoline_sales_by_product <- eia_cats(413349)$childcategories %>% as.data.table()
# 
# # 6.1.2 炼厂汽油销售量按产品和地区分类
# eia_cats(413351)# 炼厂汽油销售量按产品和地区分类
# refiner_gasoline_sales_by_product_area <- eia_cats(413351)$childcategories %>% as.data.table()
# refiner_gasoline_sales_by_product_area <- refiner_gasoline_sales_by_product_area[, setnames(.SD, "name", "area")]
# refiner_gasoline_sales_by_product_area <- refiner_gasoline_sales_by_product_area[, rbindlist(list(eia_cats(category_id, cache = F)$childseries)), by = .(category_id, area)]

# refiner_gasoline_sales_by_product_area_m <- refiner_gasoline_sales_by_product_area[f == "M", rbindlist(eia_series(series_id, cache = F)[["data"]]), by = .(area, name, f, units, updated)] # 月度数据
# sv(refiner_gasoline_sales_by_product_area_m, svname = "refiner_gasoline_sales_by_product_area_m")
# 
# refiner_gasoline_sales_by_product_area_a <- refiner_gasoline_sales_by_product_area[f == "A", rbindlist(eia_series(series_id, cache = F)[["data"]]), by = .(area, name, f, units, updated)] # 年度数据
# 
# # 6.1.3 炼厂汽油销售量按产品和出售类型分类
# eia_cats(414852)# 炼厂汽油销售量按产品和出售类型分类
# refiner_gasoline_sales_by_product_sale_type <- eia_cats(414852)$childcategories %>% as.data.table()

