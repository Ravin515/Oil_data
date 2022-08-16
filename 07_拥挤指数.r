library(styleer)
library(readxl)


# # 读取所有sheet name
# readxl::excel_sheets("congestion_data.xlsx")

# # 读取谷歌、TomTom和China-15数据
# # 谷歌 #在excel里先把多余的行删除
# google_mobility <- read_excel("congestion_data.xlsx", sheet = "Google") %>% as.data.table() %>% setnames(1, "date")
    

# # TomTom
# tomtom <- read_excel("congestion_data.xlsx", sheet = "TomTom_data") %>% as.data.table() %>% setnames(1, "date")

# # China-15数据
# China_15 <- read_excel("congestion_data.xlsx", sheet = "China-15 ") %>% as.data.table()

# China_15 <- setnames(China_15, 1:length(colnames(China_15)), c("city", colnames(China_15)[-1]%>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character())) %>% melt(measure.vars = patterns("^20"),variable.name = "date", value.name = "index")
# China_15[, date := as.Date(date)]

# # China city
# China_city_level <- read_excel("congestion_data.xlsx", sheet = "China city-level") %>% as.data.table()

# China_city_level <- setnames(China_city_level, 1:length(colnames(China_city_level)), c("city", "name", colnames(China_city_level)[-(1:2)]%>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character()))%>% melt(measure.vars = patterns("^20"), variable.name = "date", value.name = "index")
# China_city_level[, date := as.Date(date)]

# 全美周度消费量
ld(gasoline_sales_weekly)
motor_gasoline_sales_weekly <- gasoline_sales_weekly[str_detect(name, "Finished Motor Gasoline"), .SD
    ][, dcast(.SD, date + week ~ name, value.var = "value")]
# writexl::write_xlsx(motor_gasoline_sales_weekly, "motor_gasoline_sales_weekly.xlsx")

# 出行指数(Google)
congestion_index <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/出行指数8.5.xlsx", sheet = "Google") %>% as.data.table() %>% setnames(1, "date")
congestion_index[, date := as.Date(date)]

# 出行指数(TomTom)
congestion_index_tomtom <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/出行指数8.5.xlsx", sheet = "tom") %>% as.data.table() %>% setnames(1, "date")
congestion_index_tomtom[, date := as.Date(date)]

demand_travel <- motor_gasoline_sales_weekly[congestion_index, on = .(date), roll = -Inf]

demand_travel <- congestion_index_tomtom[demand_travel, on = .(date)]

writexl::write_xlsx(demand_travel, "demand_travel.xlsx")