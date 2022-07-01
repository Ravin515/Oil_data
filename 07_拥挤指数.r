library(styleer)
library(readxl)
ld(gasoline_sales_weekly)

# 读取所有sheet name
readxl::excel_sheets("congestion_data.xlsx")

# 读取谷歌、TomTom和China-15数据
# 谷歌 #在excel里先把多余的行删除
google_mobility <- read_excel("congestion_data.xlsx", sheet = "Google") %>% as.data.table() %>% setnames(1, "date")
    

# TomTom
tomtom <- read_excel("congestion_data.xlsx", sheet = "TomTom_data") %>% as.data.table() %>% setnames(1, "date")

# China-15数据
China_15 <- read_excel("congestion_data.xlsx", sheet = "China-15 ") %>% as.data.table()

China_15 <- setnames(China_15, 1:length(colnames(China_15)), c("city", colnames(China_15)[-1]%>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character())) %>% melt(measure.vars = patterns("^20"),variable.name = "date", value.name = "index")
China_15[, date := as.Date(date)]

# China city
China_city_level <- read_excel("congestion_data.xlsx", sheet = "China city-level") %>% as.data.table()

China_city_level <- setnames(China_city_level, 1:length(colnames(China_city_level)), c("city", "name", colnames(China_city_level)[-(1:2)]%>% as.numeric() %>% as.Date(origin = "1899-12-30") %>% as.character()))%>% melt(measure.vars = patterns("^20"), variable.name = "date", value.name = "index")
China_city_level[, date := as.Date(date)]