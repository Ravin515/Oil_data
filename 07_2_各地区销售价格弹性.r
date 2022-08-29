library(styleer)
library(openxlsx)

# 1. 油品消费与各地GDP
# 各地区油品消费数据（月度）
steo <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/eia_consumption.xlsx") %>% as.data.table()
consumption <- steo[, melt(steo, id = 2, measure = 7:330)
    ][, variable := convertToDate(as.character(variable), origin = "1899/12/30")
    ][, setnames(.SD, "variable", "date")
    ][, year := year(date)
    ][, quarter := quarter(date)
    ][str_detect(type, "Consumption"), .SD
    ][, area := fcase(str_detect(type, "Total OECD"), "OECD",
    str_detect(type, "U.S.") & str_detect(type, "50"), "United States",
    str_detect(type, "Canada"), "Canada",
    str_detect(type, "Japan"), "Japan",
    str_detect(type, "China"), "China"
    )
    ]

# 各国家GDP同比增长率（季度）
library(countrycode)
oecd_gdp <- fread("C:/Users/MrStylee/Desktop/出行指数/oecd_gdp.csv")
gdp_quarter <- oecd_gdp[, area := countrycode(LOCATION, origin = 'iso3c', destination = "country.name", warn = F)
    ][, area := fifelse(is.na(area), LOCATION, area)
    ][FREQUENCY == "Q" & MEASURE == "PC_CHGPY", .SD
    ][, year := str_sub(TIME, start = 1L, end = 4L) %>% as.integer()
    ][, quarter := str_sub(TIME, start = -1L, end = -1L) %>% as.integer()
    ][year >= 1998, .SD
    ][order(area, quarter, year), .SD
    ][, cum_value := 1 + Value/100
    ][, cum_value := cumprod(cum_value), by = .(area, quarter)
    ][, .(cum_value, area, quarter, year)]

# 合并GDP增长率和消费数据
gdp_consumption <- gdp_quarter[consumption, on = .(area, year, quarter)
    ][!is.na(area), .SD
    ][, value := as.numeric(value)
    ][, consumption_gdp := fifelse(is.na(cum_value), value, value/cum_value)
    ][order(area, year, quarter), .SD
    ][!(year>=2022&is.na(cum_value)), .SD] 

# 2. 美国通胀数据与WTI价格
# 美国通胀数据（月度）
inflation_rate <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/inflation_rate.xlsx", sheet = "Sheet 1") %>% as.data.table()
inflation_rate <- inflation_rate[, melt(.SD, id.vars = c("Year"), measure.vars = colnames(.SD)[2:14])
    ][, setnames(.SD, 1:3, c("year", "month", "inflt_rate"))
    ][month != "Annual", .SD
    ][, month := match(month, month.abb)
    ][, year := as.character(year)
    ][, date := str_c(year,month, "01", sep = "-")
    ][, date := as.Date(date, "%Y-%m-%d")
    ][year >= 1997, .SD
    ][order(year, month), .SD
    ]

# 美国cpi数据（月度）
cpi <- readxl::read_xlsx("C:/Users/MrStylee/Desktop/出行指数/inflation_rate.xlsx", sheet = "Sheet2") %>% as.data.table()
cpi <- cpi[, melt(.SD, id.vars = c("Year"), measure.vars = colnames(.SD)[2:14])
    ][, setnames(.SD, 1:3, c("year", "month", "cpi"))
    ][month != "Annual", .SD
    ][, month := match(month, month.abb)
    ][, year := as.character(year)
    ][, date := str_c(year,month, "01", sep = "-")
    ][, date := as.Date(date, "%Y-%m-%d")
    ][year >= 1997, .SD
    ][order(year, month), .SD
    ][!is.na(cpi), .SD]


# WTI数据（日度）
wti_price <- readxl::read_xls("C:/Users/MrStylee/Desktop/出行指数/wti_price.xls") %>% as.data.table()
wti_price <- wti_price[, date := as.Date(date)
    ][!is.na(wti_price), .SD
    ][, ':='(year = year(date) %>% as.character(), month = month(date))
    ][order(year, month), .SD
    ][, .SD[.N], .SDcols = 2, by = .(year, month)
    ][year >= 1997, .SD
    ][, .SD[-.N]]

# WTI价格数据与CPI数据合并
price_cpi <- cpi[wti_price, on = .(year, month)]
price_cpi[, price_cpi := wti_price*(cpi[1]/cpi)]

# 3. 价格与消费量合并
# 3.1 WTI价格以1997年作为基准消除通胀，Consumption以1997年作为基期消除GDP影响
pic_fit <- price_cpi[gdp_consumption, on = .(date)
    ][, area := as.factor(area)]
ggplot(pic_fit[area != "OECD"], aes(x = consumption_gdp, y = price_cpi, colour = area)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    theme_grey() +
        labs(x = "消费量(mb/d)", y = "WTI价格(d/b)", title = "需求-价格") +
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
        legend.key.size = unit(0.5, 'cm'))

# 3.2 WTI价格以最后一年作为基准消除通胀，Consumption以1997年作为基期消除GDP影响
price_cpi[, price_cpi_22 := wti_price*(cpi[.N]/cpi)]
pic_fit <- price_cpi[gdp_consumption, on = .(date)
    ][, area := as.factor(area)]
ggplot(pic_fit[area != "OECD"], aes(x = consumption_gdp, y = price_cpi_22, colour = area)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    theme_grey() +
        labs(x = "消费量(mb/d)", y = "WTI价格(d/b)", title = "需求-价格") +
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
        legend.key.size = unit(0.5, 'cm'))

ggplot(pic_fit[(area == "United States" & (consumption_gdp > 15 | price_cpi > 50) & consumption_gdp > 12.5) | 
(area == "Canada" & consumption_gdp > 1.55)| 
(area == "China" & (consumption_gdp > 2.5 | price_cpi > 50) & consumption_gdp > 2.3) | 
(area == "Japan" & (consumption_gdp > 4.3| price_cpi > 50) & consumption_gdp > 3.75)], aes(x = consumption_gdp, y = price_cpi, colour = area)) +
    geom_point(size = 1) +
    geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = F) +
    theme_grey() +
        labs(x = "消费量(mb/d)", y = "WTI价格(d/b)", title = "需求-价格") +
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
        legend.key.size = unit(0.5, 'cm'))

