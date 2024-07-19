library(jsonlite)
library(httr)
library(xts)
library(dygraphs)
library(jsonlite)
library(httr)
library(DT)
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(forecast)
library(tseries)
library(lubridate)
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(dygraphs)
# URL chứa dữ liệu JSON
number_country <- 0
url <- "https://stats.oecd.org/SDMX-JSON/data/MEI_FIN/CCUS.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA+EA19+SDR+NMEC+ARG+BRA+CHN+IND+IDN+RUS+ZAF.M/all?startTime=2023-01&endTime=2024-01&dimensionAtObservation=allDimensions"
# Tải dữ liệu từ URL
response <- httr::GET(url)

# Kiểm tra mã trạng thái của phản hồi
if (httr::http_error(response)) {
  stop("Lỗi khi tải dữ liệu từ URL: ", httr::http_status(response)$message)
}

# Đọc dữ liệu JSON từ phản hồi
json_data <- httr::content(response, as = "text")

# Chuyển đổi dữ liệu JSON thành list
json_list <- jsonlite::fromJSON(json_data)
# Tạo một danh sách để lưu trữ thông tin
info_list <- list()
# 1. Trích xuất thông tin structure
for (i in 1:length(json_list$structure$dimensions$observation$name)) {
  # Trích xuất tên
  name <- json_list$structure$dimensions$observation$name[[i]]
  # Trích xuất id
  value <- json_list$structure$dimensions$observation$values[[i]]$name
  if (identical(value, rep(NA, length(value)))) {
    value <- json_list$structure$dimensions$observation$values[[i]]$id
  }
  # Kiểm tra xem trường "value" có dữ liệu hay không
  if(name == "Subject" || name == "Frequency"){
    if(is.vector(value)){
      value <-  value[1]
    }
    #info_list[[name]] <- value
  }else if(name == "Country"){
    info_list[[name]] <- value
    number_country <- length(value)
    str(number_country)
  }else{
    # value <- Year
    str("number_country_index")
    str(value)
    
    for (i in 1:length(value)) {
      val <- list()
      #
      for(number_country_index in 1:number_country) {
        # Trích xuất tên
        name <- as.character(value[[i]])
        
        # Trích xuất giá trị
        
        # Lặp qua danh sách "observations"
        keys <- names(json_list$dataSets$observations)
        j <- i
        if(number_country_index > 1){
          j <- (number_country_index * 2) + i + 1
        }
        if(j <= length(keys)){
          observation_key <- keys[[j]]
          # Truy cập vào phần tử "observations" trong mỗi phần tử "observations"
          observation <- json_list$dataSets$observations[[observation_key]][[1]][1]
          if (is.na(observation)) {
            observation <- '---'
          }
          # lưu giá trị vào list value
          val <- append(val, list(observation))
        }else{
          observation <- '---'
          # lưu giá trị vào list value
          val <- append(val, list(observation))
        }
      }
      #
      val <- unlist(val)
      str(val)
      # Thêm thông tin vào danh sách
      info_list[[name]] <- val
    }
  }
  
}

data_list <- list()

# Duyệt qua các phần tử trong info_list
for (name in names(info_list)) {
  # Tạo một cột trong data_list với tên là name và giá trị từ info_list
  data_list[[name]] <- info_list[[name]]
}

# Tính độ dài lớn nhất của tất cả các vector con trong danh sách
max_length <- max(sapply(data_list, length))

# Thêm NA vào các vector con để đảm bảo chúng có cùng độ dài
data_list_padded <- lapply(data_list, function(x) {
  if (length(x) < max_length) {
    c(x, rep(NA, max_length - length(x)))
  } else {
    x
  }
})

# Tạo dataframe từ danh sách đã điều chỉnh
df <- data.frame(data_list_padded)

# Chuyển đổi dữ liệu từ dạng rộng sang dạng dài
df_long <- pivot_longer(df, cols = starts_with("Jan"):starts_with("May"), names_to = "Month", values_to = "Value")

# Tính toán thống kê mô tả cho mỗi quốc gia
summary_by_country <- df_long %>%
  group_by(Country) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE)
  )

# Tính toán thống kê mô tả cho mỗi tháng
summary_by_month <- df_long %>%
  group_by(Month) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE)
  )
# Tìm quốc gia có biến động lớn nhất và nhỏ nhất
quoc_gia_bien_dong_nhat <- summary_by_country[which.max(summary_by_country$SD), 'Country']
quoc_gia_on_dinh_nhat <- summary_by_country[which.min(summary_by_country$SD), 'Country']

# Xác định đúng tên quốc gia có biến động tỉ giá hối đoái lớn nhất
quoc_gia_bien_dong_nhat <- summary_by_country[which.max(summary_by_country$SD), ]$Country

# Chuyển đổi dữ liệu từ dạng rộng sang dạng dài
df_long <- pivot_longer(df, cols = -Country, names_to = "Month", values_to = "Value")

# Xử lý cột 'Month'
df_long <- df_long %>%
  mutate(
    Month = gsub("\\.", "", Month), # Loại bỏ dấu chấm
    Month = paste0("01-", Month), # Thêm ngày "01-" vào đầu
    Month = dmy(Month) # Chuyển đổi chuỗi thành ngày tháng
  )

# Tạo dữ liệu mẫu cho price_data
if (!exists("price_data")) {
  price_data <- xts::xts(runif(100, min = 50, max = 100), order.by = Sys.Date() - 100:1)
}


# Đảm bảo rằng đã tạo price_data và là một xts object
# Tạo một xts object mẫu
if (!exists("price_data")) {
  price_data <- xts::xts(rnorm(100), order.by = Sys.Date() - 100:1)
}

# Tính RSI từ dữ liệu giá
rsi_values <- TTR::RSI(price_data, n = 14)

# Tính MACD từ dữ liệu giá
macd_values <- TTR::MACD(price_data, nFast = 12, nSlow = 26, nSig = 9)
# Chuẩn bị dữ liệu cho biểu đồ
combined_data <- cbind(Price = price_data, RSI = rsi_values, MACD = macd_values$macd)

# Chuyển đổi dữ liệu thành đối tượng xts
combined_xts <- xts::xts(combined_data, order.by = index(price_data))



