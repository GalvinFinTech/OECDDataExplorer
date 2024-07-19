library(dbplyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(plotly)


ict <- read_excel("ICT.xlsx")

ict <- ict %>%
  rename(ID = !!names(.)[1])
str(ict)

# Tìm các giá trị NA trong dataframe
na_values <- is.na(ict)
# Tổng số giá trị NA trong mỗi cột
na_count <- colSums(na_values)
# Hiển thị số lượng giá trị NA trong mỗi cột
print(na_count)
#Xác định dòng có giá trị NA
ict$has_na <- ifelse(rowSums(is.na(ict)) > 0, 1, 0)
ict <- ict %>%
  mutate(across(where(is.character), ~if_else(is.na(.), "Unknown", .)))


# Tính giá trị trung bình của biến "Value" cho mỗi quốc gia
ict_avg <- ict %>%
  group_by(Country) %>%
  summarize(avg_value = mean(Value, na.rm = TRUE))

# Lọc dữ liệu cho các quốc gia và các giai đoạn khác nhau
ict_selected <- ict %>%
  filter(Country %in% c("Australia", "Brazil", "Colombia", "Finland", "New Zealand"),
         TIME < 2019 | (TIME >= 2019 & TIME <= 2022) | TIME > 2022)
# Tính giá trị trung bình cho mỗi quốc gia và mỗi giai đoạn
avg_values <- ict_selected %>%
  group_by(Country, GiaiDoan = case_when(
    TIME < 2019 ~ "Trước năm 2019",
    TIME >= 2019 & TIME <= 2022 ~ "Trong giai đoạn Covid-19",
    TIME > 2022 ~ "Sau năm 2022"
  )) %>%
  summarize(avg_value = mean(Value, na.rm = TRUE))
# Tính giá trị trung bình của chỉ số ICT cho mỗi quốc gia
avg_ict <- ict %>%
  group_by(Country) %>%
  summarize(avg_ict_value = mean(Value, na.rm = TRUE))
# Sắp xếp lại dữ liệu theo giá trị trung bình giảm dần
avg_ict <- avg_ict %>%
  arrange(desc(avg_ict_value))

