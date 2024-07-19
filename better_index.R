# Tải thư viện
library(dplyr)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(ggmap)
library(maps)
library(mapdata)
library(readxl)


# 1- Tải dữ liệu
data_oecd <- read_excel("BLI1.xlsx",skip = 1, 
                        col_types = c("text", rep("numeric", 24))) 
# Bỏ dòng đầu tiên của dataframe
data_oecd <- data_oecd[-1, ]
# Thay thế các giá trị NA bằng 0
data_oecd[is.na(data_oecd)] <- 0

data_oecd_normalized <- data_oecd %>%
  mutate_at(
    vars(-Indicator),  # Loại bỏ cột Indicator nếu có
    list(~  100*(.-min(.))/(max(.)-min(.)))  # Sử dụng danh sách lambda
  )

df_new <- data_oecd_normalized %>%
  transmute(
    Country = Indicator,
    Housing = (`Rooms per person` + `Housing expenditure` + `Dwellings without basic facilities`) / 3,
    Wealth = (`Household net adjusted disposable income` + `Household net wealth`) / 2,
    Jobs = (`Labour market insecurity` + `Personal earnings` + `Long-term unemployment rate` + `Employment rate`) / 4,
    Education = (`Years in education` + `Student skills` + `Educational attainment`) / 3,
    Environment = (`Water quality` + `Air pollution`) / 2,
    Engagement = (`Stakeholder engagement for developing regulations` + `Voter turnout`) / 2,
    Health = (`Self-reported health` + `Life expectancy`) / 2,
    Safety = (`Homicide rate` + `Feeling safe walking alone at night`) / 2,
    Work.life.balance = (`Time devoted to leisure and personal care` + `Employees working very long hours`) / 2,
    Community = `Quality of support network`,
    Life.satisfaction = `Life satisfaction`
  )

draw_world_map <- function(df, chartype) {
  # Tạo đối tượng biểu đồ world-map tương tác
  plot <- plot_geo(df) %>%
    add_trace(
      z = ~get(chartype),
      locations = ~Country,
      locationmode = "country names",
      type = "choropleth",
      color = ~get(chartype),
      colorscale = "Viridis",
      text = ~paste("Country: ", Country, "<br>", chartype, ": ", get(chartype))
    ) %>%
    layout(
      title = paste("World Map -", chartype),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = "natural earth")
      )
    )
  # Hiển thị biểu đồ
  plotly::ggplotly(plot)
}



