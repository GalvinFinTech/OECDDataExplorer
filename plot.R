
#--------------------------------------------
#  Load R packages 
#--------------------------------------------
pacman::p_load(tidyverse, rvest, xml2,
               #OECD,
               janitor, lubridate, ggtext, ggrepel, extrafont, scales, ggalt, zoo,
               countrycode, gghighlight, ggimage, glue, grid, png, leaflet)

pacman::p_loaded()

options(timeout = max(1000, getOption("timeout")))

# Select Open Sans font: 

my_font <- "Open Sans"

font_add_google(name = my_font, family = my_font)

showtext_auto()

# Extract GDP data by country: 

url <- "https://www.worldometers.info/gdp/gdp-by-country/"

url %>% 
  read_html() %>% 
  html_nodes(xpath = '//*[@id="example2"]') %>% 
  html_table() %>% 
  .[[1]] -> gdpData

#-------------------------------
# Stage 1: Data pro-processing
#-------------------------------

gdpData %>% 
  select(2, 3, 4) %>% 
  rename(gdp = `GDP (nominal, 2022)`, gdpAbb = `GDP (abbrev.)`) -> gdpData

gdpData %>% 
  mutate(gdp = str_replace_all(gdp, pattern = "\\$|\\,", replacement = ""), 
         gdpAbb = str_replace_all(gdpAbb, pattern = "\\$", replacement = "")) %>% 
  #mutate(unit = str_replace_all(gdpAbb, pattern = "[0-9]|\\.| ", replacement = "")) %>% 
  #mutate(unit = str_sub(unit, start = 1, end = 3)) %>% 
  #mutate(unit = StrCap(unit)) %>% 
  mutate(gdpAbb = str_replace_all(gdpAbb, pattern = "[a-z]| ", replacement = "")) %>% 
  mutate(gdp = as.numeric(gdp)) -> gdpData

gdpData %>% 
  mutate(shareGdp = 100*gdp / sum(gdp)) %>% 
  mutate(shareGdp = round(shareGdp, 1)) %>% 
  mutate(shareGdp = as.character(shareGdp)) %>% 
  mutate(shareGdp = case_when(!str_detect(shareGdp, "\\.") ~ str_c(shareGdp, ".0"), TRUE ~ shareGdp)) %>% 
  mutate(shareGdp = str_c(shareGdp, "%")) -> gdpData

gdpData %>% 
  mutate(label = str_c(Country, "\n", gdpAbb, " ", " (", shareGdp, ")")) -> gdpData



#--------------------------------------------
#  Stage 0: Load R packages and select font
#--------------------------------------------
library(rvest)
library(ggplot2)
library(stringr)
library(DescTools) # For capitalizing the first letter of a string. 
library(treemapify) # For ploting tree map chart. 
library(viridis) # For using Viridis Color Scales. 
library(showtext) # For using Google fonts. 
library(dplyr)
library(treemap)
library(plotly)

#--------------------------------------------
# Thiết lập giao diện đồ thị
theme_set(theme_minimal(base_size = 15))
tema_base <- theme(
  plot.title = element_markdown(size = 30),
  plot.subtitle = element_markdown(size = 15, lineheight = 1.2),
  axis.text.x = element_markdown(size = 12, hjust = 1),
  axis.text = element_markdown(size = 15, color = "black"),
  axis.line = element_line(color = "black"),
  panel.grid.minor.y = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.background = element_rect(colour = "white", fill = "white"),
  plot.background = element_rect(colour = "white", fill = "white"),
  plot.caption = element_markdown(hjust = -0.06, margin = unit(c(-5, 0, 0, 0), "mm"))
)

#---------------------------------------#

#Đồ thi hiển thị tỷ lệ việc làm của mõi nước gồm cà nam và nữ 
rate_plot <- function(data, years, countries) {
  filtered_data <- data %>%
    filter(year %in% years, country %in% countries) %>%
    mutate(iso2c = countrycode(country, origin = "iso3c", destination = "iso2c"))
  
  ggplot(filtered_data, aes(x = reorder(country, -emp, sum))) +
    geom_col(data = . %>% filter(sex == "_T"), aes(y = emp), fill = "#a9d297", width = 0.5) +
    geom_point(data = . %>% filter(sex != "_T"), aes(y = emp, fill = sex, shape = sex),
               color = "black", size = 4, show.legend = FALSE) +

    geom_flag(aes(image = iso2c), y = -5) +
    expand_limits(y = -5) +
    geom_hline(yintercept = 0) +
    coord_cartesian(clip = 'off') +
    scale_fill_manual(values = c("#ff9c9c","#6fa8dc")) +
    scale_shape_manual(values = c(24, 23)) +
    scale_y_continuous(breaks = seq(0, 60, 10)) +
    labs(
      title = "The Employment Rate Of OECD Countries -------------------------------------",
      subtitle = glue("In {years}, the employment rate of both men and women in each country."),
      x = "", y = "", color = "", fill = "", shape = ""
    ) +
    tema_base +
    theme(
      axis.text.x = element_markdown(angle = 90),
      axis.line = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, linetype = "dashed", color = "#d6d6d6"),
      legend.position = c(0.6, 0.9),
      legend.background = element_rect(color = "black", fill = "white"),
      legend.key = element_rect(color = "transparent", fill = "transparent"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    )
}
rate_plot(gradpc,2023,'FRA')


rate_plot <- function(data, years, countries) {
  filtered_data <- data %>%
    filter(year %in% years, country %in% countries) %>%
    mutate(iso2c = countrycode(country, origin = "iso3c", destination = "iso2c"))
  
  ggplot(filtered_data, aes(x = reorder(country, -emp, sum))) +
    geom_col(data = . %>% filter(sex == "_T"), aes(y = emp), fill = "#a9d297", width = 0.5) +
    geom_point(data = . %>% filter(sex != "_T"), aes(y = emp, fill = sex, shape = sex),
               color = "black", size = 4, show.legend = FALSE) +
    geom_flag(aes(image = iso2c), y = -5) +
    expand_limits(y = -5) +
    geom_hline(yintercept = 0) +
    coord_cartesian(clip = 'off') +
    scale_fill_manual(values = c("#ff9c9c","#6fa8dc")) +
    scale_shape_manual(values = c(24, 23)) +
    scale_y_continuous(breaks = seq(0, 60, 10)) +
    labs(
      title = "The Employment Rate Of OECD Countries",
      subtitle = glue("In {years}, the employment rate of both men and women in each country."),
      x = "", y = "", color = "", fill = "", shape = ""
    ) +
    tema_base +
    theme(
      axis.text.x = element_markdown(angle = 90),
      axis.line = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.3, linetype = "dashed", color = "#d6d6d6"),
      legend.position = c(0.6, 0.9),
      legend.background = element_rect(color = "black", fill = "white"),
      legend.key = element_rect(color = "transparent", fill = "transparent"),
      legend.text = element_text(size = 12),
      legend.title = element_blank()
    )
}

# Gọi hàm rate_plot() với dữ liệu gradpc
rate_plot(gradpc, 2023, 'FRA')
#------------------------------#

# Định nghĩa hàm để tạo đồ thị so sánh tỷ lệ làm việc của nam và nữ
create_plot <- function(data, countries, years) {
  filtered_data <- data %>%
    filter(year == years, country %in% countries, sex %in% c("M", "F"))
  
  plot <- ggplot(filtered_data, aes(x = reorder(country, -emp, sum), y = emp, fill = sex, text = paste("Tỷ lệ:", emp, "<br>Giới tính:", sex))) +
    geom_col() +
    labs(x = "Quốc gia", y = "Tỷ lệ việc làm") +
    scale_fill_manual(values = c("#a9d297", "#ff9c9c"), labels = c("Nam", "Nữ")) +
    theme_bw()
  
  plotly_plot <- ggplotly(plot, tooltip = "text", dynamicTicks = TRUE)
  
  return(plotly_plot)
}


#-------------------------------
#    Stage 2: Plot Tree Map 
#-------------------------------
create_gdp_treemap_1 <- function(gdpData, my_font) {
  # Load font from Google Fonts
  font_add_google(name = my_font, family = my_font)
  
  # Activate showtext
  showtext_auto()
  
  ggplot(gdpData, aes(area = gdp, fill = gdp, label = label)) + 
    geom_treemap(show.legend = FALSE) +
    geom_treemap_text(colour = "white",
                      place = "centre",
                      family = my_font, 
                      size = 13) + 
    scale_fill_viridis(option = "H", direction = -1) + 
    theme(legend.title = element_blank()) + 
    labs(title = "Nominal GDP Ranked by Country 2023", 
         subtitle = "Gross Domestic Product (GDP) is the monetary market value of all final goods and services made within a country during a specific period.\nAs of 2022, the United States and China would occupy the first two places. The US is ahead of China by $7 trillion in 2022 but the margin\nis coming down in nominal ranking as China's GDP growth rate of 2023 (5.01%) is higher than the US's 2.09%.", 
         caption = "Source: https://www.worldometers.info/gdp/gdp-by-country/") + 
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) + 
    theme(text = element_text(family = my_font)) +  
    theme(plot.title = element_text(size = 18, color = "grey10"), 
          plot.subtitle = element_text(size = 10, color = "grey30"), 
          plot.caption = element_text(color = "grey30", size = 8))
}
create_gdp_treemap_1(gdpData, "Open Sans")


#-------------------------------

plot_gdp_share <- function(gdp, years) {
  gdp_year <- subset(gdp, year == years)
  
  plot <- plot_ly(
    data = gdp_year,
    labels = ~country,
    values = ~shareGdp,
    type = "pie",
    text = ~paste0(country, ": ", shareGdp, "%"),
    hoverinfo = "text",
    textinfo = "label+percent",
    marker = list(colors = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd"))
  ) %>%
    layout(
      title = paste("Share of GDP by Country in", years),
      showlegend = FALSE
    )
  
  return(plot)
}

plot_gdp_share(gdp, 2022)
#-------------------------------
#    Stage 4: Map chart 
#-------------------------------
# Function to create dynamic world map
create_world_map <- function(data, years) {
  filtered_data <- subset(data, year == years)
  
  plot_ly() %>%
    add_trace(
      type = 'choropleth',
      locations = filtered_data$country,
      locationmode = 'ISO-3',
      z = filtered_data$pop,
      text = paste("Country: ", filtered_data$country, "<br>Population: ", filtered_data$pop),
      colorscale = "Viridis",
      colorbar = list(title = "Population"),
      marker = list(line = list(color = 'rgb(255,255,255)', width = 2)),
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "<extra>Population: %{z}</extra>"
      )
    ) %>%
    layout(
      title = paste("World Population in", years),
      geo = list(
        showframe = FALSE,
        showcoastlines = TRUE,
        projection = list(type = "orthographic"),
        resolution = 50,
        lonaxis = list(showgrid = FALSE),
        lataxis = list(showgrid = FALSE)
      ),
      scene = list(
        camera = list(
          eye = list(x = 1.2, y = 1.2, z = 0.8),
          up = list(x = 0, y = 0, z = 1)
        ),
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        zaxis = list(visible = FALSE)
      ),
      updatemenus = list(
        list(
          type = "buttons",
          buttons = list(
            list(
              args = list(
                geo = list(projection = list(type = "orthographic")),
                scene = list(camera = list(eye = list(x = 1.2, y = 1.2, z = 0.8), up = list(x = 0, y = 0, z = 1)))
              ),
              label = "Orthographic",
              method = "relayout"
            ),
            list(
              args = list(
                geo = list(projection = list(type = "equirectangular")),
                scene = list(camera = list(eye = list(x = 1.2, y = 1.2, z = 0.8), up = list(x = 0, y = 0, z = 1)))
              ),
              label = "Equirectangular",
              method = "relayout"
            )
          )
        )
      )
    )
}

create_world_map(pop, 2023)
#-------------------------------

#    Stage 4: Line-bar chart 
#-------------------------------
create_employment_rate_plot <- function(year_bar, year_scatter, countries) {
  data_bar <- emp %>%
    filter(year == year_bar, sex == '_T', country %in% countries)
  
  data_scatter <- emp %>%
    filter(year == year_scatter, sex == '_T', country %in% countries)
  
  plot_ly() %>%
    add_trace(
      data = data_bar,
      x = ~country,
      y = ~emp,
      type = 'bar',
      marker = list(color = '#6fa8dc', line = list(color = '#2c5d83', width = 1)),
      name = paste("Năm", year_bar),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Tỷ lệ việc làm: %{y}<extra></extra>"
      )
    ) %>%
    add_trace(
      data = data_scatter,
      x = ~country,
      y = ~emp,
      type = 'bar',
      marker = list(color = 'red', line = list(color = 'darkred', width = 1)),
      name = paste("Năm", year_scatter),
      hovertemplate = paste(
        "<b>%{x}</b><br>",
        "Tỷ lệ việc làm: %{y}<extra></extra>"
      )
    ) %>%
    layout(
      title = paste("Tỷ lệ việc làm của các quốc gia năm", year_scatter, "so với năm", year_bar),
      xaxis = list(title = "Quốc gia"),
      yaxis = list(title = "Tỷ lệ việc làm"),
      hovermode = "closest",
      showlegend = TRUE,
      barmode = 'group'
    )
}




create_gdpGrowth <- function(gdp_data, countries, start_year, end_year) {
  # Filter GDP data for the selected countries and time range
  gdp_filtered <- gdp_data %>%
    filter(country %in% countries, year >= start_year, year <= end_year)
  
  # Create the interactive chart with custom line style and annotations
  num_countries <- length(countries)
  color_palette <- if (num_countries >= 3) {
    RColorBrewer::brewer.pal(num_countries, "Set2")
  } else {
    RColorBrewer::brewer.pal(3, "Set2")[1:num_countries]
  }
  
  p <- plot_ly(gdp_filtered, x = ~year, y = ~gdpAbb, color = ~country, type = "scatter", mode = "lines+markers",
               text = ~paste("GDP:", gdpAbb), colors = color_palette) %>%
    layout(title = paste("Interactive GDP Chart for", paste(countries, collapse = ", ")),
           xaxis = list(title = "Year", range = c(start_year, end_year)),
           yaxis = list(title = "GDP"),
           hoverlabel = list(bgcolor = "white", font = list(family = "Arial", size = 12, color = "black")))
  
  p
}





