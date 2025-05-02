library(shiny)
library(ggplot2)
library(leaflet)
library(gt)
library(dplyr)
library(sf)
library(tidyr)
library(opendatatoronto)
library(scales)
library(osmdata)

packages <- search_packages("Traffic Volumes - Midblock Vehicle Speed, Volume and Classification Counts") 
package_id <- packages$id[1] 
resources <- list_package_resources(package_id) 
traffic_data <- get_resource(resources[3,'id'])
ui <- fluidPage(
  titlePanel("Toronto Traffic Analysis"),
  tabsetPanel(
    tabPanel("About",
             fluidRow(
               column(4, 
                      tags$div(
                        tags$p("This report will use the \"Traffic Volumes - Midblock Vehicle Speed, Volume and Classification Counts\" dataset provided by the City of Toronto through the opendatatoronto API."),
                        tags$p("This report will attempt to visualize and interpret various characteristics within traffic volumes and behaviour across Toronto. The analysis includes:"),
                        tags$ul(
                          tags$li("Examination of continuous variates and their relationships."),
                          tags$li("Geospatial visualization of traffic volume."),
                          tags$li("Categorical comparison of traffic volume on weekdays and weekends."),
                          tags$li("Tabular summary of driving behaviours in regions of interest.")
                        )
                      )
               )
             )
    ),
    tabPanel("Road Characteristics and Driving Behaviour
",
             fluidRow(
               column(4, 
                      tags$div(
                        tags$p("This hexbin plot attempts to visualize how traffic volume and average speed relate to driving behaviour. A driving behaviour of interest was how much faster the speeding drivers were driving compared to an average driver on the same road segment."),
                        tags$p("We have devised a 'speed spread' metric for this purpose, which is defined as the percentage difference between the 95th percentile and average speed, across urban roads in Toronto. The analysis focuses on roads with average speeds below 90 km/h to capture non-highway traffic behavior."),
                        tags$h5("Key insights include:"),
                        tags$ul(
                          tags$li("Low-volume, low-speed segments (<10,000 vehicles/day and <30 km/h) show high speed spread. Suggests greater variance in speed, between speeding drivers and average drivers."),
                          tags$li("High-volume roads show consistently low speed spread. We suspect it is likely due to a certain level of congestion that limits vehicle speeds."),
                          tags$li("Moderate speed segments (40–60 km/h) also show low speed spread, possibly reflecting well-designed arterial roads with smoother traffic flow.")
                        ),
                        tags$p("Overall, driver speed consistency appears strongest on busier roads, while quiet streets may see more varying behavior from a few speeding vehicles.")
                      )
               ),
               column(8, plotOutput("plot1", height = "500px"))
             )
    ),
    
    tabPanel("High-Volume Traffic Segments",
             fluidRow(
               column(4,
                      tags$div(
                        tags$p("This map shows high-traffic road segments in downtown Toronto, plotted over a map of Toronto found on OpenStreetMap framework. Each point plots a traffic monitoring location, color-scaled by average daily volume (yellow to red)."),
                        tags$p("The dataset used in the map has been filtered to include only the segments within a defined downtown boundary (longitude and latitude coordinates) suggested by a Google search, and with daily volume of at least 10,000 vehicles. This highlights key corridors in the location of interest (Downtown Toronto) and removes noise from minor roads."),
                        tags$h5("Key Insights include:"),
                        tags$ul(
                          tags$li("The highest traffic zones are center of downtown near Union Station and Bay Street, which are major downtown regions, as expected."),
                          tags$li("Outer downtown roads also exceed 10,000 vehicles/day but see less intensity."),
                          tags$li("Map suggests there are higher volumes spotted on vertical road segments like University Ave. than horizontal road segments.")
                        ),
                        tags$p("Overall, this spatial view highlights congestion hotspots and may be useful in selecting regions for further traffic planning, enforcement, and infrastructure upgrades.")
                      )
               ),
               column(8, plotOutput("plot2", height = "500px"))
             )
    ),
    
    tabPanel("Weekday vs Weekend Volumes on Comparable Segments",
             fluidRow(
               column(4,
                      tags$div(
                        tags$p("This violin plot compares the distribution of average daily traffic volume between weekdays and weekends across monitored road segments in Toronto. Only segments with data for both day types are included. To remove the influence of extreme values, segments with average volumes above 40,000 vehicles/day were excluded. This resulted in the removal of 2–3 values per category."),
                        tags$p("Each violin illustrates the shape of the distribution, with the white dot marking the median."),
                        tags$h5("Key Insights:"),
                        tags$ul(
                          tags$li("Both distributions are extremely similar in shape, excluding outliers."),
                          tags$li("Weekday traffic shows a higher median volume and greater spread, likely reflecting varied commuting patterns and busier routes."),
                          tags$li("Weekend traffic is more concentrated, with volumes typically between 5,000 and 15,000 vehicles/day, suggesting more uniform activity.")
                        ),
                        tags$p("Weekday traffic volumes appear to be slightly more dispersed and skewed toward higher values due to workday commuting, while weekend traffic appears more stable and centered across road segments. That being said, both distributions are very similar overall — which makes sense, as busy segments on weekdays are often busy on weekends too.")
                      )
               ),
               column(8, plotOutput("plot3", height = "500px"))
             )
    
  
  ),
    
  tabPanel("Segments Vulnerable to Speeding",
           fluidRow(
             column(4,
                    tags$div(
                      tags$p("This table displays the top 10 Toronto road segments in terms of highest speed spread, which is defined to be the percent difference between the 95th percentile and average speeds. The speed spread is used as a metric for extreme or inconsistent driver behavior on a particular road segment. Segments are also conditioned on location and flagged as 'Downtown' or 'Non-Downtown' based on coordinate boundaries defined in previous analysis."),
                      tags$p("Surprisingly, all top 10 segments are located outside downtown, presumably in extremely low-volume residential areas. The highest spread, over 280%, occurred on a segment averaging just 9 vehicles/day."),
                      tags$p("Table suggests that suburban streets, not downtown roads, may be most vulnerable to dangerous speeding and these potentially secluded areas may benefit from targeted traffic calming or enforcement.")
                    )
             ),
             column(8, gt_output("plot4"))
           )
  )))


server <- function(input, output) {
  output$plot1 <- renderPlot({
    df_filtered <- traffic_data %>%
      mutate(speed_spread = (avg_95th_percentile_speed - avg_speed) * 100 / avg_speed) %>%
      filter(!is.na(avg_daily_vol), !is.na(avg_speed), !is.na(speed_spread), avg_speed < 90)
    
    ggplot(df_filtered, aes(x = avg_daily_vol, y = avg_speed)) +
      stat_summary_hex(aes(z = speed_spread), bins = 30, fun = mean) +
      scale_fill_viridis_c(option = "magma", name = "Speed Spread (%)") +
      labs(
        title = "Speed Spread by Traffic Volume and Speed (Urban Roads)",
        subtitle = "Filtered to locations with avg speed < 90 km/h;",
        x = "Average Daily Volume (vehicles/day)",
        y = "Average Speed (km/h)"
      ) +
      theme_minimal(base_size = 13)
  })
  
  output$plot2 <- renderPlot({
    xmin <- -79.42
    xmax <- -79.36
    ymin <- 43.630
    ymax <- 43.675
    
    osm_bb <- c(left = xmin, bottom = ymin, right = xmax, top = ymax)
    streets <- opq(bbox = osm_bb) %>%
      add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary")) %>%
      osmdata_sf()
    
    traffic_sf <- traffic_data %>%
      filter(
        !is.na(latitude), !is.na(longitude), !is.na(avg_daily_vol),
        longitude >= xmin, longitude <= xmax,
        latitude  >= ymin, latitude  <= ymax,
        avg_daily_vol >= 10000
      ) %>%
      st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    ggplot() +
      geom_sf(data = streets$osm_lines, color = "gray60", size = 0.3, alpha = 0.5) +
      geom_sf(data = traffic_sf, aes(color = avg_daily_vol), size = 1.5, alpha = 0.8) +
      scale_color_gradient(name = "Avg Daily Volume", low = "gold", high = "red") +
      labs(
        title = "Toronto Traffic Volume (Downtown, High Volume)",
        subtitle = "Filtered to volume ≥ 10,000",
        x = "Longitude", y = "Latitude"
      ) +
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "gray90", size = 0.1),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        axis.text.y = element_text(size = 8)
      )
  })
  
  output$plot3 <- renderPlot({
    df_wdy_wknd <- traffic_data %>%
      filter(!is.na(avg_weekday_daily_vol), !is.na(avg_weekend_daily_vol))
    
    df_long <- df_wdy_wknd %>%
      select(centreline_id, avg_weekday_daily_vol, avg_weekend_daily_vol) %>%
      filter(avg_weekday_daily_vol <= 40000, avg_weekend_daily_vol <= 40000) %>%
      pivot_longer(cols = c(avg_weekday_daily_vol, avg_weekend_daily_vol),
                   names_to = "day_type", values_to = "volume") %>%
      mutate(day_type = recode(day_type,
                               avg_weekday_daily_vol = "Weekday",
                               avg_weekend_daily_vol = "Weekend"),
             day_type = factor(day_type, levels = c("Weekday", "Weekend")))
    
    ggplot(df_long, aes(x = day_type, y = volume, fill = day_type)) +
      geom_violin(trim = TRUE, alpha = 0.7) +
      stat_summary(fun = median, geom = "point", shape = 21, fill = "white", size = 3) +
      coord_cartesian(ylim = c(0, 40000)) +
      scale_fill_manual(values = c("lightblue", "pink")) +
      labs(
        title = "Distribution of Average Daily Volume: Weekday vs Weekend",
        subtitle = "Violin plot with media shown as white dot",
        x = NULL,
        y = "Average Daily Volume"
      ) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "none")
  })
  
  output$plot4 <- render_gt({
    downtown_bb <- list(lon_min = -79.42, lon_max = -79.36, lat_min = 43.63, lat_max = 43.675)
    
    df_top10_speed <- traffic_data %>%
      filter(!is.na(location_name), !is.na(avg_daily_vol), !is.na(avg_speed),
             !is.na(avg_95th_percentile_speed), !is.na(latitude), !is.na(longitude), avg_speed > 0) %>%
      mutate(speed_spread = (avg_95th_percentile_speed - avg_speed) * 100 / avg_speed,
             flag = ifelse(latitude >= downtown_bb$lat_min & latitude <= downtown_bb$lat_max &
                             longitude >= downtown_bb$lon_min & longitude <= downtown_bb$lon_max,
                           "Downtown", "Non-Downtown")) %>%
      select(location_name, speed_spread, avg_daily_vol, flag) %>%
      arrange(desc(speed_spread)) %>%
      slice(1:10)
    
    df_top10_speed %>%
      gt() %>%
      tab_header(
        title = "Top 10 Segments with Highest Speed Spread",
        subtitle = "Downtown or Non-downtown: conditioned on coordinates "
      ) %>%
      fmt_number(columns = c(speed_spread), decimals = 1) %>%
      fmt_number(columns = c(avg_daily_vol), sep_mark = ",", decimals = 0) %>%
      data_color(columns = c(speed_spread), colors = col_numeric(palette = c("orange", "red"), domain = NULL)) %>%
      data_color(columns = c(avg_daily_vol), colors = col_numeric(palette = c("lightblue", "blue"), domain = NULL)) %>%
      cols_label(location_name = "Road Segment", speed_spread = "Speed Spread (%)", avg_daily_vol = "Avg Daily Volume", flag = "Downtown") %>%
      tab_style(style = list(cell_fill(color = "lightgreen"), cell_text(weight = "bold", color = "darkgreen")),
                locations = cells_body(columns = c(flag), rows = flag == "Downtown"))
  })
}

shinyApp(ui, server)
