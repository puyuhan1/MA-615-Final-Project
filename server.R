#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(mapdata)
library(leaflet)
library(xml2)
library(readxl)
library(dplyr)
library(rsconnect)
#for general description#
nauru_coords <- c(166.9315, -0.5228)  # Longitude, Latitude
world_map_plot <- ggplot() +
    geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), fill = "bisque", color = "white") +
    geom_point(data = data.frame(lon = nauru_coords[1], lat = nauru_coords[2]), aes(x = lon, y = lat), color = "red", size = 3) +
    theme_minimal()

# Define the leaflet map for Nauru
leaflet_nauru_map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    setView(lng = nauru_coords[1], lat = nauru_coords[2], zoom = 10) %>%
    addCircleMarkers(lng = nauru_coords[1], lat = nauru_coords[2], color = "red", radius = 8, popup = "Nauru")
#for demographics#
xml_file_path <- "annual_population_change.xml"
xml_data <- read_xml(xml_file_path)
records <- xml_find_all(xml_data, ".//record")
records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})

annual <- bind_rows(records_data)

annual$CountryOrArea <- as.character(annual$CountryOrArea)
annual$Year <- as.character(annual$Year)
annual$Variant <- as.character(annual$Variant)

annual<-na.omit(annual)

annual$Year <- as.numeric(annual$Year)

annualplot<-ggplot(annual, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(annual$Year), max(annual$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Population Change for Nauru",
        x = "Year",
        y = "Population Change Value"
    )
xml_file_path <- "life_expectancy.xml"

xml_data <- read_xml(xml_file_path)

records <- xml_find_all(xml_data, ".//record")

records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})

lifeexpect <- bind_rows(records_data)

lifeexpect$CountryOrArea <- as.character(lifeexpect$CountryOrArea)
lifeexpect$Year <- as.character(lifeexpect$Year)
lifeexpect$Variant <- as.character(lifeexpect$Variant)

lifeexpect<-na.omit(lifeexpect)

lifeexpect$Year <- as.numeric(lifeexpect$Year)

lifeexpectplot<-ggplot(lifeexpect, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(lifeexpect$Year), max(lifeexpect$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Life Expectancy for Nauru",
        x = "Year",
        y = "Life Expectancy"
    )
xml_file_path <- "fertility.xml"

xml_data <- read_xml(xml_file_path)

records <- xml_find_all(xml_data, ".//record")

records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})

fertility <- bind_rows(records_data)

fertility$CountryOrArea <- as.character(fertility$CountryOrArea)
fertility$Year <- as.character(fertility$Year)
fertility$Variant <- as.character(fertility$Variant)

fertility<-na.omit(fertility)

fertility$Year <- as.numeric(fertility$Year)

fertilityplot<-ggplot(fertility, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(fertility$Year), max(fertility$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Total Fertility Rate for Nauru",
        x = "Year",
        y = "Total Fertility Rate"
    )
#for Comparison#
xml_file_path <- "Tuvalu_annual.xml"

xml_data <- read_xml(xml_file_path)

records <- xml_find_all(xml_data, ".//record")

records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})

Tannual <- bind_rows(records_data)

Tannual$CountryOrArea <- as.character(Tannual$CountryOrArea)
Tannual$Year <- as.character(Tannual$Year)
Tannual$Variant <- as.character(Tannual$Variant)

Tannual<-na.omit(Tannual)

Tannual$Year <- as.numeric(Tannual$Year)

Tannualplot<-ggplot(Tannual, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(Tannual$Year), max(Tannual$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Population Rate for Tuvalu",
        x = "Year",
        y = "Annual Population Rate"
    )
#for comparison#
annual$Group <- 'Nauru'
Tannual$Group <- 'Tuvalu'

combined_df <- bind_rows(annual, Tannual)

combined_df$Year <- as.numeric(as.character(combined_df$Year))

Comparisonannual<-ggplot(combined_df, aes(x = Year, y = Value, group = Group, color = Group)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(combined_df$Year), max(combined_df$Year), by = 20)) +  
    theme_minimal() +  
    labs(title = "Annual Rate of Population Comparison", x = "Year", y = "Value") +
    scale_color_manual(values = c("Nauru" = "blue", "Tuvalu" = "red"))
#for Comparison#
xml_file_path <- "Tuvalu_Life.xml"

xml_data <- read_xml(xml_file_path)
records <- xml_find_all(xml_data, ".//record")

records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})

Tlifeexpect <- bind_rows(records_data)

Tlifeexpect$CountryOrArea <- as.character(Tlifeexpect$CountryOrArea)
Tlifeexpect$Year <- as.character(Tlifeexpect$Year)
Tlifeexpect$Variant <- as.character(Tlifeexpect$Variant)

Tlifeexpect<-na.omit(Tlifeexpect)

Tlifeexpect$Year <- as.numeric(Tlifeexpect$Year)

Tlifeplot<-ggplot(Tlifeexpect, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(Tlifeexpect$Year), max(Tlifeexpect$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Life Expectancy for Tuvalu",
        x = "Year",
        y = "Annual Life Expectancy"
    )
#for comparison#
lifeexpect$Group <- 'Nauru'
Tlifeexpect$Group <- 'Tuvalu'

combined_df <- bind_rows(lifeexpect, Tlifeexpect)

combined_df$Year <- as.numeric(as.character(combined_df$Year))

Comparisonlife<-ggplot(combined_df, aes(x = Year, y = Value, group = Group, color = Group)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(combined_df$Year), max(combined_df$Year), by = 20)) +  
    theme_minimal() +  
    labs(title = "Annual Life Expectancy Comparison", x = "Year", y = "Value") +
    scale_color_manual(values = c("Nauru" = "blue", "Tuvalu" = "red"))
xml_file_path <- "Tuvalu_fertility.xml"

xml_data <- read_xml(xml_file_path)

records <- xml_find_all(xml_data, ".//record")

records_data <- lapply(records, function(record) {
    country_area <- xml_text(xml_find_first(record, ".//field[@name='Country or Area']"))
    year <- xml_text(xml_find_first(record, ".//field[@name='Year(s)']"))
    variant <- xml_text(xml_find_first(record, ".//field[@name='Variant']"))
    value <- xml_text(xml_find_first(record, ".//field[@name='Value']"))
    
    data.frame(CountryOrArea = country_area, 
               Year = year, 
               Variant = variant, 
               Value = as.numeric(value))
})
Tfertility <- bind_rows(records_data)

Tfertility$CountryOrArea <- as.character(Tfertility$CountryOrArea)
Tfertility$Year <- as.character(Tfertility$Year)
Tfertility$Variant <- as.character(Tfertility$Variant)

Tfertility<-na.omit(Tfertility)

Tfertility$Year <- as.numeric(Tfertility$Year)

Tfertilityplot<-ggplot(Tfertility, aes(x = Year, y = Value)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(Tfertility$Year), max(Tfertility$Year), by = 20)) +
    theme_minimal() + 
    labs(
        title = "Annual Total Fertility Rate for Tuvalu",
        x = "Year",
        y = "Total Fertility Rate"
    )
#for comparison#
fertility$Group <- 'Nauru'
Tfertility$Group <- 'Tuvalu'

combined_df <- bind_rows(fertility, Tfertility)

combined_df$Year <- as.numeric(as.character(combined_df$Year))

Comparisonfertility<-ggplot(combined_df, aes(x = Year, y = Value, group = Group, color = Group)) +
    geom_line() +  
    geom_point() +  
    scale_x_continuous(breaks = seq(min(combined_df$Year), max(combined_df$Year), by = 20)) + 
    theme_minimal() +  
    labs(title = "Total Fertility for Women", x = "Year", y = "Value") +
    scale_color_manual(values = c("Nauru" = "blue", "Tuvalu" = "red"))
#For SWOT#
xml_data <- read_xml("Airport.xml")

records <- xml_find_all(xml_data, ".//record")

Airport <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
xml_data <- read_xml("Import_and_Export.xml")

records <- xml_find_all(xml_data, ".//record")

ImandEx <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Element = xml_text(xml_find_first(records, ".//field[@name='Element']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Unit = xml_text(xml_find_first(records, ".//field[@name='Unit']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
Import<-subset(ImandEx, Element == "Import Value")
Export<-subset(ImandEx, Element == "Export Value")
xml_data <- read_xml("GDP_PPP.xml")

records <- xml_find_all(xml_data, ".//record")

GDPPPP <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
GDPPPP$Source <- 'Ratio of GDP'
Airport$Source <- 'Airline Carrier Departure'
Import$Source <- 'Import of Forest Product'
Export$Source <- 'Export of Forest Product'
GDPPPP$Value <- GDPPPP$Value/50000
combined_df <- bind_rows(GDPPPP, Airport, Import, Export)

combined_df$Year <- as.factor(combined_df$Year)

Weaknessplot<-ggplot(combined_df, aes(x = as.numeric(Year), y = Value, group = Source, color = Source)) +
    geom_line() +  
    geom_point() + 
    scale_x_continuous(breaks = seq(min(as.numeric(combined_df$Year)), max(as.numeric(combined_df$Year)), by = 20)) +
    theme_minimal() +  
    labs(title = "Value by Year from Multiple Datasets", x = "Year", y = "Value") +
    scale_color_manual(values = c('Ratio of GDP' = 'red', 'Airline Carrier Departure' = 'blue', 'Import of Forest Product' = 'green', 'Export of Forest Product' = 'orange'))
#for strength#
xml_data <- read_xml("Chicken.xml")

records <- xml_find_all(xml_data, ".//record")

Chicken <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Element = xml_text(xml_find_first(records, ".//field[@name='Element']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Unit = xml_text(xml_find_first(records, ".//field[@name='Unit']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
Chicken$Value <- Chicken$Value*1000
xml_data <- read_xml("Pig.xml")

records <- xml_find_all(xml_data, ".//record")

Pigs <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Element = xml_text(xml_find_first(records, ".//field[@name='Element']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Unit = xml_text(xml_find_first(records, ".//field[@name='Unit']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
Pigs<-subset(Pigs,Unit=="Head")
head(Pigs)
xml_data <- read_xml("Coconut.xml")

records <- xml_find_all(xml_data, ".//record")

Coconuts <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Element = xml_text(xml_find_first(records, ".//field[@name='Element']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Unit = xml_text(xml_find_first(records, ".//field[@name='Unit']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
Coconuts<-subset(Coconuts,Unit=="index")
xml_data <- read_xml("Crops.xml")

records <- xml_find_all(xml_data, ".//record")

Crops <- purrr::map_df(records, function(x) {
    data <- xml_find_all(x, ".//field")
    tibble::tibble(
        CountryOrArea = xml_text(xml_find_first(records, ".//field[@name='Country or Area']")),
        Element = xml_text(xml_find_first(records, ".//field[@name='Element']")),
        Year = xml_text(xml_find_first(records, ".//field[@name='Year']")),
        Unit = xml_text(xml_find_first(records, ".//field[@name='Unit']")),
        Value = as.numeric(xml_text(xml_find_first(records, ".//field[@name='Value']"))),
        ValueFootnotes = xml_text(xml_find_first(records, ".//field[@name='Value Footnotes']"))
    )
})
Chicken$Source <- 'Annual Amonts of Chicken'
Pigs$Source <- 'Annual Amonts of Pigs'
Coconuts$Source <- 'Annual Amonts of Coconuts'
Crops$Source <- 'Annual Amonts of Crops'

combined_df_meat <- bind_rows(Chicken,Pigs)

combined_df_meat$Year <- as.factor(combined_df_meat$Year)

Meatplot<-ggplot(combined_df_meat, aes(x = as.numeric(Year), y = Value, group = Source, color = Source)) +
    geom_line() +  
    geom_point() + 
    scale_x_continuous(breaks = seq(min(as.numeric(combined_df_meat$Year)), max(as.numeric(combined_df_meat$Year)), by = 20)) +
    theme_minimal() +  
    labs(title = "Amonts of Meat by Year from Multiple Datasets", x = "Year", y = "Value") +
    scale_color_manual(values = c('Annual Amonts of Chicken' = 'red', 'Annual Amonts of Pigs' = 'blue'))
combined_df_plant <- bind_rows(Coconuts,Crops)

combined_df_plant$Year <- as.factor(combined_df_plant$Year)

Plantplot<-ggplot(combined_df_plant, aes(x = as.numeric(Year), y = Value, group = Source, color = Source)) +
    geom_line() +  
    geom_point() + 
    scale_x_continuous(breaks = seq(min(as.numeric(combined_df_plant$Year)), max(as.numeric(combined_df_plant$Year)), by = 20)) +
    theme_minimal() +  
    labs(title = "Amonts of Plants by Year from Multiple Datasets", x = "Year", y = "Value") +
    scale_color_manual(values = c('Annual Amonts of Coconuts' = 'red', 'Annual Amonts of Crops' = 'blue'))




library(shiny)

# Define server logic required to draw a histogram
function(input, output) {
    output$selected_content <- renderUI({
        if(input$category == "General Description") {
            tagList(
                h4("General Description"),
                textOutput("description"),
                tabsetPanel(
                    tabPanel("World Map", plotOutput("worldMap")),
                    tabPanel("Interactive Map", leafletOutput("nauruLeafletMap", width = "100%", height = "600px"),HTML("<div style='padding-top: 10px;'>This is the detailed map that you can interact with by zoom in or zoom out.</div>"))
                ),
            )
        } else if (input$category == "Key Demographics") {
            tagList(
                h4("Key Demographics"),
                textOutput("Key Demographics"),
                tabsetPanel(
                    tabPanel("Population Rate", plotOutput("annualplot"),HTML("<div style='padding-top: 10px;'>The plot shows that the average annual rate of population is decreasing, which indicates that the population on this island is going to shrink until totally disappear if we don't do anything to change this situation.</div>")),
                    tabPanel("Life Expectancy", plotOutput("lifeexpectplot"),HTML("<div style='padding-top: 10px;'>The plot shows that the annual life expectancy is increasing, which reflects overall improvements in the socio-economic and health conditions of a country. It suggests that on average, people are living longer, healthier lives than in previous generations..</div>")),
                    tabPanel("Total Fertility Rate for Women",plotOutput("fertilityplot"),HTML("<div style='padding-top: 10px;'>The plot shows that the annual Total fertility rate for women is decreasing, which reflects overall improvements in the health conditions for women here. It suggests that the living condition is better.</div>"))
                ),
            )
        } else if (input$category == "Comparison") {
            tagList(
                h4("Comparison"),
                textOutput("Comparison between Nauru and Tuvalu"),
                tabsetPanel(
                    tabPanel("Population Rate", plotOutput("Comparisonannual"),HTML("<div style='padding-top: 10px;'>The plot shows that the average annual rate of population for both two places are decreasing, also they have similar fluctuation and trend.They all fluctuated a lot in last era and tend to decrease in the future. This might indicates that these two places are associated in some patterns that their annual rate of population could change simultaneously.</div>")),
                    tabPanel("Life Expectancy", plotOutput("Comparisonlife"),HTML("<div style='padding-top: 10px;'>The plot shows that the annual Life Expectancy for both two places are increasing, also they have similar trend.The only difference probably is that from around 1920 to 1970, the life expectancy of Tuvalu increase in a rapid rate while it always increase in a smooth rate for Nauru.</div>")),
                    tabPanel("Total Fertility Rate for Women", plotOutput("Comparisonfertility"),HTML("<div style='padding-top: 10px;'>The plot shows that the Total Fertility Rate for women for both two places are decreasing, also they have similar trend.</div>")),
                    tabPanel("Summary",HTML("<div style='padding-top: 10px;'>Tuvalue is an island which is close to Nauru, by collecting three same datas of both two places, we could notice that they all have same patterns and trend with each other, which might indicates that there are close ties between the two places, and both places receive similar aid and development resources.</div>"))
                ),
            )
        } else if (input$category == "SWOT") {
            tagList(
                h4("SWOT"),
                textOutput("SWOT Analysis"),
                tabsetPanel(
                    tabPanel("Weakness", plotOutput("Weaknessplot"),HTML("<div style='padding-top: 10px;'>This graph brings together four different variables:ratio of GDP, the number of carriers departure, the import and export of forest products. I choose to research these four vairables because selling of forest product is one of the main financial resources of Nauru. However, imports, exports, and carriers departure are all showing the decreasing trend which indicates that less and less commercial trade passes through the island, which also means that the island's forest resources are gradually dwindling. So I think one of the big disadvantages of this island is the increasingly scarce forest resources and commercial trade.</div>")),
                    tabPanel("Strength", plotOutput("Meatplot"), plotOutput("Plantplot"),HTML("<div style='padding-top: 10px;'>I collected various agricultural-related data separately, including the annual output of corn, coconut, pork, and chicken. I made charts based on animals and plants. The results showed that the annual output of these four major agricultural products were the same. There is an upward trend and I think this will be an advantage for the island in the future.</div>")),
                    tabPanel("Opportunities",HTML("<div style='padding-top: 10px;'>Based on various previous analyses, agricultural products may become an important factor in the development of this place. Producing more agricultural products every year will not only provide more sources of income through foreign trade, but also allow local residents to Enjoy better food treatment, which is why the local GDP is showing an upward trend.</div>")),
                    tabPanel("Threatens",HTML("<div style='padding-top: 10px;'>However, we found that the island’s forest resources have become less and less due to past trade. This will not only reduce the island’s external source of income, but also have a serious impact on the island’s ecological environment, especially when The number of external routes is showing a decreasing trend。</div>")),
                    tabPanel("Summary",HTML("<div style='padding-top: 10px;'>Strength:Growing agricultural output; Weakness:Decreasing Forest Resources; Opportuniteis:Increasing GDP and crops mean that people's lives are getting better and better, which also allows more and more labor to be invested in building islands；Threatens:Potential environmental factors, as well as reduced trade, could sound alarm bells about the island's future.</div>"))
                )
            )
        }else if (input$category == "References") {
            tagList(
                h4("References"),
                textOutput("References"),
                tabsetPanel(
                    tabPanel("Resources", HTML("<div style='padding-top: 10px;'>All datasets used in my analysis are from this website:https://data.un.org/Default.aspx </div>")),
                    tabPanel("Coding", HTML("<div style='padding-top: 10px;'>For the part of getting some characteristics (such as color and range) of ggplot, I got some help from Chatgpt. Besides this, I asked Chatgpt to fix some bugs in my code of generating the website. </div>"))
                )
            )
        }
    })
    
    output$description <- renderText({
        "Nauru,officially the Republic of Nauru (Nauruan: Repubrikin Naoero) and formerly known as Pleasant Island, is an island country and microstate in Micronesia, part of Oceania in the Central Pacific. Its nearest neighbour is Banaba of Kiribati, about 300 km (190 mi) to the east. It lies northwest of Tuvalu, 1,300 km (810 mi) northeast of Solomon Islands, east-northeast of Papua New Guinea, southeast of the Federated States of Micronesia and south of the Marshall Islands.The official languages are Nauruan and English. Nauruans are divided into twelve tribes, as represented by the twelve-pointed star on the national flag. The island was first inhabited by Micronesians around 1000 B.C., with evidence of Polynesian influence as well."
    })
    
    output$worldMap <- renderPlot({ world_map_plot })
    
    output$nauruLeafletMap <- renderLeaflet({ leaflet_nauru_map })
    output$annualplot <- renderPlot({ annualplot })
    output$lifeexpectplot <- renderPlot({ lifeexpectplot })
    output$fertilityplot <- renderPlot({ fertilityplot })
    output$Comparisonannual <- renderPlot({ Comparisonannual })
    output$Comparisonlife <- renderPlot({ Comparisonlife })
    output$Comparisonfertility <- renderPlot({ Comparisonfertility })
    output$Weaknessplot <- renderPlot({ Weaknessplot })
    output$Meatplot <- renderPlot({ Meatplot })
    output$Plantplot <- renderPlot({ Plantplot })
}

