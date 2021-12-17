
##################
# ROBERT'S CODE  #
##################


library(sf)
library(leaflet)
library(leaflegend)
library(shiny)
library(RColorBrewer)
library(dplyr)
library(shinydashboard)
library(tidyverse)

#setwd("~/Grad School/Fall 2021/Data Viz/Project/Final Project")

census <- st_read("2010_CensusData.shp", stringsAsFactors = FALSE)  
school <- st_read("School_Boundaries.shp", stringsAsFactors = FALSE) 
school_df <- st_set_geometry(school, NULL)
city_lim <- st_read("City_Limits.shp", stringsAsFactors = FALSE)
skool <- read.csv("school_locations.csv")
high <- st_read("South_Bend_High_School_District_Boundaries.shp", stringsAsFactors = FALSE)
mid <- st_read("South_Bend_Middle_School_District_Boundaries.shp", stringsAsFactors = FALSE)
elem <- st_read("South_Bend_Elementary_School_District_Boundaries.shp", stringsAsFactors = FALSE )
school_population <- read.csv("school_population.csv")

facilities <- read.csv("Public_Facilities.csv")
facilities.spatial <- facilities %>%
  st_as_sf(coords  = c("Lon","Lat"))%>%
  st_set_crs(value = 4326)


# Rename census categories
census$"Under 5 years" <- census$SE_T008_02
census$"5 to 9 years" <-  census$SE_T008_03
census$"10 to 14 years" <-  census$SE_T008_04
census$"15 to 17 years" <-  census$SE_T008_05

# try out various ways to get an acceptable color palette function
getpal <- function(cpop,nmax){
  if (length(cpop)>1){
    # try out value from nmax down to 1
    for (n in nmax:1){
      qpct <- 0:n/n
      cpopcuts <- quantile(cpop,qpct)
      # here we test to see if all the cuts are unique
      if (length(unique(cpopcuts))==length(cpopcuts)){
        if (n==1){ 
          # The data is very very skewed.
          # using quantiles will make everything one color in this case (bug?)
          # so fall back to colorBin method
          return(colorBin("OrRd",cpop, bins=nmax))
        }
        return(colorQuantile("OrRd", cpop, probs=qpct))
      }
    }
  }
  # if all values and methods fail make everything white
  pal <- function(x) { return("white") }
}

bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)






############## END ROBERT'S CODE #####################




#################### BRADY's CODE ################


aprop <- st_read("Abandoned_Property_Parcels.shp")

city_lim <- st_read("City_Limits.shp")

aprop$Date_of_Ou2 <- as.Date(aprop$Date_of_Ou)

pal_aprop <- colorFactor(palette = 'Set2', domain = aprop$Outcome_St)

######################### END BRADY'S CODE ###############



############### MATTHEW'S CODE ##################


facilities <- read.csv("Public_Facilities.csv")
facilities_spatial <- facilities %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(value = 4326)
pal_facility <- colorFactor(palette = c("red", "green", "blue"), domain = c("POLICE STATION", "FIRE STATION", "LIBRARY"))
pal_shooting <- colorFactor(palette = 'black', domain = 'Shootings')
facilities.data <- facilities_spatial %>% st_set_geometry(NULL)


# CRIME DATA PREP WORK
data <- read.csv("Criminally_Assaulted_Shootings.csv")
colnames(data)[1] <- "Lon"
colnames(data)[2] <- "Lat"
data2 <- data%>%
  filter(USER_Year == 2021)
data2_spatial <- data2 %>%
  st_as_sf(coords = c("Lon","Lat")) %>%
  st_set_crs(value = 4326)# CITY LIMIT DATA
city_lim <- st_read("City_Limits.shp")


# the above chunk is to process the data 


############### END MATTHEW'S CODE ##################



############### BEGIN TOM'S CODE ##################


#####Code used to generate supplemented 2010 census data for the shape file
#
# library(tidycensus)
# racevars <- c(White = "P005003", 
#              Black = "P005004", 
#              Asian = "P005006", 
#              Hispanic = "P004003")
# census_api_key("insert your census key here")

# census_st_joseph_county_tracts <- get_decennial(geography = "tract", variables = racevars, 
#                                   state = "IN", county = "St. Joseph", geometry = TRUE,
#                                  summary_var = "P001001", year = 2010)     

#census_st_joseph_county_tracts <- census_st_joseph_county_tracts %>% 
#                     mutate(pct = 100 * value / summary_value



## Parks data
park.points <- read_csv("Parks_Locations_and_Features.csv")

park.spatial  <- park.points %>% #projecting the table as an sf and setting the coordinate system
  st_as_sf(coords = c("Lon","Lat")) %>% 
  st_set_crs(value = 4326) 
park.spatial$popup <- paste("<b>",park.spatial$Park_Name,"</b><br>",
                            "Type: ", park.spatial$Park_Type,"<br>",sep ="")

# Census data read
census_st_joseph_county_tracts <- st_read("st_joseph_tracts.shp")
# Setting pallete
pallet_park <- colorNumeric(
  palette = "Reds",
  domain = census_st_joseph_county_tracts$pct)

#Need to flip legend going lowest to highest, have to reverse color pallet
pallet_park_legend <- colorNumeric(
  palette = "Reds",
  domain = census_st_joseph_county_tracts$pct, reverse = TRUE)


############### END TOM'S CODE ##################

################# BEGIN UI #####################################


ui <- dashboardPage(
  
  dashboardHeader(title = "TEAM EAST 7"),
  
  dashboardSidebar(    
    
    sidebarMenu(
      
###### First tab Name 
      menuItem("School Redistricting", tabName = "redistricting", icon = icon("school")),
      
      
###### Second tab Name
      menuItem("Abandoned Properties", tabName = "widgets", icon = icon("house-damage")),
    

###### 3rd tab Name 
      menuItem("Shootings", tabName = "matthew", icon = icon("user-secret")),


###### 4th tab Name
      menuItem("Park Access", tabName = "thomas", icon = icon("tree"))
  )

    
  ),

  ## Body content
  dashboardBody(
    tabItems(
      
      
      
###### Robert's tab content
      tabItem(tabName = "redistricting",
              fluidPage(
                # Application title
                titlePanel("Location of Youth Population compared to School Districts"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    radioButtons("rb", "Choose an Age Population to Display:",
                                 choices = c(
                                   "Under 5 years", 
                                   "5 to 9 years", 
                                   "10 to 14 years", 
                                   "15 to 17 years"
                                 )
                    ),
                    
                    radioButtons("rb2", "Choose a School Level to Display:",
                                 choices = c(
                                   "Elementary School", 
                                   "Middle School", 
                                   "High School"
                                 )
                    ),# end radioButtons
                    br(),
                    h5(strong("Note:")),
                    h5("Click on the Schools or the Census Tracts in the map to view population count."),
                    br(),
                    span(
                      h6(" "),
                      h6(strong("School Population Source:")),
                      h6("https://nces.ed.gov/ccd/schoolsearch/school_list.asp?Search=1&DistrictID=1810290", style = "word-wrap: break-word;"),
                      h6(" "),
                      h6(strong("School District Boundary Source:")),
                      h6("https://data-southbend.opendata.arcgis.com/search?q=school", style = "word-wrap: break-word;"),
                      style = "color:#5DADE2")
                    
                  ),# end sidebarPanel
                  # Show a plot of the generated distribution
                  
                  mainPanel( leafletOutput(outputId = "map", height = "95vh")
                  )# end mainPanel
                )# end sidebarLayout
                
              )
              
              
      ),
      
      
      
      
      
      
####### Brady's tab content
      tabItem(tabName = "widgets", 
              
              fluidPage(
              # Application title
              titlePanel("Abandoned Properties in the South Bend Area"),
                        sidebarLayout(
                              sidebarPanel(
                                                  selectInput("rb_outcome",
                                                              "Choose Outcome of Property to Observe: ",
                                                              choices = list(
                                                                "Deconstructed",
                                                                "Demolished",
                                                                "Occupied & Not Repaired",
                                                                "Repaired",
                                                                "Repaired & Occupied",
                                                                "NA"
                                                              ),
                                                              selected = "Repaired"),
                                                  dateRangeInput(inputId = "dates",
                                                                 label = "Date range input: yyyy-mm-dd",
                                                                 start = "2013-01-01", end = "2018-12-31",
                                                                 min = "2013-01-01", max ="2018-12-31"),
                                                ),
                                                mainPanel(
                                                  leafletOutput(outputId = "map_aprop", height = "95vh")
                                                )
                                              )
                                     )
              
              
              
              
                      
              
              
              
      
              ),

####### Matthew's tab content
      tabItem(tabName = "matthew", 
        
              fluidPage(
                
                # Application title
                titlePanel("Location of Public Facilities compared to Location of Shootings"),
                
                # Sidebar with a drop down panel for selecting type of facility
                sidebarLayout(
                  sidebarPanel(
                    #selectInput(inputId = "type",label = "Choose Facility Type",choices = facilities_spatial$POPL_TYPE)
                    selectizeInput(inputId = "type",label = "Choose Facility Type",choices = facilities_spatial$POPL_TYPE, multiple = T, selected = c("FIRE STATION", "POLICE STATION", "LIBRARY"), size = 3),
                    br(),
                    h5(strong("Note:")),
                    h5("Black Dots represent the locations of criminal shootings that occured in 2021"),
                    br(),
                    ),
                  
                  # show leaflet map
                  mainPanel(
                    #tabsetPanel(
                      #tabPanel(title = "Map1",
                               leafletOutput(outputId = "map1", height = "95vh")
                      #)
                    #)#end tabset
                  )
                )
              )
              
              
              
              
        
            ),

####### Thomas' tab content
        tabItem(tabName = "thomas", 
        
                fluidPage(
                  titlePanel("Proximity to Parks by Racial Demographics"),
                  sidebarLayout(
                    sidebarPanel(
                  selectInput("race", "Race", choices = c("Black", "Hispanic", "White", "Asian")),
                  selectInput("park", "Park Type", choices = c("Neighborhood Park", "Zoo", "Community Park", "Special", "Memorial", "Block Park", "Cemetery", "Golf Course"))),
                  mainPanel(
                    leafletOutput("mymap", height = "95vh")
                    )
                )
            )         
        )


    ) # end tabItems
  ) #end dashboardbody
) # end dashboardPage






  
  
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
###### Robert's Tab shtuff
  
  color.choice <- reactive({input$rb})
  
  school.choice <- reactive({input$rb2})
  
  output$map <- renderLeaflet({
    
    #Set color based on what was selected
    if(color.choice()== "Under 5 years"){color_select <- census$`Under 5 years`}
    if(color.choice()== "5 to 9 years"){color_select <- census$`5 to 9 years`}
    if(color.choice()== "10 to 14 years"){color_select <- census$`10 to 14 years`}
    if(color.choice()== "15 to 17 years"){color_select <- census$`15 to 17 years`}
   
    #Set school boundaries based on selection
    if(school.choice()== "Elementary School"){school_select <- elem}
    if(school.choice()== "Middle School"){school_select <- mid}
    if(school.choice()== "High School"){school_select <- high}
    
    #Set boundary color
    if(school.choice()== "Elementary School"){boundary_select <- "#6600CC"}
    if(school.choice()== "Middle School"){boundary_select <- "#006666"}
    if(school.choice()== "High School"){boundary_select <- "#0000FF"}
    
    pal1 <- getpal(color_select, 7)
    
    #skool_select <- skool[skool$Type == school.choice()]
    
    schoolPal <- colorFactor(c("#6600CC", "#0000FF", "#006666"), domain = skool$Type)

    numPal <- colorNumeric("OrRd", 0:600)
    
    leaflet() %>%
      addTiles()  %>%
      setView(lng=-86.25292, lat=41.68598, zoom=12) %>% 
      addLegendNumeric(
        pal = numPal, 
        title = 'Population',
        shape = 'stadium',
        values = 0:600, 
        fillOpacity = .5,
        decreasing = TRUE,
        position = 'bottomleft') %>% 
      addLegendFactor(
        pal = schoolPal,
        values = skool$Type,
        title = "School Level",
        shape = "circle",
        orientation = "vertical",
        width = 8,
        height = 8,
        position = 'bottomright') %>% 
      addPolygons(data = school_select$geometry, color = boundary_select, fillOpacity = 0) %>%
      addPolygons(data = census, fillColor = numPal(color_select), fillOpacity = 0.4, color="white", weight=.5, dashArray = "2",popup = paste("Population:",color_select)) %>%
      addCircleMarkers(data = skool[skool$Type == school.choice(),] , fillColor = ~schoolPal(school.choice()), color = ~schoolPal(school.choice()), radius =3, stroke = 1, weight=5, popup = paste("School Name:",skool$SchoolName[skool$Type == school.choice()],  "<br>","Total Students:", school_population$Population[school_population$Type == school.choice()])) %>% 

      addMeasure()
  })
  
  output$value <- renderText({ input$source1 })
  
  
  
  
  
  
  
  
  
  
  
  
########  Brady's Tab Shtuff
  
  apropFilter <- reactive({
    print(input)
    filter(aprop, between(Date_of_Ou2 ,input$dates[1], input$dates[2]) & Outcome_St == input$rb_outcome)
  })
  
  output$map_aprop <- renderLeaflet({
    
    leaflet(aprop) %>%
      addTiles() %>%
      addPolygons(data = city_lim$geometry, color="black", fillOpacity = 0) %>%
      addLegend(pal = pal_aprop, values = aprop$Outcome_St, opacity = 1) %>%
      addMeasure()
    
  })
  
  observe({
    leafletProxy("map_aprop", data = apropFilter()) %>%
      clearShapes() %>%
      addPolygons(data = city_lim$geometry, color="black", fillOpacity = 0) %>%
      addPolygons(popup = ~paste("<b>",Outcome_St,"</b><br>",
                                 "Structures: ",Structures,"<br>",
                                 "Date of Outcome: ",Date_of_Ou2,sep =""),
                  color = ~pal_aprop(Outcome_St))
  })
  
  
  
  
  
  
  
  
##### Matthew's Tab Shtuff
  
  facilities.subset <- reactive({
    facilities %>% filter(POPL_TYPE == input$type) %>% select(Lat, Lon) %>%
      st_as_sf(coords = c("Lon","Lat")) %>%
      st_set_crs(value = 4326)
  })
  
  output$map1 <- renderLeaflet({
    leaflet()%>%
      addTiles()%>%
      addLegendFactor(
        pal = pal_facility,
        values = c("POLICE STATION", "FIRE STATION", "LIBRARY"),
        title = "Facility Type",
        shape = "circle",
        orientation = "vertical",
        width = 8,
        height = 8,
        position = 'bottomright') %>% 
      addLegendFactor(
        values = '',
        pal = pal_shooting,
        title = "Shootings",
        shape = "circle",
        orientation = "vertical",
        width = 5,
        height = 5,
        position = 'bottomleft') %>% 
      addCircleMarkers(data =facilities.subset(), color = ~pal_facility(input$type))%>%
      addCircleMarkers(data = data2_spatial, color = "black", radius = 1, fillOpacity = .3)%>%
      addPolygons(data = city_lim$geometry, color="black", fillOpacity = 0) %>%
      addMeasure()
  })
  



##### Thomas' Tab Shtuff

  filtered_demographic <- reactive({filter(census_st_joseph_county_tracts, variabl == input$race)})
  filtered_park <- reactive({filter(park.spatial, Park_Type == input$park)})
  
  output$mymap <- renderLeaflet({
      leaflet(city_lim)%>%
        setView(lng=-86.25292, lat=41.68598, zoom=13) %>%
        addTiles() %>%
        addLegend(
        pal = pallet_park_legend,
        values = ~census_st_joseph_county_tracts$pct,
        title = "Percent of Population",
        labFormat = labelFormat(prefix = "%", transform = function(x) sort(x, decreasing = TRUE)),
        opacity = 1,
        position = 'topright') %>%
        addPolygons(data = filtered_demographic(), stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
                    color = ~pallet_park(pct)) %>%
        addPolygons(data = city_lim$geometry, color="black", stroke = TRUE, fillOpacity = 0, opacity = 1) %>%
        addMarkers(data = filtered_park(), popup = filtered_park()$popup)
})


  
}

  # Run the application 
shinyApp(ui = ui, server = server, options = list(height = 1080))




