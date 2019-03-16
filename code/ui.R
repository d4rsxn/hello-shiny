shinyUI(dashboardPage(skin = "purple",
                    dashboardHeader(title = "Global migration flows",
                                    tags$li(a(onclick = "openTab('home')",
                                              href = NULL,
                                              icon("home"),
                                              title = "Homepage",
                                              style = "cursor: pointer;"),
                                            class = "dropdown",
                                            tags$script(HTML("
                                                             var openTab = function(tabName){
                                                             $('a', $('.sidebar')).each(function() {
                                                             if(this.getAttribute('data-value') == tabName) {
                                                             this.click()
                                                             };
                                                             });
                                                             }")))
  ),
  dashboardSidebar(sidebarMenu(id="tabs",
                               menuItem("Exploring migration", tabName = "home", icon = icon("dashboard")),
                               menuItem("Migration flows", tabName = "trend", icon = icon("dashboard")),
                               menuItem("Mapping migration", tabName = "map", icon = icon("dashboard")),
                               menuItem("Unilateral migration", tabName = "unilateral", icon = icon("dashboard")),
                               menuItem("Bilateral migration", tabName = "bilateral", icon = icon("dashboard"))
  )),
  dashboardBody(
    tags$head(includeCSS("styles.css")),
    singleton(tags$head(
      tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/2.4.0/annyang.min.js"),
      #tags$script(src="//cdnjs.cloudflare.com/ajax/libs/SpeechKITT/1.0.0/speechkitt.min.js"),
      tags$script(src="speechkitt.min.js"),
      includeScript('init.js')
    )),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(
                box(plotlyOutput("dest_income")),
                box(plotlyOutput("orig_income"))
              ),
              fluidRow(
                box(plotlyOutput("dest_type")),
                box(plotlyOutput("orig_type"))
              ),
              fluidRow(
                box(plotlyOutput("remit_years")),
                box(align = "center", title = "What happened in 2009?",plotlyOutput("remit_change"))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "trend",
              fluidRow(
                column(width = 2,
                       box(width = 200, height = 100,radioButtons("radio",
                                                                  "Select flow aggregation type:",
                                                                  choices = c("continent", "region"),
                                                                  selected = "continent"))),
                
                #        column(width = 3,box(width = 250, height = 100,selectInput("year",
                #                   "Year:",
                #                  choices = c(1990,1995,2000,2005,2010,2015,2017),
                #                  selected = 2010))),
                column(width = 2, box(width = 200, height = 100, 
                                      #setSliderColor(c("SkyBlue", "#FF4500", "", "Teal"), c(1, 2, 4)),
                                      setSliderColor(c("rgb(236,236,236)", "rgb(236,236,236)", "", "rgb(236,236,236)"), c(1, 2, 4)),
                                      sliderInput("year",
                                                  "Select a year:",
                                                  min = 1990,
                                                  max = 2015,
                                                  value = 2015, 
                                                  ticks = T,
                                                  animate = T,
                                                  step = 5,format="####",sep = ""))),
                column(3,valueBoxOutput("cnty_flow",width = 300)),
                column(5,valueBoxOutput("conti_flow",width = 600))
                #                           ,
                #column(width = 3,box(width = 250, height = 100,uiOutput("ui1"))),
                #column(width = 3,box(width = 250, height = 100,uiOutput("ui2")))
              ),
              fluidRow(box(title="Region level flows",collapsible = T,
                           column(width = 6,chorddiagOutput("chorddiag", height = 650, width = 650))),
                       box(column(width = 6,chorddiagOutput("chorddiag_country", height = 650, width = 650)),collapsible = T,title="Country level flows (Select a region flow to update)")),
              #      fluidRow(
              #  column(width = 6,verbatimTextOutput("shiny_return"))),  
              fluidRow(column(width = 2,plotlyOutput("new",height = 300)),
                       (column(width = 9, align = "center", box(title="Bidirectional migration flow trend (Select a country flow to update)",width=10,plotlyOutput("gender_line", height = 300, width = 800))))
              ),
              
              fluidRow(
                box(column(width = 6, plotlyOutput("gender_bar1",height = 300, width = 600))),
                box(column(width = 6, plotlyOutput("gender_bar2",height = 300, width = 600)))
              )
              
      ),
      tabItem(tabName = "map",
              fluidRow(
                column(width = 8, leafletOutput("map",width = 800)), 
                box(width = 4, column(width = 3, plotlyOutput("map_bar1", width = 300)))
              ),
              fluidRow(
                column(width = 8, leafletOutput("map_remit",width = 800)),
                box(width = 4, column(width = 3, plotlyOutput("map_bar2", width = 300)))
              ),
              absolutePanel(id = "controlmap", class = "panel panel-default", fixed = TRUE,
                            draggable = TRUE, top = 30, left ="auto" , right = 30, bottom = "auto",
                            width = 300, height = 430,
                            
                            h5('MAP OPTIONS'),
                            radioButtons("gender_direction_map",
                                         "Migration direction:",
                                         choices = c("Inflow", "Outflow"),
                                         selected = "Outflow"),
                            uiOutput("selectize_countries_map"),
                            selectInput("map_year", "Year:", choices = c(2010,2015,2017), multiple = FALSE, selected = T),
                            setSliderColor(c("rgb(236,236,236)", "rgb(236,236,236)", "", "rgb(236,236,236)"), c(1, 2, 4)),
                            sliderInput("rank",
                                        "Top countries # to show:",
                                        min = 5,
                                        max = 15,
                                        value = 5, 
                                        ticks = T,
                                        animate = F,
                                        step = 5,sep = ""),
                            actionButton(inputId = "mapButton", label = "Update Map"),style = "opacity: 0.65; z-index: 1000;"
              )
              
      ),
      tabItem(tabName = "unilateral",
              fluidRow(
                column(width = 3,
                       box(width = 250, height = 100,selectInput("remittance_country",
                                                                 "Select an Origin country:",
                                                                 choices = migrant_remittance$`Origin country`,
                                                                 selected = T))),
                
                column(width = 7,box(width = 350, height = 100,radioButtons("remittance_color",inline = T,
                                                                            "Color by",
                                                                            choices = c("Destination Region", "Destination Income level","Destination Country type"),
                                                                            selected = "Destination Region")))
              ),
              fluidRow(
                column(width = 12, box(title = "Migration vs Remittance quadrant analysis",width=12,collapsible = T,plotlyOutput("remittance_plot")))
              ),
              fluidRow(
                column(width = 12, box(title = "Migrant stock in absolute numbers (Select data from quadrants to update)",collapsible = T,width=12,plotOutput("bar_quadrant")))
              ),
              fluidRow(
                column(width = 12, box(title = "Remittance amounts in thousand USD (Select data from quadrants to update)",collapsible = T,width=12,plotOutput("bar_quadrant_remit")))
              )
              
      ),
      
      tabItem(tabName = "bilateral",
              fluidRow(
                column(width = 3,box(width = 200, height = 100,selectInput("bilat_year",
                                                                           "Year:",
                                                                           choices = c(1990,1995,2000,2005,2010,2015,2017),
                                                                           selected = 2010))),
                column(width = 3,box(width = 200, height = 100,selectInput("corrmethod",
                                                                           "Correlation plot method:",
                                                                           choices = c("color","circle","square","ellipse","shade","number","pie"),
                                                                           selected = "color")
                )),
                column(width = 3,box(width = 200, height = 100,selectInput("corrorder",
                                                                           "Correlation plot order by:",
                                                                           choices = c("alphabet","AOE","FPC","hclust"),
                                                                           selected = "alphabet"))
                ),
                column(width = 3,box(width = 200, height = 100,selectInput("corrclustmethod",
                                                                           "Hclustering algorithm:",
                                                                           choices = c("ward", "single", "complete", "average", "mcquitty", "median", "centroid"),
                                                                           selected = "single")
                ))
                
              ),
              fluidRow(box(title = paste0("Yearly average cross continental route bilateral balances for selected year"),
                           valueBoxOutput("avg_bilat_least_cross"),
                           valueBoxOutput("avg_bilat_less_cross"),
                           valueBoxOutput("avg_bilat_more_cross")
              ),
              box(title = paste0("Yearly average within continental route bilateral balances for selected year"),
                  valueBoxOutput("avg_bilat_least_within"),
                  valueBoxOutput("avg_bilat_less_within"),
                  valueBoxOutput("avg_bilat_more_within")
              )),
              fluidRow(
                box(width = 4,title = paste0("Bilateral balance between Least developed countries"),
                    #plotlyOutput("bilat_plot",height =600)
                    plotOutput("bilat_corr_plot3",height =400)),
                
                box(width = 4,title = paste0("Bilateral balance between Less developed countries"),
                    #plotlyOutput("bilat_plot",height =600)
                    plotOutput("bilat_corr_plot2",height =400)),
                
                box(width = 4,title = paste0("Bilateral balance between More developed countries"),
                    #plotlyOutput("bilat_plot",height =600)
                    plotOutput("bilat_corr_plot1",height =400))
              ),
              fluidRow(
                column(align="center",width = 4,actionButton("zoom1", "Zoom")),
                column(align="center",width = 4,actionButton("zoom2", "Zoom")),
                column(align="center",width = 4,actionButton("zoom3", "Zoom"))
              ),
              bsModal("leastcorrmod", "Bilateral balance between Least developed countries", "zoom1", size = "large",
                      plotOutput("leastcorr",width = 800,height = 800)),
              bsModal("lesscorrmod", "Bilateral balance between Less developed countries", "zoom2", size = "large",
                      plotOutput("lesscorr",width = 800,height = 800)),
              bsModal("morecorrmod", "Bilateral balance between More developed countries", "zoom3", size = "large",
                      plotOutput("morecorr",width = 800,height = 800))
      )
    ))))