shinyServer(function(input, output, session) {
  
  # for selecting input for quadrant analysis
  reg<-eventReactive(input$origin,{
    #output$default <- renderText({ paste("Selecting origin ... ",input$origin) })
    
    as.character(input$origin)
  }  )
  #for selecting input for map
  map_country_select<-eventReactive(input$map_country,{
    #output$default <- renderText({ paste("Selecting origin ... ",input$origin) })
    
    as.character(input$map_country)
  }  )
  #for selecting top countries
  top_select<-eventReactive(input$top,{
    input$top
  })
  #for selecting type
  place_type<-eventReactive(input$type,{
    output$default1 <- renderText({ paste("Selecting type ...",input$type) })
    
    input$type
  }  )
  
  #for selecting tab
  gt<-eventReactive(input$gt,{
    #output$default2 <- renderText({ paste("Selecting tab ...",input$gt) })
    
    input$gt
  }  )
  
  #for selecting countries in chord plot
  co<-eventReactive(input$chord_org,{
    #output$default2 <- renderText({ paste("Selecting tab ...",input$gt) })
    
    ProperCase(input$chord_org)
  }  )
  
  cd<-eventReactive(input$chord_dest,{
    #output$default2 <- renderText({ paste("Selecting tab ...",input$gt) })
    
    ProperCase(input$chord_dest)
  }  )
  # observe({if(place_type() != "region" || place_type() != "sub region" || place_type() != "sub-region"){
  #  output$default2 <- renderText(({"Invalid type. Say either Region or Sub-Region"}))
  #  }})
  
  # output$default1 <- renderText("Hello")
  
  
  # output$ui1 <- renderUI({
  #   switch(input$radio,
  #          
  #          "continent" = selectInput("region_1",
  #                                    "Continent 1:",
  #                                    choices = setNames(nm = unique(migrant1$origin_region))),
  #          
  #          "region" = selectInput("sub_region_1",
  #                                 "Sub region 1:",
  #                                 #Can be improved by adding region as list.
  #                                 choices = setNames(nm = unique(migrant1$origin_sub_region)))
  #   )
  # })
  
  observe({updateRadioButtons(session,inputId = "radio",selected = tolower(place_type()))})
  observe({updateSelectInput(session,inputId = "remittance_country",selected = ProperCase(reg()))})
  observe({
    if(input$gender_direction_map=="Inflow"){
      updateSelectInput(session,inputId = "incoming_region_map",selected = map_country_select())
    }
    else if(input$gender_direction_map=="Outflow")
    {
      updateSelectInput(session,inputId = "outgoing_region_map",selected = map_country_select())
    }
  })
  observeEvent(input$gt,{updateTabItems(session,inputId = "tabs",selected = tolower(gt()))})
  #observeEvent(input$tabs,{updateinpuinput$gt=" "})
  observe({updateSliderInput(session,inputId = "rank",value = top_select() )})
  
  
  # output$ui2 <- renderUI({
  #   switch(input$radio,
  #          "continent" = selectInput("region_2",
  #                                    "Continent 2:",
  #                                    choices = setNames(nm = unique(migrant1$origin_region))),
  #          
  #          "region" = selectInput("sub_region_2",
  #                                 "Sub region 2:",
  #                                 #Can be improved by adding region as list.
  #                                 choices = setNames(nm = unique(migrant1$origin_sub_region)))
  #   )
  # })
  
  output$selectize_countries <- renderUI({
    switch(input$gender_direction,
           "Inbound" = selectInput("incoming_region",
                                   "Please select continents",
                                   choices = setNames(nm = unique(migrant_gender$destination_region)),
                                   multiple = TRUE),
           
           "Outbound" = selectInput("outgoing_region",
                                    "Please select continents:",
                                    choices = setNames(nm = unique(migrant_gender$origin_region)),
                                    multiple = TRUE))
    
  })
  
  
  output$bilat_plot <- renderPlotly({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(Type == input$bilat_type) 
    
    #regr = lm(bilateral_bal$Mji ~ bilateral_bal$Mij)
    
    plot_ly(data = bilateral_bal_data,
            x = ~log(Mij,base=2),
            y = ~log(Mji,base=2),
            size = ~(Bilateral.balance),
            #frame = ~as.character(year),
            text = ~paste0(combinationname," \n",i,"->",j," - ",Mij," \n",j,"->",i," - ",Mji," \nBilateral balance - ",Bilateral.balance),
            hoverinfo = "text",
            #color = ~Region,
            #color = ~log(Mij+Mji,10),
            color = ~mig_type,
            type = 'scatter',
            mode = 'markers',
            sizes = c(10,50),
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF'))
    ) %>% layout(xaxis = x, yaxis = y) }) 
  
  output$avg_bilat_least_cross <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Cross continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[3],3), "Least developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$avg_bilat_less_cross <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Cross continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[2],3), "Less developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$avg_bilat_more_cross <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Cross continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[1],3), "More developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$avg_bilat_least_within <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Within continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[3],3), "Least developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$avg_bilat_less_within <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Within continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[2],3), "Less developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$avg_bilat_more_within <- renderValueBox({
    
    bilateral_bal_data <- bilateral_bal %>% 
      filter(year == input$bilat_year) %>% 
      filter(mig_type == "Within continent") %>% 
      group_by(Type) %>% 
      summarize(avg_bilat = mean(Bilateral.balance))
    
    valueBox(
      round(bilateral_bal_data$avg_bilat[1],3), "More developed countries", icon = icon("globe", lib = "glyphicon"),
      color = "blue"
    )
  })
  
  output$chorddiag <- renderChorddiag({
    if(input$radio=="continent"){
      #if(input$radio == "Continent"){
      
      
      migrant2 <- subset(migrant1, year==input$year)
      region_matrix <- xtabs(count~origin_region+destination_region, data = migrant2)
      
      #make it as a matrix
      un1 <<- unique(sort(c(colnames(region_matrix), rownames(region_matrix))))
      region_matrix_na <- matrix(NA, length(un1), length(un1), dimnames = list(un1, un1))
      region_matrix_na[row.names(region_matrix), colnames(region_matrix)] <- region_matrix
      #region_matrix_na
      
      chorddiag(region_matrix_na,margin = 90, 
                showTicks =FALSE
                , groupnameFontsize = 14      
                , groupnamePadding = 10
                , groupThickness = .05
                , clickAction = "Shiny.onInputChange('contsourceIndex', d.source.index+1);
                Shiny.onInputChange('conttargetIndex', d.target.index+1);")
      #, chordedgeColor = "gray90")
      
    }else if(input$radio=="region"){
      # }else{
      migrant2 <- subset(migrant1, year==input$year)
      sub_region_matrix <- xtabs(count~origin_sub_region+destination_sub_region, data = migrant2)
      
      #make it as a matrix
      un2 <- unique(sort(c(colnames(sub_region_matrix), rownames(sub_region_matrix))))
      un1 <<- un2
      sub_region_matrix_na <- matrix(NA, length(un2), length(un2), dimnames = list(un2, un2))
      sub_region_matrix_na[row.names(sub_region_matrix), colnames(sub_region_matrix)] <- sub_region_matrix
      #sub_region_matrix_na
      
      
      chorddiag(sub_region_matrix_na, margin = 90, showTicks =FALSE
                , groupnameFontsize = 14      
                , groupnamePadding = 10
                , groupThickness = .05
                , clickAction = "Shiny.onInputChange('contsourceIndex', d.source.index+1);
                Shiny.onInputChange('conttargetIndex', d.target.index+1);")
      #, chordedgeColor = "gray90")
    }
  })
  
  observeEvent(input$contsourceIndex, {
    output$chorddiag_country <- renderChorddiag({
      
      if(input$radio == "continent"){
        
        migrant2 <- subset(migrant1, year==input$year)
        #total1 <- subset(migrant2,  (origin_region == reg() || origin_region == input$from_region)&(destination_region == input$to_region))
        
        if(un1[input$contsourceIndex] == un1[input$conttargetIndex]){
          
          totala <- subset(migrant2,  (origin_region == un1[input$contsourceIndex])&(destination_region == un1[input$conttargetIndex]))
          
          total1 <- totala
          
        }else{
          
          totala <- subset(migrant2,  (origin_region == un1[input$contsourceIndex])&(destination_region == un1[input$conttargetIndex]))
          totalb <- subset(migrant2,  (origin_region == un1[input$conttargetIndex])&(destination_region == un1[input$contsourceIndex]))
          
          total1 <- rbind(totala,totalb)  
        }
        
        region_matrix1 <- xtabs(count~origin_country+destination_country, data = total1)
        
        #make it as a matrix
        un3 <<- unique(sort(c(colnames(region_matrix1), rownames(region_matrix1))))
        region_matrix1_na <- matrix(NA, length(un3), length(un3), dimnames = list(un3, un3))
        region_matrix1_na[row.names(region_matrix1), colnames(region_matrix1)] <- region_matrix1
        region_matrix1_na
        
        chorddiag(remove_diags(region_matrix1_na, FALSE), margin = 90, showTicks =FALSE
                  , groupnameFontsize = 12      
                  , groupnamePadding = 10
                  , groupThickness = .05
                  , clickAction = "Shiny.onInputChange('sourceIndex', d.source.index+1);
                  Shiny.onInputChange('targetIndex', d.target.index+1);")
        #, chordedgeColor = "gray90")
        
      }else{
        migrant2 <- subset(migrant1, year==input$year)
        
        if(un1[input$contsourceIndex] == un1[input$conttargetIndex]){
          
          totalw <- subset(migrant2,  (origin_sub_region == un1[input$contsourceIndex])&(destination_sub_region == un1[input$conttargetIndex]))
          
          total2 <- totalw
          
        }else{
          
          totalw <- subset(migrant2,  (origin_sub_region == un1[input$contsourceIndex])&(destination_sub_region == un1[input$conttargetIndex]))
          totalx <- subset(migrant2,  (origin_sub_region == un1[input$conttargetIndex])&(destination_sub_region == un1[input$contsourceIndex]))
          
          total2 <- rbind(totalw,totalx)  
        }
        
        # 
        # 
        # totalw <- subset(migrant2,  (origin_sub_region == input$sub_region_1)&(destination_sub_region == input$sub_region_1))
        # totalx <- subset(migrant2,  (origin_sub_region == input$sub_region_1)&(destination_sub_region == input$sub_region_2))
        # totaly <- subset(migrant2,  (origin_sub_region == input$sub_region_2)&(destination_sub_region == input$sub_region_1))
        # totalz <- subset(migrant2,  (origin_sub_region == input$sub_region_2)&(destination_sub_region == input$sub_region_2))
        # total2 <- rbind(totalw,totalx,totaly,totalz)
        
        sub_region_matrix1 <- xtabs(count~origin_country+destination_country, data = total2)
        
        #make it as a matrix
        un4 <- unique(sort(c(colnames(sub_region_matrix1), rownames(sub_region_matrix1))))
        un3 <<- un4
        sub_region_matrix1_na <- matrix(NA, length(un4), length(un4), dimnames = list(un4, un4))
        sub_region_matrix1_na[row.names(sub_region_matrix1), colnames(sub_region_matrix1)] <- sub_region_matrix1
        sub_region_matrix1_na
        
        chorddiag(remove_diags(sub_region_matrix1_na, FALSE), margin = 90, showTicks =FALSE
                  , groupnameFontsize = 14      
                  , groupnamePadding = 10
                  , groupThickness = .05
                  , clickAction = "Shiny.onInputChange('sourceIndex', d.source.index+1);
                  Shiny.onInputChange('targetIndex', d.target.index+1);")
        #, chordedgeColor = "gray90")
      }
      
    })})
  
  observeEvent(input$sourceIndex, {
    output$conti_flow <- renderValueBox({
      
      conti <- paste0(un3[input$sourceIndex]," <-> ",un3[input$targetIndex])
      
      valueBox(
        conti, "Selected country flow", icon = icon("globe", lib = "glyphicon"),
        color = "blue"
      )
    })})
  
  observeEvent(input$contsourceIndex, {
    output$cnty_flow <- renderValueBox({
      
      if(un1[input$contsourceIndex]=="Latin America"){
        un1[input$contsourceIndex] = "LA"
      } 
      
      if(un1[input$conttargetIndex]=="Latin America"){
        un1[input$conttargetIndex] = "LA"
      } 
      
      if(un1[input$contsourceIndex]=="Northern America"){
        un1[input$contsourceIndex] = "NA"
      }       
      
      if(un1[input$conttargetIndex]=="Northern America"){
        un1[input$conttargetIndex] = "NA"
      }
      
      cnty <- paste0(un1[input$contsourceIndex]," <-> ",un1[input$conttargetIndex])
      
      valueBox(
        cnty, "Selected region flow",icon = icon("globe", lib = "glyphicon"),
        color = "blue"
      )
    })})
  
  
  #update output for selectize input of gender plot
  output$selectize_countries_map <- renderUI({
    switch(input$gender_direction_map,
           "Inflow" = selectInput("incoming_region_map",
                                  "Please select country",
                                  choices = setNames(nm = sort(unique(migrant1$destination_country))),
                                  multiple = FALSE),
           
           "Outflow" = selectInput("outgoing_region_map",
                                   "Please select country:",
                                   choices = setNames(nm = sort(unique(migrant1$origin_country))),
                                   multiple = FALSE))
    
  })
  
  #data preparation for map
  mapdata1 <- eventReactive(input$mapButton, {
    if (input$gender_direction_map == "Inflow"){
      migrant1 %>%
        filter(`year` == input$map_year) %>%
        filter(`destination_country` == input$incoming_region_map)
      
    }else{
      migrant_input = migrant1 %>%
        filter(`year` == input$map_year) %>%
        filter(`origin_country` == input$outgoing_region_map)
    }
  })
  
  
  
  observeEvent(input$mapButton, {
    
    migrant_input = mapdata1()
    worldshapes = worldshapes_data
    
    map_data = subset(migrant_input,select = c("origin_country_code","destination_country_code","count","origin_country", "destination_country"))
    if(input$gender_direction_map == "Inflow"){
      colnames(map_data) = c("id","destination_country_code","val","idname", "destination_country")
    }
    else{
      colnames(map_data) = c("origin_country_code","id","val","origin_country", "idname")
    }
    
    map_data_mig <- map_data
    
    require(sp) 
    map_data_merge <- merge(worldshapes,map_data, by="id")
    map_data_merge$val[is.na(map_data_merge$val)] <- 0
    
    countx = map_data_merge$val[map_data_merge$val > 0]
    bins <- c(0, 
              quantile(countx, probs = 0.1,na.rm = TRUE), 
              quantile(countx, probs = 0.3,na.rm = TRUE),     
              quantile(countx, probs = 0.4,na.rm = TRUE),
              quantile(countx, probs = 0.5,na.rm = TRUE),
              quantile(countx, probs = 0.6,na.rm = TRUE),
              quantile(countx, probs = 0.7,na.rm = TRUE),
              quantile(countx, probs = 0.8,na.rm = TRUE),
              quantile(countx, probs = 0.9,na.rm = TRUE),
              quantile(countx, probs = 1,na.rm = TRUE)+1)
    
    
    pal <- colorBin("YlOrRd", domain = map_data_merge$val, bins = round(bins,0))
    
    if(input$gender_direction_map == "Inflow"){
      labels <- sprintf(
        "<strong>%s</strong><br/><strong>%.0f</strong> migrants to <strong>%s</strong>",
        map_data_merge$name, map_data_merge$val,map_data_merge$destination_country_code) %>% lapply(htmltools::HTML)}
    else{labels <- sprintf(
      "<strong>%s</strong><br/><strong>%.0f</strong> migrants from <strong>%s</strong>",
      map_data_merge$name, map_data_merge$val,map_data_merge$origin_country_code) %>% lapply(htmltools::HTML)}
    
    leafletProxy(mapId = "map") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearControls() %>%
      clearImages() 
    
    
    leafletProxy(mapId = "map",data = map_data_merge) %>%
      
      addPolygons(
        fillColor = ~pal(val),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      
      addLegend(pal = pal, values = ~"val", opacity = 0.7, title = NULL,
                position = "topright") %>% addProviderTiles('CartoDB.Positron',options = providerTileOptions(noWrap = TRUE))  %>% syncWith("maps") %>%
      setMaxBounds( lng1 = -180
                    , lat1 = -89.98155760646617
                    , lng2 = 180
                    , lat2 = 89.99346179538875 )
    
    #m1 <<-   output$map
    output$map_bar1 <- renderPlotly({
      
      top_10_mig <- map_data_mig %>%
        filter(rank(desc(val))<=input$rank) %>% 
        arrange(desc(val))
      
      top_10_mig$id = factor(top_10_mig$id, levels = unique(top_10_mig$id)[order(top_10_mig$val,decreasing = T)])
      
      plot_ly(top_10_mig, x = ~id, y = ~val, type = 'bar', text = ~idname,
              marker = list(color = 'rgb(187,87,97)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5))) %>%
        layout(title = "Top countries by migrant stock",
               xaxis = list(title = "Country"),
               yaxis = list(title = "Migrant stock")
        ) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                      modeBarButtonsToRemove = c(
                        'sendDataToCloud',
                        'toImage',
                        'autoScale2d',
                        'resetScale2d',
                        'hoverClosestCartesian',
                        'hoverCompareCartesian',
                        'pan2d',
                        'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                        'zoomOut2d'
                      )) 
    })
    
  })
  
  observeEvent(input$mapButton, {
    
    migrant_input = mapdata1()
    worldshapes = worldshapes_data
    
    map_data = subset(migrant_input,select = c("origin_country_code","destination_country_code","remittance","origin_country","destination_country"))
    if(input$gender_direction_map == "Inflow"){
      colnames(map_data) = c("id","destination_country_code","val","idname", "destination_country")
    }
    else{
      colnames(map_data) = c("origin_country_code","id","val","origin_country", "idname")
    }
    
    map_data_remit <- map_data
    
    
    require(sp) 
    map_data_merge <- merge(worldshapes,map_data, by="id")
    map_data_merge$val[is.na(map_data_merge$val)] <- 0
    
    countx = map_data_merge$val[map_data_merge$val > 0]
    bins <- c(0, 
              quantile(countx, probs = 0.1,na.rm = TRUE), 
              quantile(countx, probs = 0.3,na.rm = TRUE),     
              quantile(countx, probs = 0.4,na.rm = TRUE),
              quantile(countx, probs = 0.5,na.rm = TRUE),
              quantile(countx, probs = 0.6,na.rm = TRUE),
              quantile(countx, probs = 0.7,na.rm = TRUE),
              quantile(countx, probs = 0.8,na.rm = TRUE),
              quantile(countx, probs = 0.9,na.rm = TRUE),
              quantile(countx, probs = 1,na.rm = TRUE)+1)
    
    
    pal <- colorBin("YlGn", domain = map_data_merge$val, bins = round(bins,0))
    
    
    
    if(input$gender_direction_map == "Inflow"){
      labels <- sprintf(
        "<strong>%s</strong><br/><strong>%.2f K</strong> USD received from <strong>%s</strong>",
        map_data_merge$name, map_data_merge$val,map_data_merge$destination_country_code) %>% lapply(htmltools::HTML)}
    else{labels <- sprintf(
      "<strong>%s</strong><br/><strong>%.2f K</strong> USD sent to <strong>%s</strong>",
      map_data_merge$name, map_data_merge$val,map_data_merge$origin_country_code) %>% lapply(htmltools::HTML)}
    
    
    leafletProxy(mapId = "map_remit") %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      clearShapes() %>%
      clearControls() %>%
      clearImages() 
    
    
    leafletProxy(mapId = "map_remit",data = map_data_merge) %>%
      
      addPolygons(
        fillColor = ~pal(val),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>% 
      
      addLegend(pal = pal, values = ~"val", opacity = 0.7, title = NULL,
                position = "bottomright") %>% addProviderTiles('CartoDB.Positron',options = providerTileOptions(noWrap = TRUE))  %>% syncWith("maps") %>%
      setMaxBounds( lng1 = -180
                    , lat1 = -89.98155760646617
                    , lng2 = 180
                    , lat2 = 89.99346179538875 )
    
    #m2 <<- output$map_remit
    
    #bar chart for remittance map
    output$map_bar2 <- renderPlotly({
      
      top_10_remit <- map_data_remit %>%
        filter(rank(desc(val))<=input$rank) %>% 
        arrange(desc(val))
      
      top_10_remit$id = factor(top_10_remit$id, levels = unique(top_10_remit$id)[order(top_10_remit$val,decreasing = T)])
      
      plot_ly(top_10_remit, x = ~id, y = ~val, type = 'bar', text = ~idname,
              marker = list(color = 'rgb(111,147,114)',
                            line = list(color = 'rgb(8,48,107)',
                                        width = 1.5)),
              transforms = list(list(type = 'sort', target = "val",order="descending") 
              )) %>%
        layout(title = "Top countries by remittance",
               xaxis = list(title = "Country"),
               yaxis = list(title = "Remittance")
        ) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                      modeBarButtonsToRemove = c(
                        'sendDataToCloud',
                        'toImage',
                        'autoScale2d',
                        'resetScale2d',
                        'hoverClosestCartesian',
                        'hoverCompareCartesian',
                        'pan2d',
                        'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                        'zoomOut2d'
                      ))   
    })
    
  })
  
  #migrant stock map  
  {output$map <- renderLeaflet(
    
    leaflet(worldshapes_data,options = leafletOptions(minZoom = 1)) %>%   
      addProviderTiles('CartoDB.Positron',options = providerTileOptions(noWrap = TRUE)) %>% addPolygons()
    
  )}
  
  #Remittance map
  {output$map_remit <- renderLeaflet(
    
    leaflet(worldshapes_data,options = leafletOptions(minZoom = 1)) %>%   
      addProviderTiles('CartoDB.Positron',options = providerTileOptions(noWrap = TRUE)) %>%  addPolygons()
    
  )}
  
  #EDA plots
  output$dest_income <- renderPlotly({
    dest_income_plot = ggplot() + 
      theme_minimal(base_family = "mono") + 
      scale_fill_manual(values=cbPalette) +
      geom_bar(aes(y = percent_income_dest, x = year, 
                   fill = forcats::fct_rev(destination_income_level)), data = migrant_income_dest_group, stat="identity") +
      geom_text(data=migrant_income_dest_group, aes(x = year, y = pos, label = paste0(round(percent_income_dest,0),"%")),
                colour="black", family = "mono", size=4) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      scale_x_continuous(breaks=seq(1990,2020,5)) +
      scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
      labs(x="Year", y="Percentage") +
      ggtitle("% migrants by current country of residence income level")
    
    ggplotly(dest_income_plot, tooltip = NULL) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                           modeBarButtonsToRemove = c(
                                                             'sendDataToCloud',
                                                             'toImage',
                                                             'autoScale2d',
                                                             'resetScale2d',
                                                             'hoverClosestCartesian',
                                                             'hoverCompareCartesian',
                                                             'pan2d',
                                                             'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                                             'zoomOut2d'
                                                           )) # %>% highlight(orig_income, on = "plotly_hover")
    
  })
  
  output$orig_income <- renderPlotly({
    
    orig_income_plot <- ggplot() + 
      theme_minimal(base_family = "mono") + 
      scale_fill_manual(values=cbPalette) +
      geom_bar(aes(y = percent_income_orig, x = year, 
                   fill = forcats::fct_rev(origin_income_level)), data = migrant_income_orig_group, stat="identity") +
      geom_text(data=migrant_income_orig_group, aes(x = year, y = pos, label = paste0(round(percent_income_orig,0),"%")),
                colour="black", family = "mono", size=4) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      scale_x_continuous(breaks=seq(1990,2020,5)) +
      scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
      labs(x="Year", y="Percentage") +
      ggtitle("% migrants by origin country of residence income level")
    
    ggplotly(orig_income_plot, tooltip = NULL) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                           modeBarButtonsToRemove = c(
                                                             'sendDataToCloud',
                                                             'toImage',
                                                             'autoScale2d',
                                                             'resetScale2d',
                                                             'hoverClosestCartesian',
                                                             'hoverCompareCartesian',
                                                             'pan2d',
                                                             'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                                             'zoomOut2d'
                                                           ))
    
  })
  
  output$dest_type <- renderPlotly({
    
    dest_type_plot <- ggplot() + 
      theme_minimal(base_family = "mono") + 
      scale_fill_manual(values=cbPalette) +
      geom_bar(aes(y = percent_type_dest, x = year, 
                   fill = forcats::fct_rev(destination_country_type)), data = migrant_type_dest_group, stat="identity") +
      geom_text(data=migrant_type_dest_group, aes(x = year, y = pos, label = paste0(round(percent_type_dest,0),"%")),
                colour="black", family = "mono", size=4) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      scale_x_continuous(breaks=seq(1990,2020,5)) +
      scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
      labs(x="Year", y="Percentage") +
      ggtitle("% migrants by current country of residence type")
    
    ggplotly(dest_type_plot, tooltip = NULL) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                         modeBarButtonsToRemove = c(
                                                           'sendDataToCloud',
                                                           'toImage',
                                                           'autoScale2d',
                                                           'resetScale2d',
                                                           'hoverClosestCartesian',
                                                           'hoverCompareCartesian',
                                                           'pan2d',
                                                           'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                                           'zoomOut2d'
                                                         ))
    
  })
  
  output$orig_type <- renderPlotly({
    
    orig_type_plot <- ggplot() + 
      theme_minimal(base_family = "mono") + 
      scale_fill_manual(values=cbPalette) +
      geom_bar(aes(y = percent_type_orig, x = year, 
                   fill = forcats::fct_rev(origin_country_type)), data = migrant_type_orig_group, stat="identity") +
      geom_text(data=migrant_type_orig_group, aes(x = year, y = pos, label = paste0(round(percent_type_orig,0),"%")),
                colour="black", family = "mono", size=4) +
      theme(legend.position="bottom", legend.direction="horizontal",
            legend.title = element_blank()) +
      scale_x_continuous(breaks=seq(1990,2020,5)) +
      scale_y_continuous(labels = dollar_format(suffix = "%", prefix = "")) +
      labs(x="Year", y="Percentage") +
      ggtitle("% migrants by origin country of residence type")
    
    ggplotly(orig_type_plot, tooltip = NULL) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                         modeBarButtonsToRemove = c(
                                                           'sendDataToCloud',
                                                           'toImage',
                                                           'autoScale2d',
                                                           'resetScale2d',
                                                           'hoverClosestCartesian',
                                                           'hoverCompareCartesian',
                                                           'pan2d',
                                                           'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                                           'zoomOut2d'
                                                         ))
    
  })
  
  output$remit_years <- renderPlotly(plot_ly(remittance_totals, x = ~year, y = ~remittance, name = 'Remittance', color = 'rgba(0, 158, 115, 1)', type = 'scatter', mode = 'lines+markers') %>% 
                                       layout(title = "Global remittance trend in Million US$",font= plotlyfont,
                                              xaxis = list(title = "Years"),
                                              yaxis = list (title = "Global remittance")) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                                                                      modeBarButtonsToRemove = c(
                                                                                                        'sendDataToCloud',
                                                                                                        'toImage',
                                                                                                        'autoScale2d',
                                                                                                        'resetScale2d',
                                                                                                        'hoverClosestCartesian',
                                                                                                        'hoverCompareCartesian',
                                                                                                        'pan2d',
                                                                                                        'zoomIn2d','zoom2d','select2d','lasso2d',
                                                                                                        'zoomOut2d'
                                                                                                      ))
  )
  
  output$remit_change <- renderPlotly({
    
    remit_change_plot <- ggplot(data = migrant_continent, aes(x = origin_region, y = pct_change, fill = posneg)) + 
      geom_bar(stat = "identity", show.legend = FALSE) + coord_flip() + labs(y="% change in received remittance", x="Continent") + scale_fill_manual(values = c("red","lightgreen")) + theme_minimal(base_family = "mono") 
    
    ggplotly(remit_change_plot, tooltip = NULL) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                            modeBarButtonsToRemove = c(
                                                              'sendDataToCloud',
                                                              'toImage',
                                                              'autoScale2d',
                                                              'resetScale2d',
                                                              'hoverClosestCartesian',
                                                              'hoverCompareCartesian',
                                                              'pan2d',
                                                              'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                                              'zoomOut2d'
                                                            ))
    
  })
  
  # output$shiny_return <- renderPrint({
  #       paste0(un1[input$contsourceIndex], " <-> ", un1[input$conttargetIndex])
  #   })
  
  output$remittance_plot <- renderPlotly({
    
    #mig<-migrant_remittance%>%
    # filter(`Origin country` %in% input$remittance_country)
    
    mig1<-data.frame(migrant_remittance %>% 
                       filter(Count!=0) %>%
                       group_by(`Origin country`,`Year`,`Destination country`) %>%
                       summarise(Count = sum(Count)))
    mig1[,"percentile"]=NA
    
    mig1 <-  ddply(mig1, .(Year, Origin.country), transform, percentile = ecdf(Count)(Count))
    mig_percentile<-mig1
    
    colnames(mig_percentile)<-c("Origin country","Year","Destination country","Count","Migration Percentile")
    
    migr1<-data.frame(migrant_remittance %>% 
                        filter(Remittance!=0) %>%
                        group_by(`Origin country`,`Year`,`Destination country`) %>%
                        summarise(Remittance = sum(Remittance)))
    migr1[,"rpercentile"]<-NA
    migr1 <-  ddply(migr1, .(Year, Origin.country), transform, rpercentile = ecdf(Remittance)(Remittance))
    
    mig_remit_percentile<-migr1
    
    colnames(mig_remit_percentile)<-c("Origin country","Year","Destination country","Count","Remittance Percentile")
    migrant_remittance$Year<-as.factor(migrant_remittance$Year)
    mig_percentile$Year<-as.factor(mig_percentile$Year)
    mig_remit_percentile$Year<-as.factor(mig_remit_percentile$Year)
    mig2<-left_join(mig_percentile,migrant_remittance,by=c("Origin country","Year","Destination country"))
    mig_remit_count_percentile<-left_join(mig_remit_percentile,mig2,by=c("Origin country","Year","Destination country"))
    
    mig_remit_count_percentile <-mig_remit_count_percentile %>% filter(`Migration Percentile`!='')
    mig_remit_count_percentile<<-mig_remit_count_percentile[,c(-6,-24)]
    
    
    mi<<-mig_remit_count_percentile %>% filter(`Origin country`==input$remittance_country)
    
    mi$`Migration Percentile`<-round(as.numeric((as.character(mi$`Migration Percentile`))),digits = 4)*100
    mi$`Remittance Percentile`<-round(as.numeric((as.character(mi$`Remittance Percentile`))),digits = 4)*100
    
    migrant1_plot <- migrant1
    migrant1_plot$year<-as.factor(migrant1_plot$year)
    names(mi)[names(mi) == "Destination country"] <- "destination_country"
    names(mi)[names(mi) == "Origin country"] <- "origin_country"
    names(mi)[names(mi) == "Year"] <- "year"
    mi_gdp<-left_join(mi,migrant1_plot,by=c("origin_country","year","destination_country"))
    mi_gdp<-mi_gdp %>% filter(!is.na(`origin_gdppc`) & !is.na(`destination_gdppc`))
    #names(mi_gdp)[names(mi_gdp) == "Destionation Region.x"] <- "Destination Region"
    names(mi_gdp)[names(mi_gdp) == "year"] <- "Year"
    names(mi_gdp)[names(mi_gdp) == "destination_gdppc"] <- "GDP Per Capita"
    names(mi_gdp)[names(mi_gdp) == "destination_country"] <- "Destination Country"
    
    mi_gdp$`Destination Region` <- mapvalues(mi_gdp$`Destination Region`,from=c("Latin America and the Carribean","Northern America"),to=c("LA","NA"))
    mi_gdp_bar<<-mi_gdp
    #mi_gdp$`Destination Region`<-as.factor(mi_gdp$`Destination Region`)
    #mapvalues(mi_gdp$`destination_region`,from=c("Latin America and the Carribean","Northern America"),to=c("LA","NA"))
    #mi_gdp$`Destination Region`<-as.factor(mi_gdp$`Destination Region`)
    #mi_gdp$`Destination Country Type`<-as.factor(mi_gdp$`Destination Country Type`)
    #mi_gdp$`Destination Income Level`<-as.factor(mi_gdp$`Destination Income Level`)
    
    if(input$remittance_color=="Destination Region")
    {
      
      
      remit_plot_dr<-ggplot(mi_gdp,aes(x=`Migration Percentile`,y=`Remittance Percentile`,key=`Destination Country`))+geom_point(shape=20,aes(col=`Destination Region`,size=`GDP Per Capita`),alpha=0.3)+geom_point(shape=1,aes(size=`GDP Per Capita`),color="gray15",alpha=0.1)+labs(title="Migration vs Remittance percentile",x="Migration percentile",y="Remittance percentile",col="Region",size="")+facet_wrap(~Year)+geom_hline(yintercept = 50,color="black",size=0.3,linetype="dotdash")+geom_vline(xintercept = 50,color="black",size=0.3,linetype="dotdash")+ theme(
        plot.title = element_text(color="black", size=10, face="bold"),
        axis.title.x = element_text(color="blue", size=8, face="bold",margin = margin(t = 20)),
        axis.title.y = element_text(color="blue", size=8, face="bold") ) + theme(axis.text.x=element_text(vjust=20)) + theme_bw() + scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25)) + scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25))
      #scale_color_manual(name="Region",labels=C("Africa","Asia","Europe","LA","NA","Oceania"))
      #+scale_x_continuous(limits=c(0,100))+scale_y_continuous(limits=c(0,100))
      #remit_plot_dr
      ggplotly(remit_plot_dr) %>%
        layout(dragmode = "select") %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                                modeBarButtonsToRemove = c(
                                                  'sendDataToCloud',
                                                  'toImage',
                                                  'autoScale2d',
                                                  'resetScale2d',
                                                  'hoverClosestCartesian',
                                                  'hoverCompareCartesian',
                                                  'pan2d',
                                                  'zoomIn2d',#'zoom2d','select2d','lasso2d',
                                                  'zoomOut2d'
                                                ))#%>% layout(height = 500, width = 800)
    }
    else if(input$remittance_color=="Destination Country type")
    {
      #ggplot(data=mig,aes(x=`Remittance Percentage`,y=`Migration Percentage`))+geom_point(aes(col=`Destination Country type`))+facet_grid(~Year)+theme_minimal() + xlab("Migration %") + ylab("Remittance %")
      remit_plot_dct<-ggplot(mi_gdp,aes(x=`Migration Percentile`,y=`Remittance Percentile`,key=`Destination Country`))+geom_point(shape=20,aes(col=`Destination Country type`,size=`GDP Per Capita`),alpha=0.3)+geom_point(shape=1,aes(size=`GDP Per Capita`),color="gray15",alpha=0.1)+labs(title="Migration vs Remittance percentile",x="\nMigration percentile",y="Remittance percentile",col="Country Type",size="")+facet_grid(~Year)+geom_hline(yintercept = 50,color="black",size=0.3,linetype="dotdash")+geom_vline(xintercept = 50,color="black",size=0.3,linetype="dotdash")+theme(
        plot.title = element_text(color="black", size=10, face="bold"),
        axis.title.x = element_text(color="blue", size=8, face="bold",margin = margin(t = 20)),
        axis.title.y = element_text(color="blue", size=8, face="bold") ) + theme(axis.text.x=element_text(vjust=20)) + theme_bw() + scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25)) + scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25))
      ggplotly(remit_plot_dct) %>%
        layout(dragmode = "select") %>% config(collaborate = F,cloud = F,displaylogo = FALSE,
                                               modeBarButtonsToRemove = c(
                                                 'sendDataToCloud',
                                                 'toImage',
                                                 'autoScale2d',
                                                 'resetScale2d',
                                                 'hoverClosestCartesian',
                                                 'hoverCompareCartesian',
                                                 'pan2d',
                                                 'zoomIn2d',#'zoom2d','select2d','lasso2d',
                                                 'zoomOut2d'
                                               ))#%>% layout(height = 500, width = 800)
      #remit_plot_dct
    }
    else if(input$remittance_color=="Destination Income level")
    {
      
      #ggplot(data=mig,aes(x=`Remittance Percentage`,y=`Migration Percentage`))+geom_point(aes(col=`Destination Income level`))+facet_grid(~Year)+theme_minimal() + xlab("Migration %") + ylab("Remittance %")
      remit_plot_dil<-ggplot(mi_gdp,aes(x=`Migration Percentile`,y=`Remittance Percentile`,key=`Destination Country`))+geom_point(shape=20,aes(col=`Destination Income level`,size=`GDP Per Capita`),alpha=0.3)+geom_point(shape=1,aes(size=`GDP Per Capita`),color="gray15",alpha=0.1)+labs(title="Migration vs Remittance percentile",x="\nMigration percentile",y="Remittance percentile",col="Income Level",size="")+facet_grid(~Year)+geom_hline(yintercept = 50,color="black",size=0.3,linetype="dotdash")+geom_vline(xintercept = 50,color="black",size=0.3,linetype="dotdash")+theme(
        plot.title = element_text(color="black", size=10, face="bold"),
        axis.title.x = element_text(color="blue", size=8, face="bold",margin = margin(t = 20)),
        axis.title.y = element_text(color="blue", size=8, face="bold") ) + theme(axis.text.x=element_text(vjust=20)) + theme_bw() + scale_y_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25)) + scale_x_continuous(limits = c(0, 100),breaks = seq(0, 100, by = 25))
      ggplotly(remit_plot_dil) %>%
        layout(dragmode = "select") %>% config(collaborate = F,cloud = F,displaylogo = FALSE,
                                               modeBarButtonsToRemove = c(
                                                 'sendDataToCloud',
                                                 'toImage',
                                                 'autoScale2d',
                                                 'resetScale2d',
                                                 'hoverClosestCartesian',
                                                 'hoverCompareCartesian',
                                                 'pan2d',
                                                 'zoomIn2d',#'zoom2d','select2d','lasso2d',
                                                 'zoomOut2d'
                                               ))#%>% layout(height = 500, width = 800)
      #remit_plot_dil
    }
    
  })
  
  observeEvent(event_data("plotly_selected"),{
    output$bar_quadrant<- renderPlot({
      dest_country_list<-event_data("plotly_selected")
      dest_country_list_selected<-as.character(dest_country_list[,5])
      dest_country_list_selected<-unique(dest_country_list_selected)
      #mi_gdp_bar$`Destination Country`<-as.factor(mi_gdp_bar$`Destination Country`)
      mi_gdp_bar1<-mi_gdp_bar %>% filter(gender=="Total" & flag=="Stock" & `Destination Country` %in% dest_country_list_selected)
      
      mi_gdp_bar1$'Destination Country' = factor( mi_gdp_bar1$'Destination Country', levels = unique( mi_gdp_bar1$'Destination Country'[order( mi_gdp_bar1$Year,mi_gdp_bar1$Count,decreasing = T)]))
      
      if(input$remittance_color=="Destination Region")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Count.x`))))+geom_bar(stat="identity",aes(fill=`Destination Region`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Migration Count")
      }
      else if(input$remittance_color=="Destination Country type")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Count.x`))))+geom_bar(stat="identity",aes(fill=`Destination Country type`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Migration Count")
      }
      else if(input$remittance_color=="Destination Income level")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Count.x`))))+geom_bar(stat="identity",aes(fill=`Destination Income level`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Migration Count")
      }
    })})
  
  observeEvent(event_data("plotly_selected"),{
    output$bar_quadrant_remit<- renderPlot({
      dest_country_list_remit<-event_data("plotly_selected")
      dest_country_list_selected_remit<-as.character(dest_country_list_remit[,5])
      dest_country_list_selected_remit<-unique(dest_country_list_selected_remit)
      #mi_gdp_bar$`Destination Country`<-as.factor(mi_gdp_bar$`Destination Country`)
      
      mi_gdp_bar1<-mi_gdp_bar %>% filter(gender=="Total" & flag=="Stock" & `Destination Country` %in% dest_country_list_selected_remit)
      
      mi_gdp_bar1$'Destination Country' = factor( mi_gdp_bar1$'Destination Country', levels = unique( mi_gdp_bar1$'Destination Country'[order( mi_gdp_bar1$Year,mi_gdp_bar1$Count.x,decreasing = T)]))
      
      if(input$remittance_color=="Destination Region")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Remittance`))))+geom_bar(stat="identity",aes(fill=`Destination Region`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Remittance")
      }
      else if(input$remittance_color=="Destination Country type")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Remittance`))))+geom_bar(stat="identity",aes(fill=`Destination Country type`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Remittance")
      }
      else if(input$remittance_color=="Destination Income level")
      {
        ggplot(mi_gdp_bar1,aes(x=`Destination Country`,y=as.numeric(as.character(`Remittance`))))+geom_bar(stat="identity",aes(fill=`Destination Income level`))+theme_bw()+facet_grid(~Year) + labs(x="Destination",y="Remittance")
      }
    })})
  
  
  observeEvent(input$sourceIndex,{
    output$gender_bar1 <- renderPlotly({
      
      title_plot <- paste0(un3[input$targetIndex], " -> ", un3[input$sourceIndex])
      #title_plot <- paste0(chord_dest_index(), " -> ", chord_source_index())
      
      data_gender_bar1 <- migrant_gender %>% 
        filter(`year` != 2017) %>% 
        filter(`destination_country` == un3[input$sourceIndex]) %>% 
        #filter(`destination_country` == chord_source_index()) %>% 
        filter(`origin_country` == un3[input$targetIndex])
      #filter(`origin_country` == chord_dest_index())
      
      data_gender_bar1_dup<-data_gender_bar1
      
      data_gender_bar1[data_gender_bar1$year != input$year,]$count <- 0
      data_gender_bar1_hl<- data_gender_bar1
      #data_gender_bar1_hl<- data_gender_bar1 %>% filter(year==input$year)
      
      data_gender_bar1_dup[data_gender_bar1$year ==input$year,]$count <-0
      
      data_gender_ig_current_1<-data_gender_bar1_dup
      
      #data_gender_ig_current<-data_gender_bar1 %>% filter(year!=input$year)
      
      gender_bar1_plot <- ggplot(data_gender_ig_current_1, aes(x = year, y = count, fill = gender)) +
        theme_minimal(base_family = "mono") +
        geom_bar(stat = "identity", position="dodge2", col="white", show.legend = FALSE) +
        geom_bar(data=data_gender_bar1_hl,stat = "identity", position="dodge2", colour="red", show.legend = FALSE)+
        scale_fill_manual(values=c("#999999", "#E69F00")) + 
        scale_x_continuous(breaks=seq(1990,2015,5)) +
        labs(x="Year", y="Migrant stock", title = title_plot)
      
      ggplotly(gender_bar1_plot) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                             modeBarButtonsToRemove = c(
                                               'sendDataToCloud',
                                               'toImage',
                                               'autoScale2d',
                                               'resetScale2d',
                                               'hoverClosestCartesian',
                                               'hoverCompareCartesian',
                                               'pan2d',
                                               'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                               'zoomOut2d'
                                             )) 
      
    })})
  
  observeEvent(input$sourceIndex,{
    output$gender_bar2 <- renderPlotly({
      
      title_plot <- paste0(un3[input$sourceIndex], " -> ",un3[input$targetIndex])  
      
      data_gender_bar2 <- migrant_gender %>% 
        filter(`year` != 2017) %>% 
        filter(`destination_country` == un3[input$targetIndex]) %>% 
        filter(`origin_country` == un3[input$sourceIndex])
      
      data_gender_bar2_dup<-data_gender_bar2
      
      data_gender_bar2[data_gender_bar2$year != input$year,]$count <- 0
      data_gender_bar2_hl<- data_gender_bar2
      #data_gender_bar1_hl<- data_gender_bar1 %>% filter(year==input$year)
      
      data_gender_bar2_dup[data_gender_bar2$year ==input$year,]$count <-0
      
      data_gender_ig_current_2<-data_gender_bar2_dup
      
      gender_bar2_plot <- ggplot(data_gender_ig_current_2, aes(x = year, y = count, fill = gender)) +
        theme_minimal(base_family = "mono") +
        geom_bar(stat = "identity", position="dodge2", colour="white")+
        geom_bar(data=data_gender_bar2_hl,stat = "identity", position="dodge2", colour="red", show.legend = FALSE)+
        scale_fill_manual(values=c("#999999", "#E69F00")) + 
        scale_x_continuous(breaks=seq(1990,2015,5)) +
        labs(x="Year", y="Migrant stock",fill = "Migrant gender", title = title_plot)
      
      ggplotly(gender_bar2_plot) %>%  config(collaborate = F,cloud = F,displaylogo = FALSE,
                                             modeBarButtonsToRemove = c(
                                               'sendDataToCloud',
                                               'toImage',
                                               'autoScale2d',
                                               'resetScale2d',
                                               'hoverClosestCartesian',
                                               'hoverCompareCartesian',
                                               'pan2d',
                                               'zoomIn2d','zoom2d','select2d','lasso2d','toggleSpikelines',
                                               'zoomOut2d'
                                             ))   
    })})
  
  observeEvent(input$sourceIndex,{
    output$gender_line <- renderPlotly({
      
      data_gender_line1 <- migrant1 %>% 
        filter(`year` != 2017) %>% 
        filter(`destination_country` == un3[input$targetIndex]) %>% 
        filter(`origin_country` == un3[input$sourceIndex]) %>%
        mutate(combination = paste0(un3[input$sourceIndex], " -> ", un3[input$targetIndex]))  
      
      data_gender_line2 <- migrant1 %>%   
        filter(`year` != 2017) %>% 
        filter(`destination_country` == un3[input$sourceIndex]) %>% 
        filter(`origin_country` == un3[input$targetIndex]) %>% 
        mutate(combination = paste0(un3[input$targetIndex], " -> ", un3[input$sourceIndex]))  
      
      data_gender_line3 <- rbind(data_gender_line1, data_gender_line2)
      
      plot_ly(data_gender_line3, 
              x = ~year, 
              y = ~count, 
              #  type = 'scatter', 
              #  mode = 'lines+markers', colors = c("red","blue"),
              color = ~combination) %>% 
        add_lines() %>% 
        add_markers(showlegend = FALSE) %>% 
        layout(legend = list(orientation = 'h'),
               xaxis = list(title = "",
                            gridcolor = 'rgb(248,248,248)',
                            showgrid = TRUE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside'
               ),
               yaxis = list(title = "Migrant stock",
                            gridcolor = 'rgb(248,248,248)',
                            showgrid = TRUE,
                            showticklabels = TRUE,
                            tickcolor = 'rgb(127,127,127)',
                            ticks = 'outside'
               )
        ) %>% config(collaborate = F,cloud = F,displaylogo = FALSE,
                     modeBarButtonsToRemove = c(
                       'sendDataToCloud',
                       'toImage',
                       'autoScale2d',
                       'resetScale2d',
                       'hoverClosestCartesian',
                       'hoverCompareCartesian',
                       'pan2d',
                       'zoomIn2d','zoom2d','select2d','lasso2d',
                       'zoomOut2d'
                     ))   
    })})
  
  
  
  output$bilat_corr_plot1 <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  output$bilat_corr_plot2 <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Less developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  output$bilat_corr_plot3 <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Least developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  output$morecorr <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  output$lesscorr <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Less developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  output$leastcorr <- renderPlot({
    
    bibal <- bilateral_bal %>%
      filter(Type == "Least developed") %>% 
      filter(year == input$bilat_year) %>% 
      group_by(i,j) %>%
      #summarize(COR=cor(Mij,Mji)) %>% 
      mutate(COR = Bilateral.balance) %>% 
      replace(., is.na(.), 0)
    
    bibal <- droplevels(bibal)
    bibal_matrix <- xtabs(COR~i+j, data = bibal)
    
    una <- unique(sort(c(colnames(bibal_matrix), rownames(bibal_matrix))))
    bibal_matrix_corr <- matrix(NA, length(una), length(una), dimnames = list(una, una))
    bibal_matrix_corr[row.names(bibal_matrix), colnames(bibal_matrix)] <- bibal_matrix
    bibal_matrix_corr[is.na(bibal_matrix_corr)] = 0
    bibal_matrix_corr_plot <- bibal_matrix_corr
    
    # bibal_matrix_corr_plot = bibal_matrix_corr[rowSums(bibal_matrix_corr[,-1]) != 0,]
    # 
    # i <- (colSums(bibal_matrix_corr_plot, na.rm=T) != 0)
    # 
    # bibal_matrix_corr_plot <- bibal_matrix_corr_plot[, i] 
    corrplot(abs(bibal_matrix_corr_plot), order = input$corrorder,hclust.method=input$corrclustmethod,method = input$corrmethod, col = col3(200),diag = FALSE,is.corr = F,tl.col = "black",tl.srt = 90)
    
    
  })
  
  
  })

