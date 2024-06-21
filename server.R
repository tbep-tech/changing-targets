function(input, output, session) {
  
  ##
  # reactives 
  
  observeEvent(input$reset_input, {
    updateSliderInput(session, 'otbchltrg', value = 8.5)
    updateSliderInput(session, 'hbchltrg', value = 13.2)
    updateSliderInput(session, 'mtbchltrg', value = 7.4)
    updateSliderInput(session, 'ltbchltrg', value = 4.6)
    updateSliderInput(session, 'otblatrg', value = 0.83)
    updateSliderInput(session, 'hblatrg', value = 1.58)
    updateSliderInput(session, 'mtblatrg', value = 0.83)
    updateSliderInput(session, 'ltblatrg', value = 0.63)
  })
  
  # format target inputs
  trginps <- reactive({
    
    # inputs
    trginps <- tibble(
      nwchltrg = input$otbchltrg,
      nechltrg = input$otbchltrg,
      cwchltrg = input$otbchltrg,
      cechltrg = input$otbchltrg,
      swchltrg = input$otbchltrg,
      sechltrg = input$otbchltrg,
      otbchltrg = input$otbchltrg,
      hbchltrg = input$hbchltrg, 
      mtbchltrg = input$mtbchltrg, 
      ltbchltrg = input$ltbchltrg,
      nwlatrg = input$otblatrg,
      nelatrg = input$otblatrg,
      cwlatrg = input$otblatrg,
      celatrg = input$otblatrg,
      swlatrg = input$otblatrg,
      selatrg = input$otblatrg,
      otblatrg = input$otblatrg,
      hblatrg = input$hblatrg,
      mtblatrg = input$mtblatrg,
      ltblatrg = input$ltblatrg
    ) %>% 
      gather('var', 'val') %>% 
      mutate(
        bay_segment = gsub('chltrg$|latrg$', '', var),
        bay_segment = toupper(bay_segment), 
        var = gsub('^.*(chltrg$|latrg$)', '\\1', var), 
        var = gsub('trg$', '_target', var), 
        var = gsub('^chl', 'chla', var)
      ) %>% 
      spread(var, val)
    
    # se diff for targets
    trgs <- targets %>% 
      mutate(
        lase1 = la_smallex - la_target, 
        chlase1 = chla_smallex - chla_target, 
        lase2 = la_thresh - la_smallex,
        chlase2 = chla_thresh - chla_smallex
      ) %>% 
      select(bay_segment, name, lase1, chlase1, lase2, chlase2)
    
    # combine with se, estimate new thresholds
    out <- trginps %>% 
      left_join(trgs, by = 'bay_segment') %>% 
      mutate(
        chla_smallex = chla_target + chlase1, 
        chla_thresh = chla_smallex + chlase2,
        la_smallex = la_target + lase1, 
        la_thresh = la_smallex + lase2
      ) %>% 
      select(bay_segment, name, chla_target, chla_smallex, chla_thresh, la_target, la_smallex, la_thresh)
    
    return(out)
    
  })
  
  # OTB threshold plot
  thrplototb <- reactive({
    
    # inputs
    trginps <- trginps()
    
    req(trginps)
    
    p1 <- show_thrplot(epcdata, bay_segment = "OTB", thr = "chla", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    p2 <- show_thrplot(epcdata, bay_segment = "OTB", thr = "la", trgs = trginps) + 
      ggtitle(NULL) + 
      pthm
    
    p1 + p2 + plot_layout(ncol = 1)
    
  })
  
  # HB threshold plot
  thrplothb <- reactive({
    
    # inputs
    trginps <- trginps()
    
    req(trginps)
    
    p1 <- show_thrplot(epcdata, bay_segment = "HB", thr = "chla", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    p2 <- show_thrplot(epcdata, bay_segment = "HB", thr = "la", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    
    p1 + p2 + plot_layout(ncol = 1)
    
  })
  
  # MTB threshold plot
  thrplotmtb <- reactive({
    
    # inputs
    trginps <- trginps()
    
    req(trginps)
    
    p1 <- show_thrplot(epcdata, bay_segment = "MTB", thr = "chla", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    p2 <- show_thrplot(epcdata, bay_segment = "MTB", thr = "la", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    
    p1 + p2 + plot_layout(ncol = 1)
    
  })
  
  # LTB threshold plot
  thrplotltb <- reactive({
    
    # inputs
    trginps <- trginps()
    
    req(trginps)
    
    p1 <- show_thrplot(epcdata, bay_segment = "LTB", thr = "chla", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    p2 <- show_thrplot(epcdata, bay_segment = "LTB", thr = "la", trgs = trginps) + 
      ggtitle(NULL) +
      pthm
    
    p1 + p2 + plot_layout(ncol = 1)
    
  })
  
  # data to map based on yrsel, thrsel
  mapdat <- reactive({
    
    # inputs
    trginps <- trginps()
    yrsel <- input$yrsel
    thrsel <- input$thrsel
    
    req(trginps)
    
    # data to map
    toplo <- epcdata %>% 
      anlz_avedatsite %>% 
      anlz_attainsite(thr = thrsel, trg = trginps)
    
    return(toplo)
    
    
  })
  
  # color palette for target met
  palyes <- reactive({
    
    # input
    mapdat <- mapdat()
    
    dmn <- mapdat %>% 
      filter(met == 'yes') %>% 
      pull(val) %>% 
      range(na.rm = T)
    
    colorNumeric(
      palette = brewer.pal(4, 'Greens'),
      na.color = 'yellow',
      domain = dmn
    )
    
  })
  
  # color palette for target not met
  palno <- reactive({
    
    # input
    mapdat <- mapdat()
    
    dmn <- mapdat %>% 
      filter(met == 'no') %>% 
      pull(val) %>% 
      range(na.rm = T)
    
    colorNumeric(
      palette = brewer.pal(4, 'Reds'),
      na.color = 'yellow',
      domain = dmn
    )
    
  })
  
  # site attainment map
  attmap <- reactive({
    
    # inputs
    thrsel <- input$thrsel
    yrsel <- input$yrsel
    palyes <- palyes()
    palno <- palno()
    mapdat <- mapdat()
    attmaploc <- isolate(attmaploc())
    
    # filter by year to map
    toplo <- mapdat %>% 
      filter(yr %in% yrsel) %>% 
      mutate(
        cols = case_when(
          met == 'yes' ~ palyes(val),
          met == 'no' ~ palno(val)
        )
      ) %>% 
      left_join(locs, by = 'epchc_station') %>% 
      st_as_sf(coords = c('Longitude', 'Latitude'), crs = prj)
    
    # values to feed to legend  
    yesval <- toplo %>% filter(met == 'yes') %>% pull(val)
    noval <- toplo %>% filter(met == 'no') %>% pull(val)
    
    # for point scaling, original range
    scls <- dplyr::case_when(
      thrsel == 'chla' ~ c(0.59, 58), 
      thrsel == 'la' ~ c(0.38, 5.92)
    )
    
    # map with custom legends
    out <- leaflet() |> 
      addProviderTiles('CartoDB.Positron') %>% 
      addScaleBar(position = 'bottomleft') %>%
      clearMarkers() %>% 
      leafem::removeMouseCoordinates() %>%
      addCircleMarkers(
        data = toplo, 
        layerId = ~epchc_station,
        stroke = TRUE,
        color = 'black',
        fill = TRUE,
        fillColor = ~cols,
        weight = 1,
        fillOpacity = 1,
        radius=~rescale(val, from = scls, to = c(4, 15)),
        label = ~paste0('Station ', epchc_station, ' (target met: ', met, ', value ', round(val, 2), ', target ', target, ')')
      ) %>% 
      addLegend("bottomright", pal = palyes, title = "Target met", labels = thrsel, opacity = 1, values = yesval) %>% 
      addLegend("bottomright", pal = palno, title = "Target not met", labels = thrsel, opacity = 1, values = noval)
    
    if(length(attmaploc) != 0)
      out <- out %>% 
        leaflet::setView(lng = attmaploc$lng, lat = attmaploc$lat, zoom = attmaploc$zoom)
    
    return(out)
    
  })
  
  # attmap locations
  attmaploc <- reactive({
    
    if(is.null(input$attmap_center))
      return(list())
    
    list(
      zoom = input$attmap_zoom,
      lat = input$attmap_center$lat,
      lng = input$attmap_center$lng
    )
    
  })
  
  # target attainment matrix
  trgmat <- reactive({
    
    # input
    trginps <- trginps()
    
    req(trginps)
    
    out <- show_matrix(epcdata, trgs = trginps, yrrng = c(1975, maxyr), historic = F) + 
      theme(
        axis.text = element_text(size = 13)
      )
    
    return(out)
    
  })
  
  # otb target attainment matrix
  otbtrgmat <- reactive({
    
    # input
    trginps <- trginps()
    
    req(trginps)
    
    out <- show_matrixotb(epcdata, trgs = trginps, yrrng = c(1975, maxyr)) + 
      theme(
        axis.text = element_text(size = 13)
      )
    
    return(out)
    
  })
  
  ##
  # outputs 
  
  output$trgmat <- renderPlot(trgmat())
  output$otbtrgmat <- renderPlot(otbtrgmat())
  output$attmap <- renderLeaflet(attmap())
  
  output$thrplototb <- renderPlot(thrplototb())
  output$thrplothb <- renderPlot(thrplothb())
  output$thrplotmtb <- renderPlot(thrplotmtb())
  output$thrplotltb <- renderPlot(thrplotltb())
  
}
