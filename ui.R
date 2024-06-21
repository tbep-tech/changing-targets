page_navbar(
  title = "Target evaluation",
  tags$style(HTML(
    "
    .label-left .form-group {
      display: flex;              /* Use flexbox for positioning children */
      flex-direction: row;        /* Place children on a row (default) */
      width: 100%;                /* Set width for container */
      max-width: 400px;
    }

    .label-left label {
      margin-right: 2rem;         /* Add spacing between label and slider */
      align-self: center;         /* Vertical align in center of row */
      text-align: right;
      flex-basis: 50px;          /* Target width for label */
    }

    .label-left .irs {
      flex-basis: 350px;          /* Target width for slider */
    }
    "
  )),
  sidebar = sidebar(title = NULL, width = 400,
                    
                    h6("CHLOROPHYLL TARGET"),
                    div(class = 'label-left',
                        sliderInput('otbchltrg', 'OTB', min = 0, max = 20, value = 8.5, step = 0.1, round = F),
                        sliderInput('hbchltrg', 'HB', min = 0, max = 20, value = 13.2, step = 0.1, round = F),
                        sliderInput('mtbchltrg', 'MTB', min = 0, max = 20, value = 7.4, step = 0.1, round = F),
                        sliderInput('ltbchltrg', 'LTB', min = 0, max = 20, value = 4.6, step = 0.1, round = F)
                    ),
                    
                    h6("LIGHT ATTENUATION TARGET"),
                    div(class = 'label-left',
                        sliderInput('otblatrg', 'OTB', min = 0, max = 3, value = 0.83, step = 0.01, round = F),
                        sliderInput('hblatrg', 'HB', min = 0, max = 3, value = 1.58, step = 0.01, round = F),
                        sliderInput('mtblatrg', 'MTB', min = 0, max = 3, value = 0.83, step = 0.01, round = F),
                        sliderInput('ltblatrg', 'LTB', min = 0, max = 3, value = 0.63, step = 0.01, round = F)
                    ),
                    
                    actionButton("reset_input", "Reset inputs")
                    
  ),
  nav_panel("TARGET ATTAINMENTS", 
            layout_column_wrap(
              style = css(grid_template_columns = "1fr 1fr 2fr"),
              card(
                full_screen = T,
                card_header("Target Attainment"),
                plotOutput('trgmat')
              ),
              card(
                full_screen = T,
                card_header("Target Attainment OTB sub-segments"),
                plotOutput('otbtrgmat')
              ),
              card(
                full_screen = T,
                card_header("Site Attainment by year"),
                fluidRow(selectInput('yrsel', 'Select year:', choices = seq(1975, maxyr), selected = maxyr),
                         selectInput('thrsel', 'Select indicator:', choices = list('Chlorophyll-a (ug/l)' = 'chla', 'Light attenuation (m-1)' = 'la'))),
                leafletOutput('attmap')
              )
            )
  ),
  nav_panel("ANNUAL PLOTS",
            navset_card_underline(
              nav_panel("Old Tampa Bay", plotOutput('thrplototb')),
              nav_panel("Hillsborough Bay", plotOutput('thrplothb')),
              nav_panel("Middle Tampa Bay", plotOutput('thrplotmtb')),
              nav_panel("Lower Tampa Bay", plotOutput('thrplotltb'))
            )        
  )
)
