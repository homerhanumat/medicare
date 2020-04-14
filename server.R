library(tidyverse)
#library(maptools)
#library(rgdal)
#library(rgeos)
library(sf)
#library(plotly)
#library(viridis)
library(mapview)


load("data/DiagnosticRelatedGroups.Rda")
load("data/MedicareCharges.Rda")
load("data/MedicareProviders.Rda")
load("data/states_names.Rda")
load("data/counties.Rda")

GapByCounty <- function(state, drg_num, year_num) {
  charges <-
    MedicareCharges %>% 
    filter(drg == drg_num)
  if (year_num != "All Years") {
    charges <-
      charges %>% 
      filter(year == as.numeric(year_num))
  }
  providers <-
    MedicareProviders %>% 
    filter(stateProvider == state) %>%  
    select(idProvider, ID)
  if (year_num == "All Years") {
    gap_summary <-
      charges %>% 
      mutate(totalCharges = aveCharges * totalDischarges,
             totalPayments = avePayments * totalDischarges) %>% 
      inner_join(providers, by = "idProvider") %>% 
      group_by(ID) %>% 
      summarize(charges = round(sum(totalCharges), 0),
                payments = round(sum(totalPayments), 0),
                percentCovered = round(sum(totalPayments) / sum(totalCharges) * 100, 2),
                discharges = sum(totalDischarges)) %>% 
      arrange(ID) %>% 
      ungroup()
  } else {
    gap_summary <-
      charges %>% 
      filter(year == as.numeric(year_num)) %>% 
      mutate(totalCharges = aveCharges * totalDischarges,
             totalPayments = avePayments * totalDischarges) %>% 
      inner_join(providers, by = "idProvider") %>% 
      group_by(ID) %>% 
      summarize(charges = round(sum(totalCharges), 0),
                payments = round(sum(totalPayments), 0),
                percentCovered = round(sum(totalPayments) / sum(totalCharges) * 100, 2),
                discharges = sum(totalDischarges)) %>% 
      arrange(ID) %>% 
      ungroup()
  }
  
  gap_summary
}



shinyServer(function(input, output) {
  rv <- reactiveValues(
    df = data.frame(a = character()),
    counties = NULL
  )
  
  observe({
    drg_number <- 
      DiagnosticRelatedGroups %>% 
      filter(drgDefinition == input$drg) %>% 
      pull(drg)
    rv$df <- GapByCounty(state = input$state, 
                         drg = drg_number, year_num = input$year)
    state <- states_names$name[states_names$abbr == input$state]
    ## be careful about Virginia vs. West Virginia, etc.:
    state_regex <- str_c("^", state)
    rv$counties <-
      counties %>%
      filter(grepl(state_regex, ID)) %>% 
      left_join(rv$df, by = "ID")%>% 
      rename(county = ID) %>% 
      mutate(county = str_replace(county, paste0(state_regex, ","), "")) %>% 
      mutate(tooltip_text = ifelse(
        is.na(discharges),
        paste0(county, " had no discharges."),
        paste0(county, " had ", discharges,
               " discharges.\nPercent covered was ",
               percentCovered,
               "%."
               )
        )
      )
  })
  
  output$title <- renderText({
    title <- ifelse(
      input$year == "All Years",
      "Medicaid Charge Information, by County, Over All Years:</p><br><br>",
      paste0("Medicaid Charge Information by County, in ", input$year,
             ":</p><br><br>")
    )
    paste0("<p style='margin-left: 10px;'>", title)
  })
  
  output$plot <- renderLeaflet({
    #print("got here")
    if (nrow(rv$df > 0)) {
      print("got here")
      m <-
        rv$counties %>% 
        mutate(discharges = ifelse(is.na(discharges), 0, discharges)) %>% 
        select(discharges, charges, percentCovered, county) %>% 
        rename('Total Dischages' = discharges,
               `Total Billed` = charges,
               `Percent Covered` = percentCovered,
               County = county) %>% 
        mapview(burst = TRUE, hide = TRUE, legend = TRUE)
      m@map
      # title <- ifelse(
      #   input$year == "All Years",
      #   "Percent Paid by Medicaid, by County, Over All Years",
      #   paste0("Percent Paid by Medicaid, by County, in ", input$year)
      # )
      # p <- plot_ly(rv$counties,
      #         split = ~ county,
      #         text = ~tooltip_text, 
      #         hoverinfo = "text", 
      #         hoveron = "fills",
      #         hoverlabel = list(bgcolor = "white"),
      #         color = ~ percentCovered,
      #         name = "Percenr\nCovered",
      #         alpha = 1,
      #         colors = viridis_pal(option = "D")(3),
      #         showlegend = FALSE) %>% 
      #   colorbar(title = "Percent\nCovered") %>% 
      #   layout(title = title) %>% 
      #   config(displayModeBar = FALSE)
      # p
      # p <-
      # ggplot() +
      #   geom_sf(data = rv$counties, 
      #           aes(fill = percentCovered,
      #               text = tooltip_text)
      #           ) +
      #   scale_fill_viridis_c(na.value = "white") +
      #   labs(title = title)
      # plotly::ggplotly(p, tooltip = c("text")) %>% plotly::style(hoveron = "fill")
    }
  })
  
  output$table <- DT::renderDataTable({
    if (nrow(rv$df > 0)) {
      df <- rv$df
      df %>% 
        rename(county = ID) %>% 
        mutate(county = str_replace(county, "^.*,", ""))
    }
  })
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename <- function() {
      drg_safe <- str_replace_all(input$drg, "\\W", "")
      fn <- paste("payments", input$state, drg_safe, input$year, sep = "_")
      fn2 <- paste0(fn, ".csv")
      print(fn2)
      fn2
    },
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {
      readr::write_csv(
        rv$df %>% 
          rename(county = ID) %>% 
          mutate(county = str_replace(county, "^.*,", "")),
        file)
    }
  )
})
