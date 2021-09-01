library(plotly)
#library(dplyr)
library(tidyverse)
library(scales)
library(data.table)
library(RColorBrewer)



# Initialize Data ----

## filtteröity data, vastausvaihtoehtoja >1 /kysymys
filt_data <- readRDS("Dummy_tilasto_filt_yli1_vastvaihtoeht.rds")

filt_data_vuosi <- readRDS("Dummy_tilasto_vuosi_filt_yli1_vastvaihtoeht.rds")


etusivu_tilastot <- readRDS("Dummy_etusivu_tilastot.rds")
etusivu_tilastot <- etusivu_tilastot %>% as.data.table(etusivu_tilastot)

##rekisterilistaus userInputtia varten
reks <- c("",unique(filt_data$lahde))




server <- function(input, output, session) {
  
  
  observeEvent(input$rekisteri, {
    req(input$rekisteri)
    
    #observe({print(input$rekisteri)})
    
    
    # A. Rekisteri----
    # _________________________ -----
    # 1 - Data Cleaning ----
    
    # tiedot infobokseihin
    pot <- etusivu_tilastot[lahde==input$rekisteri & aika=="kaikki" & otsikko=="potilaiden_maara_rekisterissa",n]
    #pot <- etusivu_tilastot[lahde=="selka" & aika=="kaikki" & otsikko=="potilaiden_maara_rekisterissa",n]
    lom <- etusivu_tilastot[lahde==input$rekisteri & aika=="kaikki" & otsikko=="taytettyjen_lomakkeiden_maara",n]
    
    # aggregaatit etusivun taulkoihin
    clean_pot <- etusivu_tilastot %>% 
      select(!lomake) %>% 
      filter(lahde==input$rekisteri & otsikko=="potilaiden_maara_rekisterissa_vuosittain")
    
    clean_lom <- etusivu_tilastot %>% 
      select(!lomake) %>% 
      filter(lahde==input$rekisteri & otsikko=="taytettyjen_lomakkeiden_maara_vuosittain")  
    
    clean_lom_v <- etusivu_tilastot %>% 
      filter(lahde==input$rekisteri & otsikko=="eri_lomakkeiden_maara_vuosittain")
    
    
    # _________________________ ----
    
    # 2 - ValueBox Data ----   
    
    output$lomake <- renderValueBox({
      ## gsub... väli erottamaan tuhansia luvussa
      valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", lom, perl=T),
               subtitle = paste0("Täytettyjen lomakkeiden määrä"),  
               icon = icon("wpforms"), color = "aqua")
    })
    
    output$potilas <- renderValueBox({
      valueBox(gsub("(?!^)(?=(?:\\d{3})+$)", " ", pot, perl=T),
               subtitle = "Potilaiden määrä rekisterissä",  
               icon = icon("hospital-user"), color = "aqua")
    })
    
    
    # _________________________ -----
    
    # 3 - Plotting RekisteriData ----  
    
    # Render Plots ----
    # 3.A potilaat_vuosi_plot ----
    output$potilaat_vuosi_plot <- renderPlotly({
      plot_ly(clean_pot, x = ~aika, y = ~n)%>% 
        add_trace(type='bar', marker = list(color = "#4C7BAD"), 
                  hovertemplate = paste(
                    paste0('<extra></extra>Potilaita: %{y}\n')))%>%
        layout(yaxis = list(title = 'Lukumäärä'), xaxis = list(autotick = F, dtick = 1), barmode = 'stack') %>% 
        #customize modebar
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
               toImageButtonOptions = list(filename = "plotOutput.png"))
      
    })
    
    # 3.B lomakkeet_vuosi_plot ----
    output$lomakkeet_vuosi_plot <- renderPlotly({
      plot_ly(clean_lom, x = ~aika, y = ~n)%>% 
        add_trace(type='bar', marker = list(color= "#104F92"),
                  hovertemplate = paste(
                    paste0('<extra></extra>Lomakkeita: %{y}\n')))%>%
        layout(yaxis = list(title = 'Lukumäärä'), xaxis = list(autotick = F, dtick = 1), barmode = 'stack') %>% 
        #customize modebar
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
               toImageButtonOptions = list(filename = "plotOutput.png"))
      
    })
    
    # 3.C lomakkeet_vuosi_plot ----
    
    ## tarvittava määrä värejä kuvaajaan
    nb.cols <- uniqueN(clean_lom_v$lomake)
    mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
    
    
    output$eri_lomakkeet_vuosi_plot <- renderPlotly({
      plot_ly(clean_lom_v, x = ~aika, y =~n)%>% 
        add_trace(type='bar', color=~lomake, text=~lomake, colors=mycolors,
                  hovertemplate = paste(
                    paste0('<extra></extra> Lomake:%{text}\nlukumäärä:%{y}')))%>%
        layout(yaxis = list(title = 'Lukumäärä'), xaxis = list(autotick = F, dtick = 1), barmode = 'stack') %>% 
        #customize modebar
        config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
               toImageButtonOptions = list(filename = "plotOutput.png"))
    })
    
    ## Laittaa tyhjän lomakesivun, kun rekisteri vaihtuu
    output$plots <- renderUI({
      #"Ole hyvä ja valitse lomake"
    })
    ## Laittaa tyhjän vuosittaisen lomakesivun, kun rekisteri vaihtuu
    output$plots_vuosi <- renderUI({
      #"Ole hyvä ja valitse lomake"
    })
    
    # _________________________ ----
    
    # 4 - Lomakkeet Input ---- 
    # 4.A rekisteri ja lomakkeet tilastotaulukosta ---- 
    
    filt_rek <- filt_data_vuosi %>%
      filter(lahde==input$rekisteri)
    
    values = c("",unique(filt_rek$lomake))
    #observe({print(values)})
    
    # lomakkeet userInput ----
    output$lomakkeet <- renderUI({
      selectInput(
        inputId = "lomakkeet",
        #multiple = TRUE,
        label = strong("Valitse lomake:", style = "font-family: 'Verdana'; font-size 28pt"),
        choices = values,
        selected = values[1]
      )
    })
    
    
  })
  
  ## Lomakevälilehdelle otsikko, joka kertoo minkä lomake on valittuna
  output$text <- renderUI({
    str1 <- paste("Lomake: ", input$lomakkeet)
    HTML(str1)
  })
  ## Lomake vuosittain välilehdelle otsikko, joka kertoo minkä lomake on valittuna
  output$text2 <- renderUI({
    str2 <- paste("Lomake: ", input$lomakkeet)
    HTML(str2)
  })
  # _________________________ ----
  
  # 4.B. Lomakkeet yleiset plots ----
  
  ## pitää laittaa oman observeEventin alle, jotta pystyy suodattamaan 
  ## ensin rekisterin ja sitten rekisterikohtaiset lomakkeet
  observeEvent(input$lomakkeet, {
    
    ## pitää olla lomake valittuna, muuten vilahtaa virheilmoitus dashboardilla
    req(input$lomakkeet)
    
    
    
    rkst <- filt_data %>%
      select(lahde, lomake, kysymys_nimi, vastausvaihtoehto_selite, pot_n) %>%
      filter(lahde==input$rekisteri &lomake==input$lomakkeet) %>%
      group_by(kysymys_nimi)
    
    #observe({print(rkst)})
    
    ## montako plottia piirtyy dashboardiin
    n_plot = uniqueN(rkst$kysymys_nimi)
    #observe({print(n_plot)})
    
    
    #Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      plot_output_list <- lapply(1:n_plot, function(i) {
        plotname <- paste("plot", i, sep="")
        box(title = "", 
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            width = 6,
            plotlyOutput(plotname, height = 250)
        )
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    
    df <- as.data.table(rkst)
    df <- split(df, by="kysymys_nimi")
    t <- list(
      family = "verdana",
      size = 9)
    
    for (i in 1:n_plot) {
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i
        plotname <- paste("plot", my_i, sep="")
        dt <- df[[my_i]]
        #observe({print(dt)})
        
        output[[plotname]] <- renderPlotly({
          plot_ly(dt, x=~vastausvaihtoehto_selite, y=~pot_n ) %>%   
            add_trace(type = 'bar', marker = list(color = 'rgb(158,202,225)',
                                                  line = list(color = 'rgb(16,79,146)',
                                                              width = 1.0))) %>%
            #layout(title = 'kysymys', xaxis = list(title = 'Vastausvaihtoehto'), yaxis = list(title = 'Lukumäärä'))
            layout(title = paste(dt[[1,3]],"?"), font=t, margin = list(b = 50),
                   yaxis = list(title = ''), xaxis = list(title =""))%>% #, xaxis = list(title = 'Vastausvaihtoehto'))  
            #customize modebar
            config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
                   toImageButtonOptions = list(filename = "plotOutput.png"))
          
          
        })
      })
    }
    
    # _________________________ ----
    
    # 4.B. Lomakkeet vuosittain plots ----
    
    rkst_vuosi <- filt_data_vuosi %>%
      select(lahde, lomake, kysymys_nimi, vastausvaihtoehto_selite, otsikko, pot_n) %>%
      filter(lahde==input$rekisteri &lomake==input$lomakkeet) %>%
      group_by(kysymys_nimi)
    #rkst_vuosi %>% mutate_if(bit64::is.integer64, as.integer)
    
    #observe({print(rkst_vuosi)})
    
    ## montako plottia piirtyy dashboardiin
    n_plot_vuosi = uniqueN(rkst_vuosi$kysymys_nimi)
    #observe({print(n_plot)})
    
    
    #Insert the right number of plot output objects into the web page
    output$plots_vuosi <- renderUI({
      plot_output_list_vuosi <- lapply(1:n_plot_vuosi, function(i) {
        plotname_vuosi <- paste("plotti", i, sep="")
        box(title = "",
            solidHeader = FALSE,
            status = "primary",
            collapsible = TRUE,
            width = 6,
            plotlyOutput(plotname_vuosi, height = 250)
        )
      })
      
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list_vuosi)
    })
    
    
    df_vuosi <- as.data.table(rkst_vuosi)
    ##lisää hover sarakkeen
    df_vuosi[,hover:= ifelse(pot_n<5, "alle 5", as.integer(pot_n))]
    df_vuosi <- split(df_vuosi, by="kysymys_nimi")
    
    t_vuosi <- list(
      family = "verdana",
      size = 9)
    
    # color palette turkoosi, sininen, pinkki
    pal1 <- c("#EAF6F9", "#D4EDF3", "#BFE4ED", "#AADBE7", "#94D1E1", "#7FC8DB", "#69BFD5", "#54B6CF")
    #pal2 <- c("#E1E9F1", "#C3D3E4", "#A5BDD6", "#88A7C9", "#6A91BB", "#4C7BAD", "#2E65A0", "#104F92")
    #pal3 <- c("#FCE2EE", "#F8C5DC", "#F5A8CB", "#F28BBA", "#EF6EA9", "#EB5198", "#E83486", "#E51775")
    
    for (i in 1:n_plot_vuosi) {
      
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_ii <- i
        plotname_vuosi <- paste("plotti", my_ii, sep="")
        dt_vuosi <- df_vuosi[[my_ii]]
        #observe({print(dt_vuosi)})
        
        output[[plotname_vuosi]] <- renderPlotly({
          plot_ly(dt_vuosi, x = ~pot_n, y =~vastausvaihtoehto_selite)%>%
            add_trace(type='bar', orientation = 'h', color=~(as.character(otsikko)), colors=pal1,
                      marker = list(line = list(color = 'rgb(248, 248, 255)', width = 1)),
                      hoverinfo='text',
                      text=~paste0("vuosi: ", otsikko, "<br>", "lukumäärä: ", hover)) %>%
            
            layout(barmode = 'stack', title=paste(dt_vuosi[[1,3]], "?"), font=t_vuosi) %>%
            layout(#margin = list(l = 120, r = 10, t = 140, b = 80),
              xaxis = list(title = "",
                           showgrid = FALSE
              ),
              yaxis = list(title = "",
                           ticksuffix = "  "
                           #ticksuffix siirtää nimet kauemmas y-akselista
              )) %>% 
            #customize modebar
            config(modeBarButtons = list(list("toImage")), displaylogo = FALSE, 
                   toImageButtonOptions = list(filename = "plotOutput.png"))
          
          
        })
      })
    }
    
  })
  
  
  
  # _________________________ ----
  # B. - Select Inputs ----
  
  # 1 - rekisteri ----
  output$rekisteri <- renderUI({
    selectInput(
      inputId = "rekisteri", 
      label = strong("Valitse laaturekisteri:", style = "font-family: 'Verdana'; font-size 28pt"),
      choices =  reks,
      selected = reks[1]
    )
  })
  
  
  # _________________________ ---- 
  # C. - UI Sidebar Output ----
  output$sidebar <- renderUI({
    div(
      uiOutput("rekisteri"),
      uiOutput("lomakkeet")
    )
    
  })
  
  output$main1 <- renderUI({
    uiOutput("plots")
  })
  
  output$main2 <- renderUI({
    uiOutput("plots_vuosi")
  })
  
  
}



library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)
library(shinycssloaders)


# UI ----
###### HEADER ######

header <- dashboardHeader(
  title = "Laaturekisteritietopöytä",
  titleWidth = 350
  
)

###### SIDEBAR ######

sidebar <- dashboardSidebar(
  
  shinyjs::useShinyjs(),
  
  width = 350,
  br(),
  h4("Laaturekisterin ja lomakkeen valinta:", style = "padding-left:15px"),
  
  uiOutput("sidebar")

)      

###### BODY ######

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          font-family: "Verdana";
        }

        /* main sidebar */
        .skin-blue .main-sidebar {
        background-color: #3c3c3c;
        }
        
        /* infoboxes */
        .small-box p {
        font-size: 20px;
        font-family: "Verdana;
        }


      '))),
  
  tabsetPanel(type = "tabs",
              # tab Yleiset tiedot ----
              tabPanel(h3("Yleiset tiedot"),
                       h2("Kuvaajissa käytetään testidataa, ei  oikeaa dataa."),
                       fluidRow(
                         br(),
                         br(),
                         
                         ## Infoboxes ----
                         column(width = 3,
                                valueBoxOutput("lomake",
                                               width = NULL)),
                         column(width = 3,
                                valueBoxOutput("potilas",
                                               width = NULL)),
                       ),
                       
                       fluidRow(
                         box(width = 12, title = "Potilaiden määrä vuosittain rekisterissä", solidHeader = TRUE,
                             collapsible = TRUE,
                             withSpinner(plotlyOutput("potilaat_vuosi_plot", height = 300)))
                         
                       ),
                       fluidRow(
                         box(width = 12, title = "Täytettyjen lomakkeiden määrä vuosittain rekisterissä", solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("lomakkeet_vuosi_plot", height = 300))
                       ),
                       fluidRow(
                         box(width = 12, title = "Eri lomakkeiden määrä vuosittain rekisterissä", solidHeader = TRUE,
                             collapsible = TRUE,
                             plotlyOutput("eri_lomakkeet_vuosi_plot"))
                       )),
              
              # tab Lomakekoht tiedot----
              tabPanel(h3("Lomakekohtaiset tiedot"),
                       h2("Kuvaajissa käytetään testidataa, ei  oikeaa dataa."),
                       br(),
                       br(),
                       
                       fluidRow(
                         ## sivun otsikko, kertoo mikä lomake valittu
                         withSpinner(htmlOutput("text", style = "font-family: 'Verdana'; font-size 40pt;
                                                 font-weight: 900; padding-left:15px")),
                         #htmlOutput("text", style = "font-family: 'Verdana'; font-size 40pt; 
                         #font-weight: 900; padding-left:15px"),
                         br(),
                         uiOutput("main1")
                       )),
              
              # tab Lomakekoht tiedot vuosittain----
              tabPanel(h3("Lomakekohtaiset tiedot vuosittain"),
                       h2("Kuvaajissa käytetään testidataa, ei  oikeaa dataa."),
                       br(),
                       br(),
                       
                       fluidRow(
                         ## sivun otsikko, kertoo mikä lomake valittu
                         withSpinner(htmlOutput("text2", style = "font-family: 'Verdana'; font-size 40pt;
                                                 font-weight: 900; padding-left:15px")),
                         #htmlOutput("text", style = "font-family: 'Verdana'; font-size 40pt"),
                         br(),
                         uiOutput("main2")
                       ))
  )
  
  
)

ui <- dashboardPage(header, sidebar, body)

shinyApp(ui=ui, server=server)
