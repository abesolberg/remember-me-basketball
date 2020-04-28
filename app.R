# Remember Me Basketball!

source('helpers.R')

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$style(type = 'text/css' , 
                       ".scoreboard {background-color:#981717;
                       width:100%;color:#EEEEEE;font-family:sans-serif;height:auto;
                       padding: 1rem;margin-top: 1rem;max-width:450px;
                       border-radius: .5rem;display:flex;flex-direction:column;
                       text-align:right;align-items:flex-end;
                       } 
                       .score{border-style:double;border-color:white;margin:auto;padding:8px;
                       font-size:30px;}
                       .qtr{border-style:solid;border-color:white;margin:auto:padding:15px;
                       font-size:18px;border-width:1px;}
                       .time{border-style:solid;border-color:white;margin:auto:padding:15px;
                       font-size:24px;border-width:1px;}")),
  setBackgroundImage(src = 'background.jpg') , 
  tags$style(".fa-info-circle{color:#F5F8FA;font-size:32px} 
             .fa-github{color:#24292e;font-size:32px}
             .fa-twitter{color:#1DA1F2;font-size:32px} 
             .fa-gamepad{color:#657786;font-size:32px}") ,
  shinyjs::inlineCSS(css) ,
  tags$head(
    tags$script(
      HTML("
           $(document).ready(function(){
           // Mark columns we want to toggle
           $('body').find('div [class=col-sm-4]').addClass('sidebarPanel');
           $('body').find('div [class=col-sm-8]').addClass('mainPanel');
           })
           
           
           Shiny.addCustomMessageHandler ('resize',function (message) {
           $('.sidebarPanel').toggle();
           $('.mainPanel').toggleClass('col-sm-8 col-sm-12');
           $(window).trigger('resize')
           });
           
           ")
      )
    ),
  absolutePanel( top = 0 , left = '50%' , width = "450px" , height = '100%' , 
                 style = 'background-color:#dfbb85;transform:translateX(-50%);' 	
                 
  ), 
  div(id = 'sidebar' , 
      absolutePanel(
        top = 20 , left = 20 , width = 300 ,style = "opacity:0.92;" ,
        wellPanel(
          sliderInput("year",
                      label = "Select Years:",
                      min = min(data$year),
                      max = max(data$year),
                      value = c(2001 , 2011) , 
                      sep = '' , 
                      step = 1) , 
          pickerInput("league", "League", choices = unique(types$league) , multiple = T , 
                      options = list(`actions-box` = T , size = 10 , 
                                     `selected-text-format` = 'count > 3'))  ,
          materialSwitch('score' , label = 'Track Score:' , value = T , inline = T , 
                         status = 'primary') ,
          div(style = 'text-align:center;' ,
              actionButton(inputId = 'flip' , label = 'Flip Card') , br() ,
              actionLink('open_opts' , 'Open Advanced Filters')) , 
          shinyjs::hidden(
            div(id = 'hidden-options' , 
                pickerInput("type", "Type", choices = NULL , multiple = T , 
                            options = list(`actions-box` = T , size = 10 , 
                                           `selected-text-format` = 'count > 3')),
                pickerInput("sid", "Set ID", choices = NULL , multiple = T , 
                            options = list(`actions-box` = T , size = 10 , 
                                           `selected-text-format` = 'count > 3'))
            )),
          br() , br() , hr()
        )
      )) ,
  absolutePanel(
    id = 'main' , style = 'text-align:center;transform:translateX(-50%);' ,
    bottom = '2%' , left = '50%' ,
    div(style = 'background-color: #dfbb85;' ,
        uiOutput("card") ,  hr() , 
        uiOutput('scoring')
    )
  ) , 
  absolutePanel(bottom = 5 , left = 10 , right = 0 , fixed = T ,
                actionButton(inputId = 'github' , 
                             label = '' , icon = icon('github') ,
                             onclick = "location.href='https://github.com/abesolberg/'" ,
                             style = 'background-color:rgba(0 , 0 ,0 ,0);
                             border-color:rgba(0,0,0,0);') ,
                actionButton(inputId = 'toggleSettings' , 
                             label = '' , icon = icon('gamepad') , #info-circle
                             style = 'background-color:rgba(0 , 0 ,0 ,0);
                             border-color:rgba(0,0,0,0);') ,
                actionButton(inputId = 'thanks' , 
                             label = '' , icon = icon('info-circle') , 
                             style = 'background-color:rgba(0 , 0 ,0 ,0);
                             border-color:rgba(0,0,0,0);') ,
                actionButton(inputId = 'twitter' , label = '' ,
                             icon = icon('twitter') , 
                             onclick = "location.href='https://twitter.com/abesolberg'" ,
                             style = 'background-color:rgba(0 , 0 ,0 ,0);
                             border-color:rgba(0,0,0,0);')
                ) , 
  absolutePanel(top = 8 , right = 25 , fixed = T ,
                tags$img(style = 'transform:rotate(23deg)' , src = 'logo.png' , width = 250))
  
                )

server <- function(input, output, session) {
  
  years <- reactive({
    filter(data , between(year , min(input$year) , max(input$year)))
  })
  observeEvent(years() , {
    choices <- unique(data$league)
    updatePickerInput(session , "league" , choices = choices , 
                      selected = 'NBA')
  })
  league <- reactive({
    filter(years(), league %in% input$league)
  })
  url <- reactive({
    c('link_front' , 'link_back')[(input$flip %% 2) + 1]
  })
  observeEvent(league() , {
    choices <- unique(league()$type)
    updatePickerInput(session, "type", choices = choices , 
                      selected = choices) 
  })
  type <- reactive({
    req(input$league)
    filter(league(), type %in% input$type)
  })
  observeEvent(type(), {
    sets <- distinct(type() , year , set , sid) %>% mutate(set = paste(year, set , sep = ': '))
    choices <- sets$sid ; names(choices) <- sets$set
    updatePickerInput(session, "sid", choices = choices , selected = choices)
  })
  observeEvent(input$open_opts , {
    shinyjs::toggle('hidden-options')
  })
  observe({
    if (is_empty(input$open_opts)) {
      text <- 'Hide Advanced Filters'
    } else if ((input$open_opts %% 2) == 0){
      text <- 'Show Advanced Filters'
    } else {
      text <- 'Hide Advanced Filters'
    }
    updateActionButton(session , 'open_opts' , label = text)
  })
  observeEvent(input$flip , {
    print(url())
  })
  card <- reactive({
    req(input$remember | input$forget)
    card <- league() %>% filter(sid %in% input$sid) %>% 
      sample_n(1)
  })
  output$card <- renderUI({
    req(input$remember | input$forget)
    url.f <- card()$link_front
    url.b <- card()$link_back
    url <- card()[url()]
    div(style = 'text-align:center;' ,
        tags$img(src = url , height = "400px"))
    
  })
  output$scoreboard <- renderUI({
    if (input$score){ r <- input$remember ; f <- input$forget } else { r <- '?' ; f <- '?' }
    
    tags$table(class = "scoreboard" , 
               style = "width:100%;text-align:center;padding:5px" ,
               tags$tr(style = 'text-align:center;' ,
                       tags$th(id = 'title' , 
                               style = 'text-align:center;font-size:2rem;letter-spacing:0.6rem' ,
                               colspan = 3 ,  
                               tags$b('Remember Me Basketball!')) , br()
               ) , 
               tags$tr(
                 tags$th(style ='width:33%;text-align:center;padding:5px' , 'Remember') , 
                 tags$th(style ='width:33%;text-align:center;padding:5px' , '') , 
                 tags$th(style ='width:33%;text-align:center;padding:5px' , "Don't Remember")
               ) , 
               tags$tr(
                 tags$th(style ='width:33%;text-align:center;padding:5px' , span(class = 'score' , r)) , 
                 tags$th(style ='width:33%;text-align:center;padding:5px;font-size:24px;' , 
                         span(class = 'time' , '02') , ":" , span(class = 'time' , '41')) , 
                 tags$th(style ='width:33%;text-align:center;padding:5px' , span(class = 'score' , f))
               ) , 
               tags$tr(
                 tags$th(rowspan = 2 , 
                         div(class = 'count' ,
                             span(id = 'BONUS' ,'BONUS') , 
                             span(id = 'POSS-R' , style = 'margin-left:.5rem;' , 'POSS') 
                         )
                 ) , 
                 tags$th(rowspan = 2 , 
                         div(style = 'text-align:center;' , 'QUARTER:' , span(class = 'qtr' ,  '4')
                         )
                 ) , 
                 tags$th(rowspan = 2 , 
                         div(class = 'count' , 
                             span(id = 'BONUS' ,'BONUS') , 
                             span(id = 'POSS-F' , style = 'margin-left:.5rem;' , 'POSS') 
                         )
                 ) 
               )
    )

    
  })
  output$controls <- renderUI({
    if(input$score) {
      div(style = 'text-align:center;' , 
          actionButton('forget' , "Don't Remember!") , 
          actionButton('remember' , 'Remember!'))
    } else (
      div(style = 'text-align:center;' , 
          actionButton('remember' , 'Remember!'))
    )
  })
  output$scoring <- renderUI({
    tags$table(#id = "inputs-table" , 
      style = "width:100%;text-align:center;padding:5px" ,
      tags$tr(style = 'text-align:center;' ,
              tags$th(style = 'text-align:center' , 
                      uiOutput('controls')
              )
      ) , 
      tags$tr(style = 'text-align:center;' ,
              tags$th(uiOutput('scoreboard'))
      ))
  })
  observeEvent(input$toggleSettings, {
    session$sendCustomMessage(type = 'resize', message = 1)
    shinyjs::toggle(id = 'sidebar')
  })
  observeEvent(input$thanks , {
    req(input$thanks)
    showModal(
      modalDialog(
        title = HTML('<center>Attaboys!</center>') ,
        size = 'm' ,
        easyClose = T , 
        footer = modalButton('Play Ball!') ,
        tags$p(
          "Thanks for stopping by Remember-Me-Basketball," ,
          "a tool for remembering some guys when you left all your old cards at your childhood home." ,
          br() , br() ,
          "This project was inspired by" ,
          tags$a(href = 'https://twitter.com/david_j_roth' , "David Roth") , 
          "and the former staff of Deadspin, and their Remembering Some Guys" ,
          tags$a(href = "https://www.youtube.com/watch?v=WDShYopQzN8" , "YouTube") ,  "series," , 
          "and by Riley and Ian at the" , 
          tags$a(href = 'https://productiveouts.com/' , "Productive Outs Podcast.") , 
          "Read their work, listen to their stuff, follow them on twitter, and support their work on Patreon," ,
          tags$a(href = 'https://www.patreon.com/ThisWeekInAtrocity' , "here") , 
          "and" , 
          tags$a(href = 'https://www.patreon.com/productiveouts' , "here.") , 
          br() , br() ,
          "All basketball cards were web-scraped from" , 
          tags$a(href = 'https://www.tcdb.com/' , "The Trading Card Database.") ,
          "Check them out online, and suport their stuff as well." ,
          br() , br() ,
          "A final thanks to my buddy Al for designing the Remember-Me-Basketball Logo," ,
          "And to all my friends who took a look, shared the link, and wanted to remember some guys." , 
          br() , br() ,
          "For any questions, I can be reached on" , 
          tags$a(href = 'https://github.com/abesolberg' , "GitHub") , 
          tags$a(href = 'https://twitter.com/abesolberg' , "Twitter") , 
          "and by" , 
          tags$a(href = 'mailto:abesolberg@gmail.com' , "email.") 
        )
      )
    )
  })
  
}

shinyApp(ui = ui, server = server)
