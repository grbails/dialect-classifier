library(shiny)
library(bslib)
library(partykit)
library(tidyverse)

##############
##### UI #####
##############

ui <- fluidPage(

      tags$style(HTML("
                  .well {
                    height:700px;
                    background-color:rgb(65 67 77 / 56%);
                    overflow-y:scroll
                  }

                  .card {
                  --bs-card-bg: rgb(65 67 77 / 56%);
                  }
                  ")),
      
      theme = bs_theme(preset = "cyborg"),
      
      titlePanel(windowTitle = "DIALECT-O-TRON",
                 img(src = "dialectotron_logo.png", width = "35%")),
      
      p('Developed by George Bailey, University of York'),
      
      sidebarLayout(
        
        # LEFT COLUMN
        
        sidebarPanel(
          width = 4,
          
          h3("Pronunciations"),
          
          selectInput(
            "book.spook",
            HTML("Do the words <em>book</em> and <em>spook</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "foot.cut",
            HTML("Do the words <em>foot</em> and <em>cut</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "for.more",
            HTML("Do the words <em>for</em> and <em>more</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "fur.bear",
            HTML("Do the words <em>fur</em> and <em>bear</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "one.gone",
            HTML("Do the words <em>one</em> and <em>gone</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "singer.finger",
            HTML("Do the words <em>singer</em> and <em>finger</em> rhyme for you?"),
            choices = c("rhyme", "don't rhyme")
          ),
          selectInput(
            "eight.ate",
            HTML("Do the words <em>eight</em> and <em>ate</em> sound the same or different for you?"),
            choices = c("same", "different")
          ),
          selectInput(
            "mute.moot",
            HTML("Do the words <em>mute</em> and <em>moot</em> sound the same or different for you?"),
            choices = c("same", "different")
          ),
          selectInput(
            "pour.poor",
            HTML("Do the words <em>pour</em> and <em>poor</em> sound the same or different for you?"),
            choices = c("same", "different")
          ),
          selectInput(
            "spa.spar",
            HTML("Do the words <em>spa</em> and <em>spar</em> sound the same or different for you?"),
            choices = c("same", "different")
          ),
          selectInput(
            "thin.fin",
            HTML("Do the words <em>thin</em> and <em>fin</em> sound the same or different for you?"),
            choices = c("same", "different")
          ),
          
          hr(),
          
          h3("Words"),
          
          img(src = "bread.png", align = "left", width = "90%"),
          checkboxGroupInput(
            "bread",
            "What word(s) do you use to refer to the kind of bread pictured above?",
            choices = c(
              "bap",
              "barm",
              "batch",
              "bun",
              "cob",
              "muffin",
              "roll",
              "teacake",
              "other"
            )
          ),
          
          checkboxGroupInput(
            "meal",
            "What word(s) do you use to refer to the evening meal?",
            choices = c(
              "dinner",
              "supper",
              "tea",
              "other"
            )
          ),
          
          img(src = "clothing.png", align = "left", width = "70%"),
          checkboxGroupInput(
            "clothing",
            "What word(s) do you use to refer to the piece of clothing pictured above?",
            choices = c(
              "pants",
              "trousers",
              "other"
            )
          ),
          
          img(src = "furniture.png", align = "left", width = "90%"),
          checkboxGroupInput(
            "furniture",
            "What word(s) do you use to refer to the item of furniture pictured above?",
            choices = c(
              "couch",
              "settee",
              "sofa",
              "other"
            )
          ),
          
          hr(),
          
          h3("Phrases"),
          
          selectInput(
            "give.it.me",
            HTML('Could you use the phrase <em>"give it me"</em>?'),
            choices = c("yes", "no")
          ),
          
          selectInput(
            "I.done.it",
            HTML('Could you use the phrase <em>"I done it"</em>?'),
            choices = c("yes", "no")
          ),
          
          selectInput(
            "things.what",
            HTML('Could you use the phrase <em>"They\'re just things what we\'ve used"</em>?'),
            choices = c("yes", "no")
          ),
          
          selectInput(
            "you.was",
            HTML('Could you use the phrase <em>"You was outside when she called"</em>?'),
            choices = c("yes", "no")
          ),
          
          hr(),
          
          actionButton(
            "submit",
            "Generate prediction!",
            class = "btn-primary"
          ),
        ),
        
        # RIGHT COLUMN
        mainPanel(
          width = 8,
          
          br(),
          
          card(
            card_body(
              htmlOutput("prediction_class"),
              br(),
              plotOutput("prediction_probs")
            )
          )
        )
      )
    )


##############
### SERVER ###
##############

server <- function(input, output, session) {

  # Load saved model
  mcr_model <- readRDS("models/Manchester-1-forest.rds")
  liv_model <- readRDS("models/Liverpool-1-forest.rds")
  lds_model <- readRDS("models/Leeds-1-forest.rds")
  shf_model <- readRDS("models/Sheffield-1-forest.rds")
  ncl_model <- readRDS("models/Newcastle-1-forest.rds")
  
  observeEvent(input$submit, {
    
    showModal(modalDialog(
      size = 'xl',
      footer = NULL,
      easyClose = T,
      tags$video(id="video2", type = "video/mp4",src = "dialectotron_vid.mp4", autoplay = TRUE, width = "100%")
    ))
    
    #validation
    #req(input$book.spook, input$foot.cut, input$you.was)
    
    # Construct new data
    new_data <- data.frame(
      book.spook = input$book.spook,
      foot.cut = input$foot.cut,
      for.more = input$for.more,
      fur.bear = input$fur.bear,
      one.gone = input$one.gone,
      eight.ate = input$eight.ate,
      singer.finger = input$singer.finger,
      mute.moot = input$mute.moot,
      pour.poor = input$pour.poor,
      spa.spar = input$spa.spar,
      thin.fin = input$thin.fin,
      
      bap = ifelse("bap" %in% input$bread, 'yes', 'no'),
      batch = ifelse("batch" %in% input$bread, 'yes', 'no'),
      barm = ifelse("barm" %in% input$bread, 'yes', 'no'),
      cob = ifelse("cob" %in% input$bread, 'yes', 'no'),
      bun = ifelse("bun" %in% input$bread, 'yes', 'no'),
      muffin = ifelse("muffin" %in% input$bread, 'yes', 'no'),
      roll = ifelse("roll" %in% input$bread, 'yes', 'no'),
      teacake = ifelse("teacake" %in% input$bread, 'yes', 'no'),
      
      supper = ifelse("supper" %in% input$meal, 'yes', 'no'),
      dinner = ifelse("dinner" %in% input$meal, 'yes', 'no'),
      tea = ifelse("tea" %in% input$meal, 'yes', 'no'),
      
      pants = ifelse("pants" %in% input$clothing, 'yes', 'no'),
      trousers = ifelse("trousers" %in% input$clothing, 'yes', 'no'),
      
      couch = ifelse("couch" %in% input$furniture, 'yes', 'no'),
      sofa = ifelse("sofa" %in% input$furniture, 'yes', 'no'),
      settee = ifelse("settee" %in% input$furniture, 'yes', 'no'),
      
      #exclude this variable because I can't be bothered asking it in the demonstration
      #but we can't have missing data so just set them all to no
      you_guys = 'no',
      you_lot = 'no',
      yous = 'no',
      
      give.it.me = input$give.it.me,
      I.done.it = input$I.done.it,
      things.what = input$things.what,
      you.was = input$you.was
    )
    
    #IMPORTANT: notice how we extract the 2nd item for the Sheffield probabilities, but the 1st item for all the others
    #this is because probabilities are returned in pairs (e.g. Manchester and NOT Manchester), which always add up to 1,
    #but in the case of Sheffield the order is NOT Sheffield first because it's alphabetical and Sheffield is the only
    #location which comes alphabetically after the word 'NOT'
    
    pred_probs <- data.frame(
      location = c('Manchester', 'Liverpool', 'Leeds', 'Sheffield', 'Newcastle'),
      probs = c(predict(mcr_model, newdata = new_data, type = "prob")[,1],
                predict(liv_model, newdata = new_data, type = "prob")[,1],
                predict(lds_model, newdata = new_data, type = "prob")[,1],
                predict(shf_model, newdata = new_data, type = "prob")[,2],
                predict(ncl_model, newdata = new_data, type = "prob")[,1])
    )
    
    #extract the most likely location (i.e. the main prediction)
    pred_class <- pred_probs %>%
      arrange(desc(probs)) %>%
      slice(1) %>%
      pull(location)
    
    #and its corresponding probability
    pred_class_prob <- pred_probs %>%
      arrange(desc(probs)) %>%
      slice(1) %>%
      pull(probs)
    
    output$prediction_class <- renderText({
      
      if (pred_class == 'Liverpool') {
        disply_pred_class = '<h3><span style="color:#fb8072">Liverpool</span></h3>'
      } else if (pred_class == 'Manchester') {
        disply_pred_class = '<h3><span style="color:#bebada">Manchester</span></h3>'
      } else if (pred_class == 'Sheffield') {
        disply_pred_class = '<h3><span style="color:#8dd3c7">Sheffield</span></h3>'
      } else if (pred_class == 'Leeds') {
        disply_pred_class = '<h3><span style="color:#ffffb3">Leeds</span></h3>'
      } else if (pred_class == 'Newcastle') {
        disply_pred_class = '<h3><span style="color:#80b1d3">Newcastle</span></h3>'
      }
      
      paste0('<span style="text-align:center"><span style="font-weight:bold">DIALECT-O-TRON</span> has thought deeply about this and thinks you are from...<br><br>',
             disply_pred_class, '<br>',
             '<span style="font-weight:bold">DIALECT-O-TRON</span> is ', round(pred_class_prob, 2)*100, '% confident about this.</span>')
      
    })
    
    output$prediction_probs <- renderPlot({
      pred_probs %>%
        ggplot(aes(location, probs, fill = location)) +
        geom_col(stat = 'identity', colour = 'black') +
        geom_label(aes(label = paste0(round(probs, 2)*100, "%")), vjust = -0.5) +
        scale_x_discrete("Location") +
        scale_y_continuous("Probability", limits = c(0, 1)) +
        scale_fill_manual(values = c('#ffffb3', '#fb8072', '#bebada', '#80b1d3', '#8dd3c7'),
                          guide = 'none') +
        theme_minimal() +
        theme(text = element_text(size = 20))
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
