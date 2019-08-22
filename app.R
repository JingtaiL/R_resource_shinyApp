library(shiny)
library(tidyverse)
library(DT)
library(extrafont)
library(shinythemes)
library(fontawesome)
library(rdrop2)


#---------------- create a self-filled form --------------------
# which fields get saved 
fieldsAll <- c("name", "title", "author", "link", "description", "keyword")
# which fields are mandatory
fieldsMandatory <- c("name", "title","author","link")
# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time: will be converted to readable time in later script
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# directory where responses get stored
responsesDir <- "responses"


saveData <- function(data) {
  fileName <- sprintf("%s_%s.csv",
                      humanTime(),
                      digest::digest(data))
  
  filePath <- file.path(responsesDir, fileName)
  write.csv(x = data, file = filePath,
            row.names = FALSE, quote = TRUE)
  drop_upload(filePath, path = responsesDir)
}

# load all responses into a data.frame
loadData <- function(){
  filesInfo <- drop_dir(responsesDir)
  filePaths <- filesInfo$path_display
  data <- lapply(filePaths,drop_read_csv,stringsAsFactors = FALSE)
  data <- do.call(rbind, data)
  data
}

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "
#----------------------------------------------------------
# Create string for hyperlinks
create_table <- function(data){
  list_table <- data %>% mutate(title_link =  paste0("<a href='",link,"' target='_blank'>",title,"</a>")) %>%
    select(title_link, everything(), -title, -link)
  return(list_table)
}

vis_color_table <- create_table(read_csv("visualization-color.csv"))
vis_theme_table <- create_table(read_csv("visualization-theme.csv"))
vis_others_table <- create_table(read_csv("visualization-others.csv"))
stats_desc_table <- create_table(read_csv("statistics-descriptive.csv"))
stats_infer_table <- create_table(read_csv("statistics-inferential.csv"))
resource_comp_table <- create_table(read_csv("resources-compilation.csv"))
resource_workshop_table <- create_table(read_csv("resources-workshop.csv"))
book_table <- create_table(read_csv("books.csv"))
wrangling_table <- create_table(read_csv("wrangling.csv"))


# ----------------- UI -------------------

ui <- fluidPage(
  # define theme ####
  #shinythemes::themeSelector(),
  theme = shinytheme("united"),
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
  # formatting with css
  includeCSS("styles1.css"),
  # Application title
  titlePanel("what R you doing?",
             windowTitle = "LearningR"),
  
  sidebarLayout(
    # for sidebar
    sidebarPanel(width = 3,
                 h3("Introduction"),
                 p("A personal collection of free R learning resources for quick reference. This website will be updated frequently."), 
                 br(),
                 div(p("Feel free to add your own resource to this website, you can check and download your and others'
        contributions by clicking the",span("Contribution",style = "color:red"),"panel.")),
                 hr(),
                 div(
                   id = "form",
                   textInput("name", labelMandatory("Your name"), placeholder = "Enter your name..."),
                   textInput("title", labelMandatory("Title"),placeholder = "Resource title..."),
                   textInput("author", labelMandatory("Author(s)")),
                   textInput("link", labelMandatory("Link"),placeholder = "Paste the link..."),
                   textInput("description", "Description",placeholder = "Brief description..."),
                   textInput("keyword", "Keywords"),
                   actionButton("submit", "Submit", class = "btn-primary"),
                   
                   shinyjs::hidden(
                     span(id = "submit_msg", "Submitting..."),
                     div(id = "error",
                         div(br(), tags$b("Error: "), span(id = "error_msg"))
                     ))
                 ),
                 shinyjs::hidden(
                   div(
                     id = "thankyou_msg",
                     h2("Thank you for your contribution!"),
                     actionLink("submit_another", "Submit another response")
                   )
                 ),
                 
                 hr(),
                 br(),
                 p("This project is inspired and motivated by David Smale's",
                   a("Free R Reading Material", href = "https://committedtotape.shinyapps.io/freeR/")),
                 hr(), # horizontal line for visual separation
                 div(p("Compiled by",a("Jingtai Liu",href = "https://twitter.com/time_points")),
                     p("Built with",img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "and",img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/Heart_coraz%C3%B3n.svg/220px-Heart_coraz%C3%B3n.svg.png",height = "20px")),
                     style="text-align: left;")
    ),
    
    
    # for main panel
    mainPanel(width = 9,
              tabsetPanel(tabPanel(div(fa("chart-area", fill = "#158cba"),"Visualization"),
                                   wellPanel(h3("Colors"),
                                             br(),
                                             DTOutput("vis_color")),
                                   wellPanel(h3("Themes"),
                                             br(),
                                             DTOutput("vis_theme")),
                                   wellPanel(h3("Others"),
                                             br(),
                                             DTOutput("vis_others"))
              ),
              tabPanel(div(fa("broom",fill = "#158cba"),"Wrangling"),
                       wellPanel(
                         DTOutput("wrangling"))
              ),
              tabPanel(div(fa("infinity", fill = "#158cba"),"Statistics"),
                       wellPanel(h3("Descriptive"),
                                 br(),
                                 DTOutput("stat_desc")),
                       wellPanel(h3("Inferential"),
                                 br(),
                                 DTOutput("stat_infer"))
              ),
              tabPanel(div(fa("bookmark", fill = "#158cba"),"Resources"),
                       wellPanel(h3("Workshop"),
                                 br(),
                                 DTOutput("resource_ws")),
                       wellPanel(h3("Collection"),
                                 br(),
                                 DTOutput("resource_collect"))
              ),
              tabPanel(div(fa("book", fill = "#158cba"),"Books"),
                       wellPanel(
                         DTOutput("books"))
              ),
              
              tabPanel(div(fa("handshake", fill = "#158cba"),"Contribution"),
                       wellPanel(
                         h3("Previous contributions"),
                         downloadButton("downloadBtn", "Download responses"),
                         br(),
                         br(),
                         DTOutput("response"))
              )
              )
    )
  )
)

# Server -------------------------------

server <- function(input, output) {
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })    
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
  # Allow user to download responses
  output$downloadBtn <- downloadHandler(
    filename = function() { 
      sprintf("R_FreeResource_%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(loadData(), file, row.names = FALSE)
    }
  ) 
  
  
  # render DT table
  output$vis_color <- renderDT({
    datatable(vis_color_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$vis_theme <- renderDT({
    datatable(vis_theme_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$vis_others <- renderDT({
    datatable(vis_others_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$wrangling <- renderDT({
    datatable(wrangling_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$stat_desc <- renderDT({
    datatable(stats_desc_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$stat_infer <- renderDT({
    datatable(stats_infer_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  }) 
  
  output$resource_ws <- renderDT({
    datatable(resource_workshop_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  }) 
  
  output$resource_collect <- renderDT({
    datatable(resource_comp_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  }) 
  
  output$books <- renderDT({
    datatable(book_table,
              colnames = c('Title', 'Author(s)', 'Description', 'Keywords'),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
  output$response <- renderDT({
    data <- loadData()
    data$timestamp <- as.POSIXct(data$timestamp,origin="1970-01-01")
    datatable(data,
              colnames = c("Name","Title","Author(s)","Link","Description","Keywords","Time"),
              rownames = FALSE,
              escape = FALSE,
              class = 'display',
              options = list(pageLength = 20,
                             searchHighlight = TRUE,
                             lengthMenu = c(10, 20, 50))
    )
  })
  
}

# Run the application

shinyApp(ui = ui, server = server)

