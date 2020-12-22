#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggfittext)

source('draw.R', local = TRUE)

ui <- navbarPage(
    "Flashcards",
    
    # CSV file upload
    
    tabPanel(
        "Add cards",
        sidebarLayout(
            
            # CSV upload sidebar
            sidebarPanel(tags$style(".well {background-color:white;}"),
                         
                         # Select CSV file
                         fileInput("file1",
                                   "Choose CSV File",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
                         p("A valid CSV file should consist of 2 columns. The first column should contain the questions, and the second column should contain the answers."),
                         tags$hr(),
                         
                         checkboxInput("header", "Header", TRUE),
                         
                         radioButtons("sep", "Separator",
                                      choices = c(Comma = ",",
                                                  Semicolon = ";",
                                                  Tab = "\t"),
                                      selected = ","),
                         
                         radioButtons("quote", "Quote",
                                      choices = c(None = "",
                                                  "Double Quote" = '"',
                                                  "Single Quote" = "'"),
                                      selected = '"'),
                         
                         tags$hr()
            ),
            
            # CSV file display
            mainPanel(
                DT::dataTableOutput("contents")
            )
        )
    ),
    
    # Practice with flashcards
    tabPanel(
        "Practice",
        splitLayout(
            cellWidths=c("10%", "90%"),
            cellArgs = list(style = "padding: 30px"),
            verticalLayout(
                br(),
                br(),
                br(),
                fluidRow(
                    actionButton(width="120px",
                                 "practice_start",
                                 "Start/Restart"
                    )
                ),
                br(),
                fluidRow(
                    actionButton(width="120px",
                                 "practice_reveal",
                                 "Reveal Answer"
                    )
                ),
                br(),
                fluidRow(
                    actionButton(width="120px",
                                 "practice_next",
                                 "Next"
                    )
                )
            ),
            # Card display
            splitLayout(
                plotOutput("practice_question"),
                plotOutput("practice_answer")
            )
            
        )
    ),
    
    # Test yourself with flashcards
    tabPanel(
        "Test Yourself",
        sidebarLayout(
            sidebarPanel(
                width=2,
                fluidRow(
                    numericInput(
                        "ncards",
                        label="Number of cards to use",
                        value=1,
                        min=1
                    )
                ),
                fluidRow(
                    actionButton(width="120px",
                                 "test_start",
                                 "Start/Restart"
                    )
                ),
                br(),
                fluidRow(
                    actionButton(width="120px",
                                 "test_reveal",
                                 "Check Answer"
                    )
                ),
                br(),
                fluidRow(
                    actionButton(width="120px",
                                 "correct",
                                 "Correct"
                    ),
                    actionButton(width="120px",
                                 "incorrect",
                                 "Incorrect"
                    )
                )
            ),
            mainPanel(width=10,
                      # Card display
                      column(
                          width=6, 
                          plotOutput("test_question")
                      ),
                      column(
                          width=6, 
                          plotOutput("test_answer")
                      )
            )
        )
    ),
    tabPanel(
        "Analytics",
        splitLayout(
            plotOutput("trend"),
            plotOutput("bar")
        )
        
    )         
    
)


server <- function(input, output) {
    values <- reactiveValues(isdf=FALSE, index=0)
    values$df <- data.frame(Question = "", Answer = "")
    values$practice_start <- FALSE
    values$test_start <- FALSE
    
    # Read in CSV file and save as dataframe 
    output$contents <- DT::renderDataTable(DT::datatable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        if(length(df) != 2) {
            df <- NA
            stop(safeError(
                "Table needs to have exactly 2 columns"
            ))
        } else {
            names(df) <- c("Question", "Answer")
            values$df <- df
            values$isdf <- TRUE
            return(df)
        }
    }))
    
    ################################# Practice ################################# 
    
    # Show blank card to start, or tell user to add cards if no cards have been added yet
    output$practice_question <- renderPlot({
        if (!values$isdf) {
            draw_card("Please add cards.", "Question")
        } else {
            draw_card("", "Question")
        }
    })
    
    output$practice_answer <- renderPlot({
        draw_card("", "Answer")
    })
    
    observeEvent(
        input$practice_start,
        {
            if (values$isdf) {
                values$practice_start <- TRUE
                values$index <- 1
                values$ordering <- sample(1:dim(values$df)[1], dim(values$df)[1], replace=FALSE)
                output$practice_question <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Question"])
                    draw_card(text, "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
            }
        }
    )
    
    observeEvent(
        input$practice_reveal,
        {
            if (values$practice_start && values$isdf && values$index <= dim(values$df)[1]) {
                output$practice_answer <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Answer"])
                    draw_card(text, "Answer")
                })
            }
        }
    )
    
    observeEvent(
        input$practice_next,
        {
            if (values$practice_start && values$isdf && values$index < dim(values$df)[1]) {
                values$index <- values$index + 1
                output$practice_question <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Question"])
                    draw_card(text, "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
            }
            
            if (values$practice_start && values$isdf && values$index >= dim(values$df)[1]) {
                values$index <- values$index + 1
                output$practice_question <- renderPlot({
                    draw_card("Done! Press Restart to practice again.", "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
            }
        }
    )
    
    ################################# Test ################################# 
    values$correct <- 0
    values$incorrect <- 0
    values$result <- data.frame(correct=numeric(0), incorrect=numeric(0))
    
    # Show blank card to start, or tell user to add cards if no cards have been added yet
    output$test_question <- renderPlot({
        if (!values$isdf) {
            draw_card("Please add cards.", "Question")
        } else {
            draw_card("", "Question")
        }
    })
    
    output$test_answer <- renderPlot({
        draw_card("", "Answer")
    })
    
    observeEvent(
        input$test_start,
        {
            if (values$isdf) {
                values$test_start <- TRUE
                values$index <- 1
                values$s <- min(isolate(input$ncards), nrow(values$df))
                values$ordering <- sample(1:nrow(values$df), size=values$s, replace=FALSE)
                output$test_question <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values$correct <- 0
                values$incorrect <- 0
            }
        }
    )
    
    observeEvent(
        input$test_reveal,
        {
            if (values$test_start && values$isdf &&  values$index <= values$s) {
                output$test_answer <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Answer"])
                    draw_card(text, "Answer")
                })
            }
        }
    )
    
    observeEvent(
        input$correct,
        { 
            if (values$test_start && values$isdf &&  values$index <= values$s) {
                output$test_question <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                
                values$index <- values$index + 1
                values$correct <- values$correct + 1
            }
            
            if (values$test_start && values$isdf && values$index == values$s + 1) {
                output$test_question <- renderPlot({
                    draw_card(paste("Score:", round(values$correct / (values$correct + values$incorrect) * 100, 2), "%"), "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values$index <- values$index + 1
                values$result <- rbind(values$result, data.frame(correct=values$correct, incorrect=values$incorrect))
            }
        }
    )
    
    observeEvent(
        input$incorrect,
        {
            if (values$test_start && values$isdf && values$index <= values$s) {
                output$test_question <- renderPlot({
                    text <- as.character(values$df[values$ordering[values$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values$index <- values$index + 1
                values$incorrect <- values$incorrect + 1
            }
            
            if (values$test_start && values$isdf && values$index == values$s + 1) {
                output$test_question <- renderPlot({
                    draw_card(paste("Score:", round(values$correct / (values$correct + values$incorrect) * 100, 2), "%"), "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values$index <- values$index + 1
                values$result <- rbind(values$result, data.frame(correct=values$correct, incorrect=values$incorrect))
            }
        }
    )
    
    output$trend <- renderPlot({
        if (nrow(values$result) == 1) {
            to_plot <- values$result
            to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
            ggplot(data=to_plot, aes(x=1, y=perc)) +
                geom_point() +
                labs(x="Trial", y="% Correct") + 
                theme_classic() +
                scale_x_discrete(breaks=1)
        } else if (nrow(values$result) > 0) {
            to_plot <- values$result
            to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
            ggplot(data=to_plot, aes(x=1:nrow(to_plot), y=perc)) +
                geom_line() +
                geom_point() +
                theme_classic() +
                scale_x_continuous(name= "Trial", breaks=1:nrow(to_plot)) +
                scale_y_continuous(name="% Correct", limits=c(0, 100))
        }
    })
    
    # output$bar <- renderPlot({
    #     if (nrow(values$result) == 1) {
    #         to_plot <- values$result
    #         to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
    #         ggplot(data=to_plot, aes(x=1, y=perc)) +
    #             geom_bar(position="stack") +
    #             labs(x="Trial", y="% Correct") + 
    #             theme_classic() +
    #             scale_x_discrete(breaks=1)
    #     } else if (nrow(values$result) > 0) {
    #         to_plot <- values$result
    #         to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
    #         ggplot(data=to_plot, aes(x=1:nrow(to_plot), y=perc)) +
    #             geom_bar(position="stack") +
    #             theme_classic() +
    #             scale_x_continuous(name= "Trial", breaks=1:nrow(to_plot)) +
    #             scale_y_continuous(name="% Correct", limits=c(0, 100))
    #     }
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
