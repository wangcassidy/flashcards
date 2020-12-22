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
                                              "text/comma-separated-values_t,text/plain",
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
    values_p <- reactiveValues(isdf=FALSE, index=0)
    values_p$df <- data.frame(Question = "", Answer = "")
    values_p$practice_start <- FALSE
    
    values_t <- reactiveValues(isdf=FALSE, index=0)
    values_t$df <- data.frame(Question = "", Answer = "")
    values_t$test_start <- FALSE
    
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
            values_p$df <- df
            values_t$df <- df
            values_p$isdf <- TRUE
            values_t$isdf <- TRUE
            return(df)
        }
    }))
    
    ################################# Practice ################################# 
    
    # Show blank card to start, or tell user to add cards if no cards have been added yet
    output$practice_question <- renderPlot({
        if (!values_p$isdf) {
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
            if (values_p$isdf) {
                values_p$practice_start <- TRUE
                values_p$index <- 1
                values_p$ordering <- sample(1:dim(values_p$df)[1], dim(values_p$df)[1], replace=FALSE)
                output$practice_question <- renderPlot({
                    text <- as.character(values_p$df[values_p$ordering[values_p$index], "Question"])
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
            if (values_p$practice_start && values_p$isdf && values_p$index <= dim(values_p$df)[1]) {
                output$practice_answer <- renderPlot({
                    text <- as.character(values_p$df[values_p$ordering[values_p$index], "Answer"])
                    draw_card(text, "Answer")
                })
            }
        }
    )
    
    observeEvent(
        input$practice_next,
        {print(values_p$index)
            print(dim(values_p$df)[1])
            print(values_p$practice_start)
            print(values_p$isdf)
            if (values_p$practice_start && values_p$isdf && values_p$index < dim(values_p$df)[1]) {
                values_p$index <- values_p$index + 1
                output$practice_question <- renderPlot({
                    text <- as.character(values_p$df[values_p$ordering[values_p$index], "Question"])
                    draw_card(text, "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
            }
            
            if (values_p$practice_start && values_p$isdf && values_p$index >= dim(values_p$df)[1]) {
                values_p$index <- values_p$index + 1
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
    values_t$correct <- 0
    values_t$incorrect <- 0
    values_t$result <- data.frame(correct=numeric(0), incorrect=numeric(0))
    
    # Show blank card to start, or tell user to add cards if no cards have been added yet
    output$test_question <- renderPlot({
        if (!values_t$isdf) {
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
            if (values_t$isdf) {
                values_t$test_start <- TRUE
                values_t$index <- 1
                values_t$s <- min(isolate(input$ncards), nrow(values_t$df))
                values_t$ordering <- sample(1:nrow(values_t$df), size=values_t$s, replace=FALSE)
                output$test_question <- renderPlot({
                    text <- as.character(values_t$df[values_t$ordering[values_t$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values_t$correct <- 0
                values_t$incorrect <- 0
            }
        }
    )
    
    observeEvent(
        input$test_reveal,
        {
            if (values_t$test_start && values_t$isdf &&  values_t$index <= values_t$s) {
                output$test_answer <- renderPlot({
                    text <- as.character(values_t$df[values_t$ordering[values_t$index], "Answer"])
                    draw_card(text, "Answer")
                })
            }
        }
    )
    
    observeEvent(
        input$correct,
        { 
            if (values_t$test_start && values_t$isdf &&  values_t$index <= values_t$s) {
                output$test_question <- renderPlot({
                    text <- as.character(values_t$df[values_t$ordering[values_t$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                
                values_t$index <- values_t$index + 1
                values_t$correct <- values_t$correct + 1
            }
            
            if (values_t$test_start && values_t$isdf && values_t$index == values_t$s + 1) {
                output$test_question <- renderPlot({
                    draw_card(paste("Score:", round(values_t$correct / (values_t$correct + values_t$incorrect) * 100, 2), "%"), "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values_t$index <- values_t$index + 1
                values_t$result <- rbind(values_t$result, data.frame(correct=values_t$correct, incorrect=values_t$incorrect))
            }
        }
    )
    
    observeEvent(
        input$incorrect,
        {
            if (values_t$test_start && values_t$isdf && values_t$index <= values_t$s) {
                output$test_question <- renderPlot({
                    text <- as.character(values_t$df[values_t$ordering[values_t$index], "Question"])
                    draw_card(text, "Question")
                })
                output$test_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values_t$index <- values_t$index + 1
                values_t$incorrect <- values_t$incorrect + 1
            }
            
            if (values_t$test_start && values_t$isdf && values_t$index == values_t$s + 1) {
                output$test_question <- renderPlot({
                    draw_card(paste("Score:", round(values_t$correct / (values_t$correct + values_t$incorrect) * 100, 2), "%"), "Question")
                })
                output$practice_answer <- renderPlot({
                    draw_card("", "Answer")
                })
                values_t$index <- values_t$index + 1
                values_t$result <- rbind(values_t$result, data.frame(correct=values_t$correct, incorrect=values_t$incorrect))
            }
        }
    )
    
    output$trend <- renderPlot({
        if (nrow(values_t$result) == 1) {
            to_plot <- values_t$result
            to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
            ggplot(data=to_plot, aes(x=1, y=perc)) +
                geom_point() +
                labs(x="Trial", y="% Correct") + 
                theme_classic() +
                scale_x_discrete(breaks=1)
        } else if (nrow(values_t$result) > 0) {
            to_plot <- values_t$result
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
    #     if (nrow(values_t$result) == 1) {
    #         to_plot <- values_t$result
    #         to_plot$perc <- to_plot$correct / (to_plot$correct + to_plot$incorrect) * 100
    #         ggplot(data=to_plot, aes(x=1, y=perc)) +
    #             geom_bar(position="stack") +
    #             labs(x="Trial", y="% Correct") + 
    #             theme_classic() +
    #             scale_x_discrete(breaks=1)
    #     } else if (nrow(values_t$result) > 0) {
    #         to_plot <- values_t$result
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
