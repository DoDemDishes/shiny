library(shiny)
library(markdown)

navbarPage("Lead Automation Tool",
    tabPanel("Introduction",
        sidebarLayout(
            sidebarPanel(
             tags$p(
               selectInput("event", label = h3("Choose an event"), 
                choices = list("New leads" = 1, "Leads update" = 2, "GMV update" = 3), 
                selected = 1)
               ),
             tags$p(
               selectInput("platform", label = h3("Choose a platform"), 
                choices = list("Amazon" = 1, "eBay" = 2, "Webstore" = 3), 
                selected = 1)

               ),
             tags$p(
               fileInput("file", "Upload file")
               )
             ),
            mainPanel(
            	#includeMarkdown("C:/Users/Marta Klimaszewska/Downloads/Lead automation app/intro.md")
                )
            )
        ),
    tabPanel("Filtering",
       sidebarPanel(
        tags$p(
            radioButtons('sep', 'Separator',
                c(Comma=',', Semicolon=';', Tab='\t')
                ),
            actionButton("change", "Change the separator")
            ),
        tags$p(
            uiOutput("ui"),
            actionButton("go", "Go!")
            ),
        tags$p(
            downloadButton('downloadData', 'Download raw'),
            downloadButton('downloadData2', 'Download ready')
            )
        ),
       mainPanel(
        div(tableOutput("df_data_out"), style = "font-size: 75%; width: 75%"),
        div(uiOutput("df_table"), style = "font-size: 75%; width: 75%"),
        tableOutput("values")
        )
       )
    )