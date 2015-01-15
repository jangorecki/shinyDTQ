shinyUI(
  navbarPage(
    "shinyDTQ",
    tabPanel("DTQ",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(width = 3,
                              shinyTree("tree",checkbox=FALSE,search=FALSE,dragAndDrop=FALSE)),
                 mainPanel(width = 9,
                           verbatimTextOutput("dtq_call"),
                           dataTableOutput("dt"))
               )
               #, wellPanel(verbatimTextOutput("tree_str"))
             ))
  )
)