library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("orders_export", "Choose CSV File"),
      # Button
      downloadButton("downloadData", "Download .csv")
    ),
    mainPanel(
      dataTableOutput("contents")
    )
  )
)