options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmltools)
library(gridExtra)
library(shiny)
library(DT)
library(kableExtra)
library(htmlTable)
library(usethis)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
md <- jsonlite::fromJSON(paste0(rprojroot::find_rstudio_root_file(),"/data-raw/metadata.json"))
dlname <- c("app0","app1","app2")
googlesheets4::gs4_auth(email="lotariohw26@gmail.com")
###############################################################################################################################################################
ui <- fluidPage(
  titlePanel("Rigged election results analyzer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "app_select", label = "Select an election from the library",
              choices = dlname,
              selected = "biden_trump_nevada_2020"),
      selectInput("form", "Rigged:",
                  choices = c("Normal Form" = "1",
                              "Hybrid Form" = "2",
                              "Opposition Form" = "3"), selected = "2"),
      textInput("purge", "Purge criteria:",value="0;0;0"),
      textInput("meqf", "Manifold object(s):",
                value = "alpha=k0+k1*g+k2*h"),
      textInput("solvf", "Solve for:",
                value = "g"),
      br(),
      h4("Rotation Settings (Euler)"),
      selectizeInput("rotation", "Euler-rotation order (optional)", choices = c(1, 2, 3, 4, 5, 6), multiple = TRUE,options = list(maxItems = 3)),
      sliderInput("theta", "Theta:", min = 0, max = 360, value = 0),
      sliderInput("phi", "Phi:", min = 0, max = 360, value = 0),
      sliderInput("rho", "Rho:", min = 0, max = 360, value = 0),
      h4("Suggested solution:"),
      textOutput("sidebarText")  # Output element for dynamic text
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Descriptive", verbatimTextOutput(outputId = "table_dsc")),
        tabPanel("2D Scatter Plots", plotOutput(outputId = "plot_xy")),
        tabPanel("Quantile Plot", plotOutput(outputId = "plot_q")),
        tabPanel("Bow Plot", plotOutput(outputId = "plot_bow")),
        tabPanel("3D rotation", plotlyOutput(outputId = "plot_3d")),
        tabPanel("3D scatter plots", htmlOutput(outputId = "plot_3ds")),
        tabPanel("Regressions", verbatimTextOutput(outputId = "print_sum")),
        tabPanel("Residuals", plotOutput(outputId = "plot_res")),
        tabPanel("Comparison", verbatimTextOutput(outputId = "print_com")),
        tabPanel("Metadata", verbatimTextOutput(outputId = "meta_dsc"))
      )
    )
  )
)
server <- function(input, output, session) {
  observeEvent(input$form, {
     #Update the default value of the "Polynomial Equation(s)" text input
    if (input$form == "1") {
      updateTextInput(session, "meqf", value = "alpha=k0+k1*x+k2*y")
      updateTextInput(session, "solvf", value = "y")
    } else if (input$form == "2") {
      updateTextInput(session, "meqf", value = "alpha=k0+k1*g+k2*h")
      updateTextInput(session, "solvf", value = "g")
    } else if (input$form == "3") {
      updateTextInput(session, "meqf", value = "alpha=k0+k1*m+k2*n")
      updateTextInput(session, "solvf", value = "m")
    }
  })  
  cformo <- reactive({
    # Manual
    seldata <- get(load(paste0(rprojroot::find_rstudio_root_file(),'/data/',input$app_select,".rda")))
    mds <- md[[input$app_select]]
    ## Purge
    abc <- strsplit(input$purge,";")
    mds$mtd$prg$cnd <- c(0)
    mds$mtd$prg$stuv <- c(0,0,0,0)
    mds$mtd$prg$blup[1] <- 0
    mds$mtd$prg$blup[2] <- 1
    ## Solution
    mds$sgs$fr <- as.numeric(input$form)
    mds$sgs$eq <- as.numeric(input$meqf)
    mds$sgs$va <- as.numeric(input$solvf)
    ## Rotation
    mds$mtd$sgs$ro[1] <- input$theta*pi/180
    mds$mtd$sgs$ro[2] <- input$phi*pi/180
    mds$mtd$sgs$ro[3] <- input$rho*pi/180
    ## Selreport
    return(selreport(seldata,mds))
  })
  observe(print(cformo()[[1]]$desms))
  output$table_dsc <- renderPrint({
    print(cformo()[[1]]$desms)
  })
  output$plot_q <- renderPlot({
    cformo()[[1]]$pl_2dsort[[1]]
  })
  output$plot_bow <- renderPlot({
    cformo()[[5]]$pl_2dsort[[1]]
  })
  output$plot_xy <- renderPlot({
    cowplot::plot_grid(plotlist=cformo()[[1]]$pl_corrxy[c(1,2)],ncol=2)
  })
  output$plot_3d <- renderPlotly({
    cformo()[[1]]$rotplotly[[1]]
  })
  output$plot_3ds <- renderUI({
    cformo()[[1]]$all_pl_3d_mani[[1]]
  })
  output$print_sum <- renderPrint({
    list(summary(cformo()[[3]]$regsum[[1]]),
	 summary(cformo()[[2]]$regsum[[1]]))
  })
  output$plot_res <- renderPlot({
    pla <- cformo()[[2]]$resplots[[1]][[c(1)]]
    plb <- cformo()[[2]]$resplots[[2]][[c(1)]]
    plc <- cformo()[[2]]$resplots[[3]][[c(1)]]
    cowplot::plot_grid(plotlist=list(pla,plb,plc),ncol=1)
  })
  output$print_com <- renderPrint({
    print(as.data.frame(cformo()[[2]]$comdesc)) #%>% dplyr::select(1,2,3,4)
    print(as.data.frame(cformo()[[2]]$compare)) #%>% dplyr::select(1,2,3,4)
  })
  output$meta_dsc <- renderPrint({
    library(RefManageR)
    BibOptions(check.entries = FALSE, bib.style = "authoryear", style = "markdown", dashed = TRUE)
    tes <- paste0(rprojroot::find_rstudio_root_file(),'/inst/references/man_bib.bib')	
    bib <- RefManageR::ReadBib(tes)
    RefManageR::NoCite(bib)
    RefManageR::PrintBibliography(bib, .opts = list(check.entries = FALSE, sorting = "ynt"))
  })
  output$sidebarText <- renderText({
    paste0(cformo()[[6]]$mtd$sgs$eq)
  })
}
runApp(shinyApp(ui = ui, server = server), port = 8100)



