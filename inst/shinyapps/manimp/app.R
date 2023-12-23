######################################################################################
options(scipen=999)
set.seed(1)
library(ManifoldDestiny)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(htmltools)
library(gridExtra)
library(shiny)
library(plotly)
source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
#vl <- ManifoldDestiny::recnav
dfm <- ManifoldDestiny::miller_stavros_nevada_2020[[1]] %>%
  dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>%
  dplyr::mutate(Z=S+T+U+V+A3+B3, Psi=Z/R) %>%
  dplyr::mutate(alpha=(S+U)/(Z)) %>% 
  dplyr::mutate(map=U/(S+T)) %>%
  dplyr::mutate(mbp=T/(T+V)) %>%
  dplyr::select(P,R,S,T,U,V)
#####################################################################################
# Simulation data
## Data used
# Define a Shiny app that references the instance of the class in a reactive contex
##titlePanel("Simulator for the rigging of a natural election"),
##tabsetPanel(
##  tabPanel(
##    "InputPanel", 
##    sidebarLayout(
##      sidebarPanel(
##        h4("Simulation settings:"),
##        textInput("probw",  "Prob of assigning to R:", value='0.51, 0.10'),
##        textInput("prob_rv", "Prob of R voting:", value='0.7, 0.10'),
##        textInput("prob_rm", "Prob of voting by mail if voting by R:", value='0.4, 0.10'),
##        textInput("prob_dv", "Prob of D voting:", value='0.5, 0.10'),
##        textInput("prob_dm", "Prob of voting by mail if voting by D:", value='0.6, 0.10'),
##        h4("Rigged election settings"),
##        selectInput("form", "Rigged:",
##                    choices = c("Normal Form" = "1",
##                                "Hybrid Form" = "2",
##                                "Opposition Form" = "3"), selected = "1"),
##        selectizeInput("prevar","Predermined variables",choices=c("alpha","x","y","zeta","omega"),multiple =TRUE,options=list(maxItems=3),
##                       selected = c("alpha", "x", "y")),
##        selectizeInput("endvar","Predermined variables",choices=c("alpha","x","y","zeta","lamda"),multiple =TRUE,options=list(maxItems=2), selected=c("zeta","lamda")),
##        textInput("pn", "Polynomial", value='0.23'),
##          selectInput("linear", "Polynomial:",
##                      choices = c("Linear" = "1",
##                                  "Quadratic" = "2",
##                                  "Cubic" = "3",
##                                  "Quartic" = "4"), selected = "1"),
##          textInput("wn", "White noise", value='0.0, 0.0'),
##          textInput("kvec", "parameters", value='0,0.5,0.5'),
##          selectInput("loss", "Loss function:",
##                      choices = c("Abc" = "1",
##                                  "Def" = "2"), selected = "1"),
##          selectizeInput("rotation", "Euler-rotation order (optional)", choices = c(1, 2, 3, 4, 5, 6), multiple = TRUE,options = list(maxItems = 3)),
##          actionButton("run", "Run Simulation")
##        ),
###        mainPanel(
###          h4("Fair election estimation"),
###          verbatimTextOutput(outputId = "table_dsc1"),
#          h4("Rigged election estimation"),
#          verbatimTextOutput(outputId = "table_dsc2")
#        )
#      )
#    ),
#    tabPanel("Quintile", 
#             sidebarLayout(
#               sidebarPanel(
#               ),
#               mainPanel(
#                 plotOutput("plotrf")
#               )
#             )
#    ),
#    tabPanel("Scatterplot", 
#             sidebarLayout(
#               sidebarPanel(
#               ),
#               mainPanel(
#                 h4("Fair election"),
#                 plotOutput("plotxyn"),
#                 h4("Rigged election"),
#                 plotOutput("plotxyr")
#               )
#             )
#    ),
#    tabPanel("3d plots", 
#             sidebarLayout(
#               sidebarPanel(
#               ),
#               mainPanel(
#                 fluidRow(
#                   plotlyOutput("plot3d1"),
#                   plotlyOutput("plot3d2")
#                 )
#               )
#             )
#    ),
#    tabPanel("Descriptive", 
#             sidebarLayout(
#               sidebarPanel(
#               ),
#               mainPanel(
#                 h4("Fair election"),
#                 verbatimTextOutput(outputId = "table_dsc1"),
#                 h4("Rigged election"),
#                 verbatimTextOutput(outputId = "table_dsc2")
#               )
#             )
#    )
#  )
#)
#server <- function(input, output, session) {
#  #observeEvent(input$form, {
#  #  # Update the default value of the "Polynomial Equation(s)" text input
#  #  #if (input$form == "1") {
#  #  #  updateSelectizeInput(session, "prevar", choices = c("alpha", "x", "y", "zeta", "lamda"), selected = c("alpha", "x", "y"))
#  #  #  updateSelectizeInput(session, "endvar", choices = c("alpha", "x", "y", "zeta", "lamda"), selected = c("zeta", "lamda"))
#  #  #} else if (input$form == "2") {
#  #  #  updateSelectizeInput(session, "prevar", choices = c("alpha", "g", "h", "Gamma", "Omega"), selected = c("alpha", "g", "h"))
#  #  #  updateSelectizeInput(session, "endvar", choices = c("alpha", "g", "h", "Gamma", "Omega"), selected = c("Gamma", "Omega"))
#  #  #} else if (input$form == "3") {
#  #  #  updateSelectizeInput(session, "prevar", choices = c("xi", "n", "m", "lamda", "Omega"), selected = c("xi", "n", "m"))
#  #  #  updateSelectizeInput(session, "endvar", choices = c("xi", "n", "m", "lamda", "Omega"), selected = c("lamda", "Omega"))
#  # # }
#  #})
#  # Rigging an election
#  result <- reactive({
#    # Ballot settings
#    ## manual
#	  browser()
#    los <- 1 
#    rots <- 0
#    kvec <- c(k0=0.0,k1=0.5,k2=0.5)
#    frm <- 1
#    linp <- 1
#    ## interactive
#    #pwn <- as.numeric(strsplit(input$probw, ",")[[1]])
#    #parv <- c(as.numeric(strsplit(input$prob_rv, ",")[[1]]),as.numeric(strsplit(input$prob_rm, ",")[[1]]))[c(1,3,2,4)]
#    #padv <- c(as.numeric(strsplit(input$prob_dv, ",")[[1]]),as.numeric(strsplit(input$prob_dm, ",")[[1]]))[c(1,3,2,4)]
#    ## Rigged settings
#    #pwn <- c(m=0.51,s=0.10); parv <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10) ;padv <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
#    #ends <- c("zeta","lamda")#input$endvar
#    #pres <- c("alpha","x","y")#input$prevar
#    ## manual
#    #frm <- as.numeric(input$form)
#    #linp <- as.numeric(input$linear)
#    #pres <- as.vector(input$prevar)
#    #ends <- as.vector(input$endvar)
#    #wn <- as.numeric(strsplit(input$wn, ",")[[1]])
#    #pnc <- input$pn
#    #kvs <- as.numeric(strsplit(input$kvec, ",")[[1]])
#    #kvec <- c(k0=kvs[1],k1=kvs[2],k2=kvs[3])
#    isys <- list(frm=frm,pre=pres,end=ends,me=c(plnr=linp,rots=0))
#    ## interactive
#    ## Simulation of ballot voting
#    app_bal <- ballcastsim(dfm,pwn,parv,padv)
#    # fair election
#    app_n_cou <- Countinggraphs(app_bal)
#    app_n_cou$sortpre()
#    app_n_cou$plotxy()
#    app_n_cou$plot2d()
#    # rigged election
#    app_exr_cou <- Countinggraphs(app_bal)
#    app_exr_cou$sortpre()
#    app_exr_cou$mansys(sygen=isys)
#    #app_exr_cou$enf[[3]]
#    #app_exr_cou$setres(pnc)
#    #app_exr_cou$manimp(init_par=kvec,man=TRUE,wn=c(wn[1],wn[2]))
#    app_exm_cou <- Countinggraphs(app_exr_cou$rdfc)
#    app_exm_cou$sortpre()
#    app_exm_cou$plotxy()
#    app_exm_cou$plot2d()
#    #result <- list(app_n_cou,app_exm_cou)
#    return(list(app_n_cou,app_exm_cou))
#
#  })
# # Table
# output$table_dsc1 <- renderPrint({
#   sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
#   nel <- Estimation(result()[[1]]$rdfc,1)
#   nel$regression(sugsol[1])
#   n1 <- summary(nel$regsum[[1]])
#   nel$regression(sugsol[2])
#   n2 <- summary(nel$regsum[[1]])
#   list(n1,n2)
# })
# output$table_dsc2 <- renderPrint({
#   sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
#   ner <- Estimation(result()[[2]]$rdfc,1)
#   ner$regression(sugsol[1])
#   r1 <- summary(ner$regsum[[1]])
#   ner$regression(sugsol[2])
#   r2 <- summary(ner$regsum[[1]])
#   list(r1,r2)
# })
# # Plot 
# output$plotrf <- renderPlot({
#    dft <- result()
#    gm1 <- dft[[1]]$pl_2dsort[[1]]
#    gm2 <- dft[[2]]$pl_2dsort[[1]]
#    cowplot::plot_grid(cowplot::plot_grid(gm1, labels = "Fair election"), cowplot::plot_grid(gm2, labels = "Rigged election"), ncol = 2, align = 'hv')
#  })
# output$plotxyn <- renderPlot({
#    dft <- result()
#    gm1 <- dft[[1]]$pl_corrxy[[1]]
#    gm3 <- dft[[1]]$pl_corrxy[[1]]
#    cowplot::plot_grid(gm1, gm3, ncol = 2, labels = c("Column 1", "Column 2"))
#  })
# output$plotxyr <- renderPlot({
#    dft <- result()
#    gm2 <- dft[[2]]$pl_corrxy[[1]]
#    gm4 <- dft[[2]]$pl_corrxy[[1]]
#    cowplot::plot_grid(gm2, gm4, ncol = 2, labels = c("Column 1", "Column 2"))
#  })
# output$plot3d1 <- renderPlotly({
#    gdf <- result()[[1]]$rdfc %>% dplyr::select(c('alpha','x','y'))
#    mrdfc <- as.matrix(gdf)
#    z <- mrdfc[,1]
#    x <- mrdfc[,2]
#    y <- mrdfc[,3]
#    p1 <- plotly::plot_ly(x=x,y=y,z=z,type="scatter3d", mode="markers", marker=list(size=3))
#    plotly::layout(p1,title = "Fair election")
# })
# output$plot3d2 <- renderPlotly({
#    gdf <- result()[[2]]$rdfc %>% dplyr::select(c('alpha','x','y'))
#    mrdfc <- as.matrix(gdf)
#    z <- mrdfc[,1]
#    x <- mrdfc[,2]
#    y <- mrdfc[,3]
#    p2 <- plotly::plot_ly(x=x,y=y,z=z,type="scatter3d", mode="markers",marker = list(size = 3))
#    plotly::layout(p2,title = "Rigged election")
# })
# output$table_dsc <- renderPrint({
# })
# output$sidebarText <- renderText({
# })
#}
#library(shiny)
#ui <- fluidPage(
#  titlePanel("Simulator for the rigging of a natural election"),
#  tabsetPanel(
#    tabPanel(
#    #tabPanel("Quintile", 
    #         sidebarLayout(
    #           sidebarPanel(
    #             h4("Simulation settings:"),
    #             textInput("probw", "Prob of assigning to R:", value='0.51, 0.10'),
    #             textInput("prob_rv", "Prob of R voting:", value='0.7, 0.10'),
    #             textInput("prob_rm", "Prob of voting by mail if voting by R:", value='0.4, 0.10'),
    #             textInput("prob_dv", "Prob of D voting:", value='0.5, 0.10'),
    #             textInput("prob_dm", "Prob of voting by mail if voting by D:", value='0.6, 0.10'),
    #             h4("Rigged election settings"),
    #             selectInput("form", "Rigged:",
    #                         choices = c("Normal Form" = "1",
    #                                     "Hybrid Form" = "2",
    #                                     "Opposition Form" = "3"), selected = "1"),
    #             selectizeInput("prevar", "Predermined variables", choices=c("alpha","x","y","zeta","omega"), multiple = TRUE, options=list(maxItems=3), selected = c("alpha", "x", "y")),
    #             selectizeInput("endvar", "Predermined variables", choices=c("alpha","x","y","zeta","lamda"), multiple = TRUE, options=list(maxItems=2), selected=c("zeta","lamda")),
    #             textInput("pn", "Polynomial", value='0.23'),
    #             selectInput("linear", "Polynomial:",
    #                         choices = c("Linear" = "1",
    #                                     "Quadratic" = "2",
    #                                     "Cubic" = "3",
    #                                     "Quartic" = "4"), selected = "1"),
    #             textInput("wn", "White noise", value='0.0, 0.0'),
    #             textInput("kvec", "parameters", value='0,0.5,0.5'),
    #             selectInput("loss", "Loss function:",
    #                         choices = c("Abc" = "1",
    #                                     "Def" = "2"), selected = "1"),
    #             selectizeInput("rotation", "Euler-rotation order (optional)", choices = c(1, 2, 3, 4, 5, 6), multiple = TRUE, options = list(maxItems = 3))
    #           ),
#mainPanel(
#library(shiny)
#ui <- fluidPage(
#  # Create an action button with the label "Run Simulation"
#  titlePanel("Simulator for the rigging of a natural election"),
#  #actionButton(inputId = "run", label = "Run Simulation"),
#  verbatimTextOutput("result"),
#  mainPanel(
#    h4("Fair election estimation"),
#    #verbatimTextOutput(outputId = "table_dsc1"),
#    h4("Rigged election estimation"),
#    #verbatimTextOutput(outputId = "table_dsc2")
#  )
#)
#server <- function(input, output) {
#  # Observe the button click
#  dfgp <- reactive({
#  #dfgp <- eventReactive(input$run, {
#    pwn <- c(m=0.51,s=0.10); parv <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10) ;padv <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
#    los <- 1 
#    rots <- 0
#    kvec <- c(k0=0.0,k1=0.5,k2=0.5)
#    frm <- 1
#    linp <- 1
#    ends <- c("zeta","lamda")#input$endvar
#    pres <- c("alpha","x","y")#input$prevar
#    ### interactive
#    isys <- list(frm=frm,pre=pres,end=ends,me=c(plnr=linp,rots=0))
#    ### interactive
#    ### Simulation of ballot voting
#    app_bal <- ballcastsim(dfm,pwn,parv,padv)
#    ## fair election
#    app_n_cou <- Countinggraphs(app_bal)
#    app_n_cou$sortpre()
#    app_n_cou$plotxy()
#    app_n_cou$plot2d()
#    ## rigged election
#    app_exr_cou <- Countinggraphs(app_bal)
#    app_exr_cou$sortpre()
#    app_exr_cou$mansys(sygen=isys)
#    #app_exr_cou$enf[[3]]
#    #app_exr_cou$setres(pnc)
#    #app_exr_cou$manimp(init_par=kvec,man=TRUE,wn=c(wn[1],wn[2]))
#    app_exm_cou <- Countinggraphs(app_exr_cou$rdfc)
#    app_exm_cou$sortpre()
#    app_exm_cou$plotxy()
#    app_exm_cou$plot2d()
#    browser()
#    return(list(app_n_cou,app_exm_cou))
#  })
#
#  output$table_dsc1 <- renderPrint({
#          browser()
#    sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
#    #nel <- Estimation(dfgp()[[1]]$rdfc,1)
#    #nel$regression(sugsol[1])
#    #n1 <- summary(nel$regsum[[1]])
#    #nel$regression(sugsol[2])
#    #n2 <- summary(nel$regsum[[1]])
#    #list(n1,n2)
#  })
#  output$table_dsc2 <- renderPrint({
#	  browser()
#    #sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
#    #ner <- Estimation(dfgp()[[2]]$rdfc,1)
#    #ner$regression(sugsol[1])
#    #r1 <- summary(ner$regsum[[1]])
#    #ner$regression(sugsol[2])
#    #r2 <- summary(ner$regsum[[1]])
#    #list(r1,r2)
#  })
#}
#runApp(shinyApp(ui = ui, server = server), port = 8011)
#
library(shiny)
ui <- fluidPage(
  titlePanel("Reactive Example in Shiny"),
  sidebarLayout(
    sidebarPanel(
      numericInput("num1", "Enter first number:", value = 1),
      numericInput("num2", "Enter second number:", value = 1),
      selectInput("operator", "Choose an operation:", 
                  choices = c("Add", "Subtract", "Multiply", "Divide"))
    ),
    mainPanel(
      verbatimTextOutput(outputId = "table_dsc1"),
      verbatimTextOutput(outputId = "table_dsc2"),
      plotOutput("plotxyn"),
      plotOutput("plotxyr"),
      plotlyOutput("plot3d1"),
      plotlyOutput("plot3d2")
    )
  )
)
server <- function(input, output) {
  # Create a reactive expression for the result
  result <- reactive({
    pwn <- c(m=0.51,s=0.10); parv <- c(vdm=0.7,mdm=0.4,vds=0.10,mds=0.10) ;padv <- c(vdm=0.5,mdm=0.6,vds=0.10,mds=0.10)
    los <- 1 
    rots <- 0
    kvec <- c(k0=0.0,k1=0.5,k2=0.5)
    frm <- 1
    linp <- 1
    ends <- c("zeta","lamda")#input$endvar
    pres <- c("alpha","x","y")#input$prevar
    ### Interactive
    isys <- list(frm=frm,pre=pres,end=ends,me=c(plnr=linp,rots=0))
    ### Interactive
    ### Simulation of ballot voting
    app_bal <- ballcastsim(dfm,pwn,parv,padv)
    ## Fair election
    app_n_cou <- Countinggraphs(app_bal)
    app_n_cou$sortpre()
    app_n_cou$plotxy()
    app_n_cou$plot2d()
    ## Rigged election
    app_exr_cou <- Countinggraphs(app_bal)
    app_exr_cou$sortpre()
    app_exr_cou$mansys(sygen=isys)
    #app_exr_cou$enf[[3]]
    #app_exr_cou$setres(pnc)
    #app_exr_cou$manimp(init_par=kvec,man=TRUE,wn=c(wn[1],wn[2]))
    app_exm_cou <- Countinggraphs(app_exr_cou$rdfc)
    app_exm_cou$sortpre()
    app_exm_cou$plotxy()
    app_exm_cou$plot2d()
    #browser()
    abc <- 1+2
    list(app_n_cou,app_exm_cou)
  })  
  output$table_dsc1 <- renderText({
    sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
    #browser()
    #nel <- Estimation(result()[[1]]$rdfc,1)
    #nel$regression(sugsol[1])
    #nel$regsum[[1]]
    #n1 <- summary(nel$regsum[[1]])
    #nel$regression(sugsol[2])
    #n2 <- summary(nel$regsum[[1]])
    #list(n1,n2)
    'abc'
  })
  output$table_dsc2 <- renderPrint({
    sugsol <- c(alpha='k0+k1*x+k2*y',alpha='k0+k1*x+k2*y+k3*zeta')
    #ner <- Estimation(result()[[2]]$rdfc,1)
    #ner$regression(sugsol[1])
    #r1 <- summary(ner$regsum[[1]])
    #ner$regression(sugsol[2])
    #r2 <- summary(ner$regsum[[1]])
    #list(r1,r2)
    'def'
  })
   # Plot 
  output$plotrf <- renderPlot({
    dft <- result()
    gm1 <- dft[[1]]$pl_2dsort[[1]]
    gm2 <- dft[[2]]$pl_2dsort[[1]]
    cowplot::plot_grid(cowplot::plot_grid(gm1, labels = "Fair election"), cowplot::plot_grid(gm2, labels = "Rigged election"), ncol = 2, align = 'hv')
   })
  output$plotxyn <- renderPlot({
     dft <- result()
     gm1 <- dft[[1]]$pl_corrxy[[1]]
     gm3 <- dft[[1]]$pl_corrxy[[1]]
     cowplot::plot_grid(gm1, gm3, ncol = 2, labels = c("Column 1", "Column 2"))
   })
  output$plotxyr <- renderPlot({
     dft <- result()
     gm2 <- dft[[2]]$pl_corrxy[[1]]
     gm4 <- dft[[2]]$pl_corrxy[[1]]
     cowplot::plot_grid(gm2, gm4, ncol = 2, labels = c("Column 1", "Column 2"))
   })
  output$plot3d1 <- renderPlotly({
     gdf <- result()[[1]]$rdfc %>% dplyr::select(c('alpha','x','y'))
     mrdfc <- as.matrix(gdf)
     z <- mrdfc[,1]
     x <- mrdfc[,2]
     y <- mrdfc[,3]
     p1 <- plotly::plot_ly(x=x,y=y,z=z,type="scatter3d", mode="markers", marker=list(size=3))
     plotly::layout(p1,title = "Fair election")
  })
  output$plot3d2 <- renderPlotly({
     gdf <- result()[[2]]$rdfc %>% dplyr::select(c('alpha','x','y'))
     mrdfc <- as.matrix(gdf)
     z <- mrdfc[,1]
     x <- mrdfc[,2]
     y <- mrdfc[,3]
     p2 <- plotly::plot_ly(x=x,y=y,z=z,type="scatter3d", mode="markers",marker = list(size = 3))
     plotly::layout(p2,title = "Rigged election")
  })
  output$table_dsc <- renderPrint({
  })
  output$sidebarText <- renderText({
  })
}
#shinyApp(ui = ui, server = server)
runApp(shinyApp(ui = ui, server = server), port = 8011)

