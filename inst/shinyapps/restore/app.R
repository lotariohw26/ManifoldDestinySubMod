compute_residuals <- function(angle, data) {
  # Compute the rotated coordinates
  data$angle <- angle * pi / 180
  data$x_r <- data$x * cos(data$angle) - data$y * sin(data$angle)
  data$y_r <- data$x * sin(data$angle) + data$y * cos(data$angle)
  
  # Compute the residuals
  residuals <- data$x_r - data$y_r
  
  # Return the sum of squared residuals
  return(mean(residuals^2))
}
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
#source(paste0(rprojroot::find_rstudio_root_file(),'/R/realregression.R'))
#source(paste0(rprojroot::find_rstudio_root_file(),'/R/simulations.R'))
#source(paste0(rprojroot::find_rstudio_root_file(),'/R/misc.R'))
#source(paste0(rprojroot::find_rstudio_root_file(),'/R/voterrollanalysis.R'))
#source(paste0(rprojroot::find_rstudio_root_file(),'/R/countingprocess.R'))
#set.seed(123) # for reproducible results
#Sigma <- matrix(c(0.05^2, 0.5*0.05*0.05, 0.5*0.05*0.05, 0.05^2), 2) # correlation matrix
#eleres <- as.data.frame(MASS::mvrnorm(num_rows, mu = c(0.5, 0.3), Sigma = Sigma))
#colnames(eleres) <- c("x", "y")
#eleres
# UI
abc <- ManifoldDestiny::miller_stavros_nevada_2020[[1]] %>%
  dplyr::mutate(S=A1,T=B1,U=A2,V=B2) %>%
  dplyr::mutate(Z=S+T+U+V+A3+B3, Psi=Z/R) %>%
  dplyr::mutate(alpha=(S+U)/(Z)) %>% 
  dplyr::mutate(map=U/(S+T)) %>%
  dplyr::mutate(mbp=T/(T+V)) %>%
  dplyr::select(P,R,S,T,U,V)
#    app_bal <- ballcastsim(dfm,pwn,parv,padv)
eleres <- Countinggraphs(abc)$sdfc %>% dplyr::mutate(Omega=0.8)

ui <- fluidPage(
  titlePanel("Restoration"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("rotation", "Rotation angle (in degrees):", min = -180, max = 180, value = 0)
    ),
    mainPanel(
      plotOutput("plot"),
      tableOutput("table")
    )
  )
)
# Server
server <- function(input, output) {
  data <- reactive({
    #angle <- input$rotation * pi / 180  # Convert angle to radians
    #eleres_transformed <- eleres %>%
    #  mutate(x_r = x * cos(angle) - y * sin(angle),
    #         y_r = x * sin(angle) + y * cos(angle),
    #         alpha1 = Omega * x + (1 - Omega) * y,
    #         alpha2 = Omega * x_r + (1 - Omega) * y_r) 
    #result <- list(eleres_transformed, optimal_angle())
    ##
    ## Use the optim function to find the angle that minimizes the sum of squared residuals
    #result <- optim(0, compute_residuals, data = eleres, control = list(fnscale = -1), method = "Brent", lower = -180, upper = 180)
    #return(result)
    #return(result)
  })  
  
  # Plot
  output$plot <- renderPlot({
    #df1 <- data()[[1]]
    #ggplot(data = df1, aes(x = x_r, y = y_r)) +
    #  geom_point(aes(color = "Before Rotation")) +
    #  geom_point(data = eleres, aes(x = y, y = x, color = "After Rotation")) +
    #  labs(x = "X", y = "Y", color = "Rotation") +
    #  scale_x_continuous(limits = c(0, 1)) +  # Limit x-axis between 0 and 1
    #  scale_y_continuous(limits = c(0, 1)) +  # Limit y-axis between 0 and 1
    #  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # Add 45-degree line
    #  theme_minimal()
  })
  
  # Table
  output$table <- renderTable({
    #df1 <- data()[[1]]
    #opo <- data()[[2]]$param
    #df2 <- df1 %>% dplyr::summarize(m_alpha1=mean(alpha1),m_alpha2=mean(alpha2),omega=mean(Omega), Precincts=n(), opteulr= opo[[1]])
  })
} 
# Run the app
shinyApp(ui = ui, server = server)

