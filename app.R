#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(ggplot2) 
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(shinydashboard)
library(LaplacesDemon)
options(max.print=1000000)
fig.width <- 1375
fig.height <- 550
fig.width2 <- 1375  
fig.height2 <- 730
fig.width3 <- 1375
fig.height3 <- 775
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
options(width=140)
set.seed(874) # reproducible

is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful
 
xwidth <- 50
xwidth2 <- 4

cola <- c("red", "pink", "green","blue","black","yellow","brown","maroon","purple")
cola <- colours()
cola <- c("pink1", "violet", "mediumpurple1", "slateblue1", "purple", "purple3",
                  "turquoise2", "skyblue", "steelblue", "blue2", "navyblue",
                  "orange", "tomato", "coral2", "palevioletred", "violetred", "red2",
                  "springgreen2", "yellowgreen", "palegreen4",
                  "wheat2", "tan", "tan2", "tan3", "brown",
                  "grey70", "grey50", "grey30")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2 
                # paper
                
                
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"),
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Visualising prior probability density functions commonly used for Bayesian logistic regression coefficients"),
                
                h4("A recommended prior for the coefficients is a Student-t distribution (bell-shaped but
                with fatter tails than a normal distribution) with 3 degrees of freedom, 
                a mean of 0, and scale of 2.5 (coloured violet below) [1]. 
                This prior roughly expresses an expectation that most values will be between -10 and +10. 
                However, a change from -5 to 0 corresponds to a change on a probability scale of .01 to .50. Situations 
                where a shift in input x corresponds to the probability of outcome y changing from 0.01 to 0.99 are rarely encountered [2].
                Here we plot the Normal, Cauchy and Student-t distributions. Student's t-distribution and the Cauchy distribution 
                are identical when the t-distribution degrees of freedom is equal to one and the t-distribution SD
                and Cauchy scale are identical. 
                The Student-t converges 
                to the normal distribution as the degrees of freedom go to infinity.
              "), 
                
                h3("  "), 
                # shinyUI(pageWithSidebar(
                #     titlePanel("Hello Shiny!"),
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  #wellPanel(style = "background: #2171B5",),
                                  h4("Enter the parameters for any of the three distributions and choose a colour. Select the x-axis range for the plot."),
                                  
                              #    actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                               #                onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/responder-non-responder-fallacy-in-RCTs/master/app.R', '_blank')"),    
                                  #actionButton("resample", "Simulate a new sample"),
                                  br(), # br(),
                                  tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                             
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      sliderInput("range",
                                                  div(h3("Select x axis range")),
                                                  min=-20, max=20, step=1, value=c(-10,10), ticks=FALSE),
                                      ##
                                      #div(h5("Enter Normal mean and standard deviation")),
                                      fluidRow(
                                          box(width = 13, title =  "Normal mean and standard deviation", 
                                              splitLayout(
                                                  textInput("m1", div(h5("Mean")), value="0", width=100),
                                                  textInput("s1", div(h5("SD")),value="1", width=100),
                                                  selectInput("col1", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      fluidRow(
                                          box(width = 13,# title = "Enter Normal mean and standard deviation", 
                                              splitLayout(
                                                  textInput("m2", div(h5("Mean")), value=NA, width=100),
                                                  textInput("s2", div(h5("SD")),value=NA, width=100),
                                                  selectInput("col2", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      
                                      fluidRow(
                                          box(width = 13, #title = "Normal mean and standard deviation", 
                                              splitLayout(
                                                  textInput("m3", div(h5("Mean")), value=NA, width=100),
                                                  textInput("s3", div(h5("SD")),value=NA, width=100),
                                                  selectInput("col3", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      #########################################################################
                                      fluidRow(
                                          box(width = 13, title = "Cauchy location and scale", 
                                              splitLayout(
                                                  textInput("cm1", div(h5("Location")), value="0", width=100),
                                                  textInput("cs1", div(h5("Scale")),value="1", width=100),
                                                  selectInput("col4", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      fluidRow(
                                          box(width = 13,# title = "Enter Normal mean and standard deviation", 
                                              splitLayout(
                                                  textInput("cm2", div(h5("Location")), value="0", width=100),
                                                  textInput("cs2", div(h5("Scale")),value="2.5", width=100),
                                                  selectInput("col5", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      
                                      fluidRow(
                                          box(width = 13, #title = "Enter Normal mean and standard deviation", 
                                              splitLayout(
                                                  textInput("cm3", div(h5("Location")), value=NA, width=100),
                                                  textInput("cs3", div(h5("Scale")),value=NA, width=100),
                                                  selectInput("col6", div(h5("Colour")),  sample(cola), width=120 )
                                              )
                                          )
                                      ),
                                      
                                      #########################################################################
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  fluidRow(
                                      box(width = 13, title = "t distribution df, location and scale", 
                                          splitLayout(
                                              textInput("t1a", div(h5("df")), value="3", width=100),
                                              textInput("t1b", div(h5("Location")), value="0", width=100),
                                              textInput("t1c", div(h5("Scale")),value="2.5", width=100),
                                              selectInput("col7", div(h5("Colour")),  sample(cola), width=120 )
                                          )
                                      )
                                  ),
                                  
                                  fluidRow(
                                      box(width = 13,# title = "Enter Normal mean and standard deviation", 
                                          splitLayout(
                                              textInput("t2a", div(h5("df")), value=NA, width=100),
                                              textInput("t2b", div(h5("Location")), value=NA, width=100),
                                              textInput("t2c", div(h5("Scale")),value=NA, width=100),
                                              selectInput("col8", div(h5("Colour")),  sample(cola), width=120 )
                                          )
                                      )
                                  ),
                                  
                                  
                                  fluidRow(
                                      box(width = 13, #title = "Enter Normal mean and standard deviation", 
                                          splitLayout(
                                              textInput("t3a", div(h5("df")), value=NA, width=100),
                                              textInput("t3b", div(h5("Location")), value=NA, width=100),
                                              textInput("t3c", div(h5("Scale")),value=NA, width=100),
                                              selectInput("col9", div(h5("Colour")),  sample(cola), width=120 )
                                          )
                                      )
                                  ),
                                  
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/logistic_regression_priors/master/app.R', '_blank')"),    
                                  #actionButton("resample", "Simulate a new sample"),
                                  br(), # br(),
                       
                        
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations", "[1] Prior choice recommendations"),
                                      div(p(" ")),
                                      tags$a(href = "https://projecteuclid.org/download/pdfview_1/euclid.aoas/1231424214", "[2]  A WEAKLY INFORMATIVE DEFAULT PRIOR DISTRIBUTION FOR
LOGISTIC AND OTHER REGRESSION MODELS"),
                                      div(p(" ")),
                                      tags$a(href = "https://statdist.ksmzn.com/", "[3] Nice app"),
                                      div(p(" ")),
                                     
                                  tags$a(href = "https://en.wikipedia.org/wiki/Normal_distribution/", "[4] Normal distribution"),
                                  div(p(" ")),
                                  tags$a(href = " https://en.wikipedia.org/wiki/Cauchy_distribution", "[5] Cauchy distribution"),
                                  div(p(" ")),
                                  tags$a(href = "https://en.wikipedia.org/wiki/Student%27s_t-distribution", "[6] Student t distribution"),
                                  div(p(" ")),
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                 
                                  
                                  
                                  )
                    
                    
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                              
                                            h4(" "), 
                                           div(plotOutput("plot", width=fig.width3, height=fig.height3)),
                                           h4("Figure 1 Probability density functions on the log odds scale")#,
                                  
                    )
                    
                ) #
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
) 

server <- shinyServer(function(input, output   ) {
    
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
 
    output$plot <- renderPlot({  
 
        m1 <- as.numeric(input$m1 )
        s1 <- as.numeric(input$s1)
        m2 <- as.numeric(input$m2)
        s2 <- as.numeric(input$s2)
        m3 <- as.numeric(input$m3)
        s3 <- as.numeric(input$s3)
        cm1 <-as.numeric(input$cm1)
        cs1 <- as.numeric(input$cs1)
        cm2 <- as.numeric(input$cm2)
        cs2 <- as.numeric(input$cs2)
        cm3 <- as.numeric(input$cm3)
        cs3 <- as.numeric(input$cs3)
        t1a <- as.numeric(input$t1a)
        t1b <- as.numeric(input$t1b)
        t1c <- as.numeric(input$t1c )
        t2a <- as.numeric(input$t2a)
        t2b <- as.numeric(input$t2b)
        t2c <- as.numeric(input$t2c)
        t3a <- as.numeric(input$t3a)
        t3b <- as.numeric(input$t3b)
        t3c <- as.numeric(input$t3c )
         
     
      
        
        x1 <- input$range[1]   
        x2 <- input$range[2]

        xs <- seq(x1,x2, by=2)
        
        xs2 <- c(-5,-4,-3,-2,-1,0)
        prob <- print(exp(xs2)/(1+exp(xs2)), digits=2)
        prob <- round(prob,2)
         
        x_values <- seq(x1,x2, length.out = 999)
        siz <- 1.
        
        
            
            data.frame(x_values) %>%
                ggplot(aes(x_values) ) +   
                stat_function(fun = dnorm,   args=list(mean=m1        ,sd=s1, log = FALSE), aes(colour = "a"),   size=siz) + 
                stat_function(fun = dnorm,   args=list(mean=m2        ,sd=s2, log = FALSE), aes(colour = "b"),   size=siz) +  
                stat_function(fun = dnorm,   args=list(mean=m3        ,sd=s3, log = FALSE), aes(colour = "cc"),  size=siz) + 
                stat_function(fun = dcauchy, args=list(location = cm1, scale = cs1),        aes(colour = "d"),   size=siz) + 
                stat_function(fun = dcauchy, args=list(location = cm2, scale = cs2),        aes(colour = "e"),   size=siz) + 
                stat_function(fun = dcauchy, args=list(location = cm3, scale = cs3),         aes(colour = "f"),  size=siz) + 
                stat_function(fun = dst,     args=list(nu=t1a,mu=t1b     ,sigma=t1c),        aes(colour = "g"),  size=siz) + 
                stat_function(fun = dst,     args=list(nu=t2a,mu=t2b     ,sigma=t2c),        aes(colour = "h"),  size=siz) + 
                stat_function(fun = dst,     args=list(nu=t3a,mu=t3b     ,sigma=t3c),        aes(colour = "j"),  size=siz) + 
                
                scale_colour_manual("", values = c(input$col1, input$col2, input$col3, 
                                                   input$col4, input$col5, input$col6,
                                                   input$col7, input$col8, input$col9))  +
                labs(title=paste0(c("Note probabilites", prob,"map to log odds: -5,-4,-3,-2,-1 and 0 for a distribution centered on 0 log odds"), collapse=", "), 
                     x = "log odds",
                     y = "",
                     #subtitle =paste0(c("Note probabilites", prob," are equivalent to log odds: -4,-2, 0 ,2, 4 "), collapse=", "),
                     caption = "") +
                guides(fill=FALSE) +
                theme_bw() +
              #  theme(legend.justification=c(1,0), legend.position=c(.96,.6)) +
                scale_x_continuous("log odds", breaks=xs, labels=xs, limits=c(x1,x2)) +
                theme(legend.position="none") +
                theme(#panel.background=element_blank(),
                    # axis.text.y=element_blank(),
                    # axis.ticks.y=element_blank(),
                    # https://stackoverflow.com/questions/46482846/ggplot2-x-axis-extreme-right-tick-label-clipped-after-insetting-legend
                    # stop axis being clipped
                    plot.title=element_text(size = 18), plot.margin = unit(c(5.5,12,5.5,5.5), "pt"),
                    legend.text=element_text(size=14),
                    legend.title=element_text(size=14),
                    legend.position="none",
                    axis.text.x  = element_text(size=15),
                    axis.text.y  = element_text(size=15),
                    axis.line.x = element_line(color="black"),
                    axis.line.y = element_line(color="black"),
                    plot.caption=element_text(hjust = 0, size = 7),
                    strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                    axis.title.y = element_text(size = rel(1.5), angle = 90),
                    axis.title.x = element_text(size = rel(1.5), angle = 0),
                    panel.grid.major.x = element_line(color = "grey80", linetype="dotted", size = 1),
                    panel.grid.major.y = element_line(color = "grey80", linetype="dotted", size = 1),
                    strip.background = element_rect(colour = "black", fill = "#ececf0"),
                    panel.background = element_rect(fill = '#ececf0', colour = '#ececf0'),
                    plot.background = element_rect(fill = '#ececf0', colour = '#ececf0')
                )
            
 
       
    })
   
  
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
})

# Run the application 
shinyApp(ui = ui, server = server) 