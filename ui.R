    library(dplyr)
    library(httr)
    library(ggplot2)
    library(colorspace)
    library(Rcpp)
    library(shiny)
    suppressPackageStartupMessages(library(deSolve))
    
    
    
    # Define UI ----
    ui <- fluidPage(
        HTML("<div><script>
       $( document ).on('shiny:sessioninitialized', function(event) {
           (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
})(window,document,'script','dataLayer','GTM-WWXRMFP');
       });</script></div>"),
        tabsetPanel( id = "tabset",
            
        
            tabPanel("Covid Simulation",
        
        titlePanel(h1("Covid-19 Simulation", align = "center", style = "margin-bottom: 5%; color: white"), windowTitle = "Covid-19 Simulation"),
       
        sidebarLayout(
            sidebarPanel(id = "sidebar", style = "background-color: black;", 
                conditionalPanel("(input.city) == 'Innsbruck'", sliderInput(inputId = "ppl",label =  "Configure Population Size", min = 5000, max = 1e7, step = 10e3, value = 310297, animate = TRUE, post = " people", width = "100%")),
                conditionalPanel("(input.city) == 'Wien'", sliderInput(inputId = "ppl",label =  "Configure Population Size", min = 5000, max = 1e7, step = 10e3, value = 1889000, animate = TRUE, post = " people", width = "100%")),
                conditionalPanel("(input.city) == 'New York'", sliderInput(inputId = "ppl",label =  "Configure Population Size", min = 5000, max = 1e7, step = 10e3, value = 8623000, animate = TRUE, post = " people", width = "100%")),
                conditionalPanel("(input.city) == false", sliderInput(inputId = "ppl2",label =  "Configure Population Size", min = 5000, max = 1e7, step = 10e3, value = 5000, animate = TRUE, post = " people", width = "100%")),
               
                div(style = "font-size: 12pt;", checkboxGroupInput(label = "... or select city", inputId = "city", choices = c("Innsbruck", "Wien", "New York"))),
                
                numericInput(inputId = "infects", label =  "Infected population size in t0", min = 1, max = 100000, step = 1, value = 10, width = "100%"),
                
                sliderInput(inputId = "reduction", post = " %", label =  "Reduce contacts by ...", min = 0, max = 100, step = 1, value = 0, width = "100%"),
                div(style = "display: inline-block; font-size: 12pt", checkboxGroupInput(inputId = "addplot", label = "Plot against ...", selected = c("30 % reduction"), choices = c("20 % reduction", "30 % reduction","50 % reduction")))
                
                ), 
            mainPanel(plotOutput("map"))
        ),
        tags$style(
            HTML('
             * {
                background-color: black;
                color: white;
                font-family: "Ubuntu";
                font-size:12pt;
             }
             #addplot, #addplot2 {
                display: inline-block;
             }
             h1 {
             color: white;
             }
             #sidebar   > div > div > label {
                font-size: 12pt;
             }
             .active{
             color: black;
             }
             .nav-tabs>li>a  {
             border: white;
             margin-right: 0px;
             }
             .nav-tabs  {
             background-color: #00366C;
             }
             #tabset > li.active > a {
             color:black;
             background-color:  #79ABE2;
             border-bottom: 1px solid white;
             font-style: oblique;
             font-weight: bold;
             }
             #tabset > li > a {
             color: white;
             background-color:  #00366C;
             border-bottom: 1px;
             border-bottom-width: 1px;
             border-bottom-style: initial;
             border-bottom-color: initial;
             }
            ')
        )
        ),
       
        tabPanel("About",
            br(),
            div(class = "about", p("The dynamics of the infectious covid-19 desease are descriped using the SIR model by Kermack and McKendrick (1927) and the following very basic ODE: ")),
            imageOutput("myImage", width = "100%", height = "120px",),
            br(),
            br(),
            br(),
            p("The model implies that each individual of the population N belongs to one of the three states so S(t)+I(t)+R(t)=N "),
            p("β = ap, where a is the probability of meeting a specific person an p the proportion of these contacts resulting in an infection. γ represents the time an individual is infectous which is set to 0.2 (5 days)"),
            p("The Basic Reproduction Number (R0) of this simulation without any interventions (reduction of contacts) is automatically set and adjusted to remain at 2.5, no matter what size N has. In Austria R0 was around 4.5 at the beginning of March, so a R0 of 2.5 is conservatively estimated "),
            p("Finally, the ODE is solved with the Runge-Kutta method"),
            
            )
            ),
        br(),
        br(),
        br(),
        br(),
        p("© 2020 — Toni Wahrstätter")
    )
    
    
    
    
    