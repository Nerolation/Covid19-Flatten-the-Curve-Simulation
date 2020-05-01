colors <- list(sequential_hcl(5, palette = "Blues 3"))
colors[[1]][1] <- "#005BB7"


# Define server logic ----
server <- function(input, output) {
  output$map <- renderPlot({ 
  

# Catch if multiple cities 

if(length(as.numeric(input$city)) > 1) {
  par(bg="black")
  plot(1)
  text(1, "Multiple cities selected", col = "white", cex = 2)
} else {

#Infected persons in t0
infects_t0 <- input$infects

#reduction of contact in %
red <- input$reduction/100 + 1
plots_ <- c(red)
#population size
if(!is.null(input$ppl2)) { ppl <- input$ppl2 }
city <- ""
#inputbox city
if(!is.null(input$city)) { city <- input$city 
  if(city == "Innsbruck") { ppl <- 310297 }
  if(city == "Wien") { ppl <- 1889000 }
  if(city == "New York") { ppl <- 8623000 }
}

#Additional plots
nr_ <- input$addplot

if ("20 % reduction" %in% nr_) { plots_ <- c(plots_, 1.20) }
if ("30 % reduction" %in% nr_) { plots_ <- c(plots_, 1.30) }
if ("50 % reduction" %in% nr_) { plots_ <- c(plots_, 1.50) }

#ODE System
sir <- function(t, y, parms) {
  beta <- parms[1]/i
  gamma <- parms[2]
  #beta <- ifelse(t<120, ifelse(t<70 && t> 10, beta/1.4, beta/1.4), beta)
  S <- y[1]
  I <- y[2]
  return(list(c(S = -beta * S * I, I = beta * S * I - gamma * I)))
}

# Population size 
N <- ppl
# Rate at which person stays in the infectious compartment (disease specific and tracing specific)
gamma <- 1/5

# Infectious contact rate - beta = R0/N*gamma and when R0 \approx 2.25 then  2.25/N*gamma
beta <- 2.5/N*gamma

R0 <- beta*N/gamma

# Grid where to evaluate
if(red > 1.5 || "50 % reduction" %in% nr_ ){ xax <- 200} else { xax <- 150 }
max_time <- xax
times <- seq(0, max_time, by=0.01)
# Solve ODE system using Runge-Kutta numerical method.
solutions <- data.frame(t = times)
for(i in plots_) {
  ode_solution <- rk4(y = c(N - infects_t0, infects_t0), times = times, func = sir, parms = c(beta, gamma)) %>%
    as.data.frame() %>%
    setNames(c("t", "S", "I")) %>%
    mutate(beta = beta, gama = gamma, R0 = N * beta / gamma, s = S / N, i = I / N, type = "without_intervention")
  
  solutions[paste(i)] <- ode_solution$I
  solutions[paste(i, "_s")] <- ode_solution$s
  solutions[paste(i, "S")] <- ode_solution$S
}

#BASIC PLOT 
par(bg = "black")

maxplot_y <- max(solutions[as.character(plots_[1])], solutions[as.character(plots_[2])])
if(maxplot_y < 5000) { maxplot_y <- 2000; seqq <- -2 } else { seqq <- -4}
if(maxplot_y > 5000 && maxplot_y < 100000) { seqq <- -3 } 

layout.matrix <- matrix(c(1, 2), nrow = 2, ncol = 1)
layout(mat = layout.matrix,
       heights = c(8, 1), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns

plot(1, axes = TRUE,  xlim = c(1,xax), ylim = c(1,maxplot_y + maxplot_y/3.5))

title(paste("Covid 19 Simulation \n", city, ifelse(city != "", "\n", ""), "with reducing contacts by", input$reduction,"%"), col.main = colors[[1]][4], col.lab = colors[[1]][4], xlab = "Days (t)", ylab = "Infected people in t" )
grid(col = colors[[1]][4], lty = "dotted")
yax <- round(seq(0,round(maxplot_y,6)+ maxplot_y/4, length.out = 10), seqq)
axis(side = 1, at = c(0, 30, 60, 90, 120, 150, ifelse(xax == 200, 180, ""), ifelse(xax == 200, 200, "")), col = colors[[1]][4], col.axis = colors[[1]][4])
axis(side = 2, at = yax, col = colors[[1]][4], col.axis = colors[[1]][4])
box(col = colors[[1]][4])
legend(xjust = 0.5, yjust = 0.7, x = ifelse(xax == 200, 120, 90), y = maxplot_y + 0.2*maxplot_y,  legend = c(paste(as.character(round((plots_-1)*100),0), " % reduction")), text.col=colors[[1]], col=colors[[1]], lty=1, cex=1,text.font = 2, bg = alpha("black", 0.5) )
title(sub="hallo", adj=1, line=3, font=2, col.main = "white", )
for(i in 1:length(plots_)) {
  lines(solutions[,paste(plots_[i])] ~ solutions$t, lwd = 5, col = colors[[1]][i])
  polygon(c(solutions$t[solutions$t >= 0], 60), c(solutions[,paste(plots_[i])][solutions$t >= 0], 0), col=alpha(colors[[1]][i], 0.3), border = NA)
  }

par(mar = c(0,0,0,0))
plot(1)
text(1.3, paste("~", as.character(round(1 - tail(solutions, n=1) %>% pull(paste(as.character(red),"_s")), 2)*100), "% Infection Rate - ", as.character(round(N -tail(solutions, n=1) %>% pull(paste(as.character(red),"S")), 0)), " persons in total"), col = colors[[1]][1], cex = 1)
if(length(plots_) > 1){
  text(1, paste("~",as.character(round(1 - tail(solutions, n=1) %>% pull(paste(as.character(plots_[2]),"_s")), 2)*100), "% Infection Rate - ", as.character(round(N - tail(solutions, n=1) %>% pull(paste(as.character(plots_[2]),"S")),0)), " persons in total"), col = colors[[1]][2], cex = 1)
}
if(length(plots_) > 2){
  text(0.7, paste("~", as.character(round(1 - tail(solutions, n=1) %>% pull(paste(as.character(plots_[3]),"_s")), 2)*100), "% Infection Rate - ", as.character(round(N -tail(solutions, n=1) %>% pull(paste(as.character(plots_[3]),"S")), 0)), " persons in total"), col = colors[[1]][3], cex = 1)
}

} # else 2 cities
  })
  output$myImage <- renderImage({
    # Return a list containing the filename
    list(src = "static/ODE.png",
         contentType = 'image/png',
         width = 250,
         height = 170,
         alt = "This is alternate text")
  }, deleteFile = FALSE)
}