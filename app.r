library("deSolve")
library("ggplot2")
library("rsconnect")
#library("shiny")
library("dplyr")

a <- function(t, ti, tf, r){
  if(t < ti){
    return(1)
  } else if(t <= tf){
    return(((1 - r)/(ti - tf))*(t - ti)+1)
  } else {
    return(r)
  }
}

f <- function(t, e){
  return(e * exp(t/3.0)) # ajustar exponencial
}

brusselator <- function(t, y, p) {
  with(as.list(c(y, p)), {
    alpha <- a(t, ti, tf, r)*b
    dS <- (-alpha/N)*S*I
    dI <- (alpha/N)*S*I + f(t, e) - beta*I - gamma*I
    dR <- gamma*I
    dO <- beta*I
    list(c(S=dS, I=dI, R=dR, O=dO))
  })
}

server <- function(input, output) {
  output$brussels <- renderPlot({
    t0 <- input$t1 + input$t2
    tr <- input$t1 + input$t3
    beta <- input$m * (1.0/t0)
    gamma <- (1-input$m)/tr
    I0 <- input$In0 / input$theta
    S0 <- input$N - I0
    parms <- c(ti=input$ti, tf=input$tf, r=input$r, e=input$e, b=input$b, beta=beta, gamma=gamma, N=input$N)
    resultado <- ode(y = c(S=S0, I=I0, R=0, O=input$O0), times=seq(0, 10, .1), brusselator, parms)[,c(1,3,4,5)]
    matplot.0D(resultado)
  })
}

ui <- fluidPage(
  titlePanel("Modelo de Predição do Número de Casos de COVID-19"),
  plotOutput("brussels"),
  numericInput("b", label = "b", value = 0.381), # taxa de infecção
  numericInput("theta", label = "theta", value = 0.0812), # percentual de casos notificados
  numericInput("r", label = "r", value = 0.58), # fator de redução da taxa de contato
  numericInput("ti", label = "ti", value = 7), # instnte inicial das políticas de contenção
  numericInput("tf", label = "tf", value = 29), # instnte final das políticas de contenção
  numericInput("c", label = "c", value = 0.0647), # porcentagem de casos graves
  numericInput("h", label = "h", value = 0.0683), # porcentagem de hospitalizados
  numericInput("m", label = "m", value = 0.0138), # taxa de mortalidade
  numericInput("t1", label = "t1", value = 13.8), # tempo de incubação
  numericInput("t2", label = "t2", value = 21.2), # tempo dos sintomas ao óbito
  numericInput("t3", label = "t3", value = 7.1), # tempo dos sintomas à recuperação
  numericInput("e", label = "e", value = 0.46*10^-6), # taxa de migração afetada pelas políticas de restrição
  numericInput("N", label = "N", value = 2.093*10^8), #população
  numericInput("In0", label = "In0", value = 2000), #casos notificados inicialmente
  numericInput("O0", label = "O0", value = 150) #mortes iniciais
)

shinyApp(ui=ui, server=server)
#runApp()