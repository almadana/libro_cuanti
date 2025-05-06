directory <- "./imgs/navarro_img/"
source( "./src/helpers.R" )
probImg <- list()
emphCol <- rgb(0,0,1)
emphColLight <- rgb(.5,.5,1)
emphGrey <- grey(.5)

png <- TRUE
colour <- TRUE


# Capítulo 4 --------------

## fig-4FreqProb
set.seed(42)

frequentistProb <- function() {

  # needed for printing
  fileName <- "frequentistProb_es.png"
  width <- 12
  height <- 8

  def.par <- par(no.readonly = TRUE)
  layout( matrix(1:4,2,2) )
  for( i in 1:4 ) {

    X <- as.numeric( runif(1000) > .5 )
    X <- cumsum(X) / (1:1000)
    plot( 1:1000, X, type="l", ylim=c(.3,.7), col=ifelse(colour,emphCol,emphGrey),
          xlab = "Número de tiradas", ylab = "Proporción de Caras", lwd=3
    )
    abline(h=.5,lty=2,col=emphGrey,lwd=2)

  }
  par(def.par)#- reset to default

  # print
  if( png ) makePng( paste0(directory, paste0("probability/",fileName)), width, height )


}
frequentistProb()

## -- fig-4pantsprob ------

set.seed(42)
pantsDist <- function() {

  # plots the bar graph showing the "pants" probability distribution

  # needed for printing
  fileName <- "pantsDistribution_es.png"
  width <- 8
  height <- 6

  # key information
  probabilities <- c( .5, .3, .1, 0, .1)
  eventNames <- c( "Jean azul", "Jean gris", "Jean negro",
                   "Pant. traje", "Pant. deportivo" )

  # draw the plot
  barplot(
    height= probabilities,
    xlab = "Evento",
    ylab = "Probabilidad del evento",
    names.arg = eventNames,
    density = 10,
    col = ifelse(colour,emphCol,emphGrey)
  )

  # print
  if( png ) makePng( paste0(directory, paste0("probability/",fileName)), width, height )
}
pantsDist()


## fig-4binomial1 ------

set.seed(42)
binomPlot <- function( n,p, ... ) {

  # probabilities of each outcome
  out <- 0:n
  prob <- dbinom( x=out, size=n, prob=p )

  # plot
  plot(
    out, prob, type="h", lwd=3, ylab="Probabilidad",
    frame.plot=FALSE, col=ifelse(colour,emphCol,"black"), ...
  )

}
binomExamples <- function() {

  # plots the three examples of a binomial distribution

  # needed for printing
  width <- 8
  height <- 6

  # function to produce a styled binomial plot

  # skulls image...
  binomPlot( n=20, p=1/6, xlab="Número de cráneos observados" )
  if( png ) makePng( paste0(directory, paste0("probability/","binomSkulls20_es.png")), width, height )

  # coins image #1...
  binomPlot( n=20, p=1/2, xlab="Número de caras observadas" )
  if( png ) makePng( paste0(directory, paste0("probability/", "binomHeads20_es.png")), width, height )

  # coins image #2...
  binomPlot( n=100, p=1/2, xlab="Número de caras observadas" )
  if( png ) makePng( paste0(directory, paste0("probability/", "binomHeads100_es.png")), width, height )
}
binomExamples()

# fig-4binomial2---------
par(mfrow = c(2, 1))  # 2 filas, 1 columna
binomPlot( n=20, p=1/2, xlab="Número de caras observadas" ,main="a)")
binomPlot( n=100, p=1/2, xlab="Número de caras observadas",main="b)" )
makePng( paste0(directory, paste0("probability/", "Binomial2_es.png")), 8, 12 )

#fig-4normal --------
set.seed(42)
standardNormal <- function() {

  # plots the standard normal

  # needed for printing
  width <- 8
  height <- 6
  fileName <- "standardNormal_es.png"

  # draw the plot
  xval <- seq(-3,3,.01)
  yval <- dnorm( xval, 0, 1)
  plot( 	xval, yval, lwd=3, ylab="Densidad de probabilidad", xlab="Valor observado",
         frame.plot = FALSE, col=ifelse(colour,emphCol,"black"), type="l"
  )

  # print
  if( png ) makePng( paste0(directory, paste0("probability/",  fileName)), width, height )

}
standardNormal()
