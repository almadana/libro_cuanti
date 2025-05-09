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
par(mfrow = c(1, 1))

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
par(mfrow = c(1, 1))


## fig-IQdist -----------
#navIQdist ----

estImg <- list()
emphCol <- rgb(0,0,1)
emphColLight <- rgb(.5,.5,1)
emphGrey <- grey(.5)

colour <- TRUE

width <- 4.5
height <- 4.5


#panels

par(mfrow = c(1, 3))  # 2 filas, 1 columna


# plot
x <- 60:140
y <- dnorm(x,100,15)
plot(x,y,lwd=3,type="l",col=ifelse(colour,emphCol,"black"),
     xlab="Puntuaciones de CI", ylab="Densidad de probabilidad",frame.plot=FALSE,main="a)"
)


# function to do all the work
plotSamples <- function( n,main=NULL ) {

  IQ <- rnorm(n, 100, 15)
  hist( IQ, breaks=seq(10,180,5), border="white",
        col=ifelse(colour,emphColLight,emphGrey),
        xlab="Puntuaciones de CI", ylab="Frecuencia", xlim=c(60,140),
        main=main,
  )
  print( paste( "n=",n,"media=",mean(IQ), "desvío=",sd(IQ) ) )
}

# plot two different sample sizes
plotSamples(100,main="b)")
plotSamples(10000,main="c)")
makePng( paste0(directory, "navIQ_es.png"), 8, 3 )
par(mfrow = c(1, 1))

## sampdistsd -------------
plotSD <- function( n, N) {

  IQ <- matrix(rnorm(n*N, 100,15),N,n)

  sd2 <- function(x) { sqrt(mean((x-mean(x))^2)) }

  IQ <- apply(IQ,1,sd2)
  hist(IQ)
  print(mean(IQ))

  hist( IQ, breaks=seq(0,60,2), border="white", freq=FALSE,
        col=ifelse(colour,emphColLight,emphGrey),
        xlab="Desvío estándar muestral", ylab="", xlim=c(0,60),
        main="", axes=FALSE,
        font.main=1, ylim=c(0,.075)
  )
  axis(1)
  abline(v=15,lwd=2,lty=2)
  text(y = .0825, x=15, labels = "Desvío estándar poblacional", xpd=1)
}


# plot two different sample sizes
fileName <- "sampdistsd_es.png"
plotSD(2,100000)
#lines(x,y,lwd=2,col="black",type="l")
makePng( paste0(directory, paste0("estimation/",fileName)), width*1.5, height*1.5 )

## - fig-estimatorbiasA --------


estimatorBehaviour <- function() {

  # plots histograms of IQ samples

  # needed for printing
  width <- 6
  height <- 6

  # generate samples
  n <- 10000
  X <- matrix( rnorm(n*10,100,15), nrow=n, ncol=10 )

  # calculate sample means
  M <- matrix( NA, n, 10)
  for( i in 1:10 ) {
    M[,i] <- rowMeans(X[,1:i,drop=FALSE])
  }

  # calculate sample standard deviation
  S <- matrix( 0, n, 10)
  for( i in 2:10 ) {
    S[,i] <- apply(X[,1:i],1,sd) * sqrt((i-1) / i)
  }

  # plot the means
  plot(1:10, colMeans(M), type="b", ylim=c(95,105),
       xlab="Tamaño de la muestra", ylab="Media muestral promedio",
       col=ifelse(colour,emphCol,emphGrey),
       pch=19, lwd=3
  )
  abline( h = 100, lty=2, lwd=3 )

  fileName <- "biasMean_es.png"
  if( png ) makePng( paste0(directory, paste0("estimation/",fileName)), width, height )

  # plot the variances
  plot(1:10, colMeans(S), type="b", ylim=c(0,16),
       xlab="Tamaño de la muestra", ylab="Desvío estándar muestral promedio",
       col=ifelse(colour,emphCol,emphGrey),
       pch=19, lwd=3
  )
  abline( h = 15, lty=2, lwd=3 )

  fileName <- "biasSD_es.png"
  if( png ) makePng( paste0(directory, paste0("estimation/",fileName)), width, height )


}

estimatorBehaviour()

