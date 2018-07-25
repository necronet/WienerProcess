# First, we simulate continuous trait evolution by adding in
# each iteration a random number from a normal distribution
# with mean equal to 0 and standard deviation equal to 1. We
# simulate a total of 4 processes, to obtain at first two
# species and a specieation event at the middle of the
# simulation, obtaining a total of 3 species at the end.



#from,to,initial_value_y
generate_random_process <- function(from, to, initial_y_value = 0) {
  df <- data.frame(Y = initial_y_value, X = 0)
  y <- initial_y_value
  for (g in from:to) {
    df[g - initial_y_value, 2] <- g
    df[g - initial_y_value, 1] <- y
    y <- y + rnorm(1, 0, 1)
  }
  return (df)
}

df1 <- generate_random_process(1, 750)
df2 <- generate_random_process(1, 1500)
#DF 3 will start when df1 ended
df3 <- generate_random_process(750, 1500, df1[750, 1])
df4 <- generate_random_process(750, 1500, df1[750, 1])

plot(
  0,
  0,
  ylim = c(-100, 100),
  xlim = c(0, 1500),
  cex = 0,
  xlab = "",
  ylab = ""
)
# Generate graph
showMotion <- function() {
  
  title(main = "Brownian model generation", xlab = "Generation index", ylab = "Trait value")
  lines(df2$X, df2$Y, col = "blue")
  lines(df1$X, df1$Y, col = "red")
  lines(df3$X, df3$Y, col = "green")
  lines(df4$X, df4$Y, col = "orange")   
}
# Function to display the motion graph 
#showMotion()


# Now, we have to plot each simmulation lapse and store them in our computer.  
# I added some code to make lighter the gif (plotting just odd generations) and   
# to add a label at the speciation time. Note that, since Brownian Model is a   
# stocasthic process, my simulation will be different from yours.  
# You should adjust labels or repeat the simulation process if you don't   
# like the shape of your plot.  

parp <- rep(0:1, times=7, each= 15)  
parp<- c(parp, rep(0, 1290))


speciation_event_pnt = 750

for (q in seq(1,1500,10)) {
    id <- sprintf("%04d", q)
    png(paste("bm",id,".png", sep=""), width=900, height=570, units="px", pointsize=18)  
    par(omd = c(.05, 1, .05, 1))  
    
    plot(0, 0, ylim=c(-70,70), xlim=c(0,1500), cex=0,   
         main=paste("Brownian motion model \n generation=", q) ,   
         xlab="generations", ylab="trait value", font.lab=2, cex.lab=1.5 )
    lines(df2$X[1:q],df2$Y[1:q], col="blue", lwd=1)  
    lines(df1$X[1:q],df1$Y[1:q], col="red", lwd=1)    
    lines(df3$X[1:q],df3$Y[1:q], col="green", lwd=1)
    lines(df4$X[1:q],df4$Y[1:q], col="orange", lwd=1)
    
    if (parp[q]==0 && q > speciation_event_pnt) {
      text(920, 70,labels="Speciation event", cex= 1, col="black", font=1)  
      abline(v = 750, col="red", lwd=1, lty=2)
    }
    dev.off()  
    
}  

system("convert -delay 10 *.png bm.gif ")