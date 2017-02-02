mov_rev <- concat(sampfile,collapse= " ")
unilen <- length(get.ngrams(ngram(mov_rev,n=1)))
sampfile <- get.ngrams(ngram(mov_rev,n=3))
#sampfile1 <- moviefeat1((sampfile))
temp <- data.frame(positive_unigram_sort[,1])

for(i in 1:nrow(temp)){
  
  wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
  
  temp1 <- length(sampfile[grep(wd1,sampfile)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
  
  temp[i,2] <- max(temp1) # 
  
  
}
t <- paste(sum(temp[,2]))

moviedf <- data.frame(unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)
#                                moviedf[1,1] <- t #sum(temp[,2])  # /unilen

#classpred <- predict(model1,sampfile[,1:6])

############################ End code #################


#                                   movtxt <- concat(movtxt1,sep="")# This works
#                                   
#                                   sampfile <- get.ngrams(ngram(movtxt,n=6))# This works
#                                   t <- paste0(length(sampfile)) # IF paste0 is applied the length can be calculated
#                                   sampfile[grep("action",sampfile)]# This works
#                                   


#sampfile <- concat(movtxt,collapse = " ") # This will collapse it into one big paragraph
#sampfile <- read.table(movtxt)
#len <- strsplit(movtxt,'Yet')[1]
#sampfile <- strsplit(movtxt," ") # This will split it according to various sentences
#                                   sampfile <- cleansamp(sampfile[[1]]) # This cleans and again splits it into various sentenses
#sampfile <- moviefeat1(sampfile) # Outputs the fetures of this text
#                                   classpred <- predict(model1,sampfile[,1:6])

######### JMJPFU #########
# 12 Feb 2016 #########

devtools::install_github("rstudio/leaflet")
library(leaflet)


#### Creating a gauge plot ############

# function to create a circle
circle <- function(center=c(0,0), radius=1, npoints=100)
{
  r = radius
  tt = seq(0, 2*pi, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# function to get slices
slice2xy <- function(t, rad) 
{
  t2p = -1 * t * pi + 10*pi/8
  list(x = rad * cos(t2p), y = rad * sin(t2p))
}

# function to get major and minor tick marks

ticks <- function(center=c(0,0), from=0, to=2*pi, radius=0.9, npoints=5)
{
  r = radius
  tt = seq(from, to, length=npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

### Preparing elements

# external circle (this will be used for the black border)
border_cir = circle(c(0,0), radius=1, npoints = 100)

# gray border circle
external_cir = circle(c(0,0), radius=0.97, npoints = 100)

# yellow slice (this will be used for the yellow band)
yellowFrom = 75
yellowTo = 90
yel_ini = (yellowFrom/100) * (12/8)
yel_fin = (yellowTo/100) * (12/8)
Syel = slice2xy(seq.int(yel_ini, yel_fin, length.out = 30), rad=0.9)

# red slice (this will be used for the red band)
redFrom = 90
redTo = 100
red_ini = (redFrom/100) * (12/8)
red_fin = (redTo/100) * (12/8)
Sred = slice2xy(seq.int(red_ini, red_fin, length.out = 30), rad=0.9)

# white slice (this will be used to get the yellow and red bands)
whiteFrom = 74
whiteTo = 101
white_ini = (whiteFrom/100) * (12/8)
white_fin = (whiteTo/100) * (12/8)
Swhi = slice2xy(seq.int(white_ini, white_fin, length.out = 30), rad=0.8)

# coordinates of major ticks (will be plotted as arrows)
major_ticks_out = ticks(c(0,0), from=5*pi/4, to=-pi/4, radius=0.9, 5)
major_ticks_in = ticks(c(0,0), from=5*pi/4, to=-pi/4, radius=0.75, 5)

# coordinates of minor ticks (will be plotted as arrows)
tix1_out = ticks(c(0,0), from=5*pi/4, to=5*pi/4-3*pi/8, radius=0.9, 6)
tix2_out = ticks(c(0,0), from=7*pi/8, to=7*pi/8-3*pi/8, radius=0.9, 6)
tix3_out = ticks(c(0,0), from=4*pi/8, to=4*pi/8-3*pi/8, radius=0.9, 6)
tix4_out = ticks(c(0,0), from=pi/8, to=pi/8-3*pi/8, radius=0.9, 6)
tix1_in = ticks(c(0,0), from=5*pi/4, to=5*pi/4-3*pi/8, radius=0.85, 6)
tix2_in = ticks(c(0,0), from=7*pi/8, to=7*pi/8-3*pi/8, radius=0.85, 6)
tix3_in = ticks(c(0,0), from=4*pi/8, to=4*pi/8-3*pi/8, radius=0.85, 6)
tix4_in = ticks(c(0,0), from=pi/8, to=pi/8-3*pi/8, radius=0.85, 6)

# coordinates of min and max values (0, 100)
v0 = -1 * 0 * pi + 10*pi/8
z0x = 0.65 * cos(v0)
z0y = 0.65 * sin(v0)
v100 = -1 * 12/8 * pi + 10*pi/8
z100x = 0.65 * cos(v100)
z100y = 0.65 * sin(v100)

# indicated value, say 80 (you can choose another number between 0-100)
value = 80

# angle of needle pointing to the specified value
val = (value/100) * (12/8)
v = -1 * val * pi + 10*pi/8
# x-y coordinates of needle
val_x = 0.7 * cos(v)
val_y = 0.7 * sin(v)

# label to be displayed
label = "Movie Rating!"

## Putting everything together

# open plot
plot(border_cir$x, border_cir$y, type="n", asp=1, axes=FALSE,
     xlim=c(-1.05,1.05), ylim=c(-1.05,1.05),
     xlab="", ylab="")

# yellow slice
polygon(c(Syel$x, 0), c(Syel$y, 0),
        border = "#FF9900", col = "#FF9900", lty = NULL)

# red slice
polygon(c(Sred$x, 0), c(Sred$y, 0),
        border = "#DC3912", col = "#DC3912", lty = NULL)

# white slice
polygon(c(Swhi$x, 0), c(Swhi$y, 0),
        border = "white", col = "white", lty = NULL)

# add gray border
lines(external_cir$x, external_cir$y, col="gray85", lwd=20)

# add external border
lines(border_cir$x, border_cir$y, col="gray20", lwd=2)

# add minor ticks
arrows(x0=tix1_out$x, y0=tix1_out$y, x1=tix1_in$x, y1=tix1_in$y,
       length=0, lwd=2.5, col="gray55")
arrows(x0=tix2_out$x, y0=tix2_out$y, x1=tix2_in$x, y1=tix2_in$y,
       length=0, lwd=2.5, col="gray55")
arrows(x0=tix3_out$x, y0=tix3_out$y, x1=tix3_in$x, y1=tix3_in$y,
       length=0, lwd=2.5, col="gray55")
arrows(x0=tix4_out$x, y0=tix4_out$y, x1=tix4_in$x, y1=tix4_in$y,
       length=0, lwd=2.5, col="gray55")

# add major ticks
arrows(x0=major_ticks_out$x, y0=major_ticks_out$y,
       x1=major_ticks_in$x, y1=major_ticks_in$y, length=0, lwd=4)

# add value
text(0, -0.65, value, cex=4)

# add label of variable
text(0, 0.43, "Movie", cex=3.8)

# add needle
arrows(0, 0, val_x, val_y, col="#f38171", lwd=7)

# add central blue point
points(0, 0, col="#2e9ef3", pch=19, cex=5)

# add values 0 and 100
text(z0x, z0y, labels="0", col="gray50")
text(z100x, z100y, labels="5", col="gray50")

## Working with google charts

ggdata <- data.frame(label = "movie",rating=3)
gvisGauge(ggdata, labelvar = "movie", numvar = "rating", options = list(), chartid)

p <- gvisGauge(ggdata, labelvar = "movie", numvar = "rating",options=list(width=400,height=120,redFrom=1,redTo=3,yellowFrom=4,yellowTo=5,minorTicks=5))
print(p)
## Example of gvisGauge
library(dplyr)
library(plyr)

d <- ddply(iris, .(Species), colwise(mean))

p <- gvisGauge(d, chartid = "chart02")
print(p, tag = "chart")
plot(p)

Gauge1 <- gvisGauge(ggdata, options=list(min=0, max=5, greenFrom=4,
                                                 greenTo=5, yellowFrom=2, yellowTo=4,
                                                 redFrom=0, redTo=2))
plot(Gauge1)


########### Other Way of making the plot ##########

M0 <- matrix(c('Label','Value'),ncol=2,byrow=TRUE)
> M1 <- matrix(c('IRR',4),ncol=2,byrow=TRUE)
> MU <- rbind(M0,M1)
> df <- as.data.frame(MU)
> df
V1    V2
1 Label Value
2   IRR     4


library(shiny)
library(googleVis)
runApp(list(
  ui = bootstrapPage(
    numericInput('n', 'Number of obs', 4, 1, 10),
    htmlOutput("view")
  ),
  server = function(input, output) {
    output$view <- renderGvis({
      df <- data.frame(Label = "IRR", Value = input$n)
      gvisGauge(df,
                options=list(min=0, max=10, greenFrom=8,
                             greenTo=10, yellowFrom=6, yellowTo=8,
                             redFrom=0, redTo=6, width=300, height=300));
      
    })
  }
))