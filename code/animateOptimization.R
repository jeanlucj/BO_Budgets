##################################################################################################
# A script with animation examples using R
# by: Isaac J. Faber, examples derived from https://yihui.name/animation/examples/
# WARNING!!!! without dependcies installed you can only render to HTML
# Make sure you install FFmpeg (for videos) and Graphics Magick (for gifs) on the terminal (here in RStuido) first.
# Use these commands for FFmepg
# -----------------------------------------------
# sudo add-apt-repository ppa:jonathonf/ffmpeg-4
# sudo apt-get update
# sudo apt-get install ffmpeg
# -----------------------------------------------
# Use these commands for Graphics Magick
# -----------------------------------------------
# sudo apt-get install python-software-properties
# sudo apt-get install software-properties-common
# sudo add-apt-repository ppa:rwky/graphicsmagick
# sudo apt-get update
# sudo apt-get install graphicsmagick
#################################################################################################
if (FALSE){
# The package is written by Yihui with a companion website here: https://yihui.name/animation/
library(animation)

# Some helper libraries for data munging
library(tidyverse)

# check the animation options with
ani.options()

# set some of the options by specifiying the commands
ani.options(interval = 0.5,
            nmax = 200)

## The good animation as a simple GIF
saveGIF({
  for (whichOpt in 1:6){
    for (nSim in 36 + 0:30 * 50){
      plotUpto(whichOpt=whichOpt, nSim=nSim, degree=0)
    }
  }
},
#close the animation builder
movie.name = '~/optimizationDeg0.gif'
)

## The good animation as a simple GIF
saveGIF({
  end_year = 2017 #last year of the plot
  num_years = 30 #number of years in the animation
  #create a loop the does the subsetting
  for(i in 1:num_years){
    gdp_subset <- gdp_filtered %>% filter(year <= end_year-(num_years-i))
    #write the plot with a subset
    p<-ggplot(gdp_subset,aes(x=year,y=gdp,group=`Country Name`,colour=`Country Name`)) +
      geom_line(size = 2) +
      scale_x_continuous(breaks=c(1960,1980,2000,2017)) +
      ylim(0,(2*10^13))+
      xlab('') +
      ylab('Gross Domestic Product') +
      theme_bw() +
      theme(legend.title=element_blank())
    print(p)
  }#close the for loop
  #close the animation builder
}, convert = 'gm convert', movie.name = 'good_animation.gif')
}#END FALSE

# ptCol: what colors to have HiMn, HiCt, HiPredGain
# addPoints: Three column matrix with x, y, color
# addLegend: I think it's generally ugly so would want to remove
# Modifying this in a way that doesn't make sense for the general package
plotLoessPred_ <- function(resultMat, nSim=nrow(resultMat),
                          xlim=NULL, ylim=NULL,
                          budg1=1, budg2=2, binMeanContrast=3,
                          plotMn=T, plotHiMn=T, plotHiCt=F, plotHiPredGain=F,
                          ptCol=2:4, addPoints=NULL, addLegend=F, giveRange=T,
                          degree=1){
  require(hexbin)
  require(grid)
  lpc <- BreedSimCost::loessPredCount(resultMat=resultMat, nSim=nSim,
                                      xlim=xlim, ylim=ylim,
                                      budg1=budg1, budg2=budg2,
                                      degree=degree)

  rangeTitle <- " Simulations"
  if (giveRange) rangeTitle <- paste0(" Sims. Range: ", paste(round(range(lpc$binMean), 1), collapse=" to "))
  prefTitle <- paste0(rep(" ", 4 - ceiling(log10(nSim))), collapse="")
  main <- paste0(prefTitle, nSim, rangeTitle)
  if (plotMn){
    bmc <- binMeanContrast
    binRange <- diff(range(lpc$binMean))^bmc
    meanAsCount <- round(99*(lpc$binMean - min(lpc$binMean))^bmc / binRange) + 1
    lpc$bins@count <- meanAsCount
  }
  p <- hexbin::gplot.hexbin(lpc$bins, main=main,
                    legend=ifelse(addLegend & !plotMn, 1, 0))
  pushHexport(p$plot.vp)
  if (plotHiMn){
    grid::grid.points(lpc$hiMeanXY[1], lpc$hiMeanXY[2], gp=gpar(col=ptCol[1]),
                      pch=16, size = unit(1, "char"))
  }
  if (plotHiCt){
    grid::grid.points(lpc$hiCtXY[1], lpc$hiCtXY[2], gp=gpar(col=ptCol[2]),
                      pch=16, size = unit(1, "char"))
  }
  if (plotHiPredGain){
    grid::grid.points(lpc$hiPredXY[1], lpc$hiPredXY[2], gp=gpar(col=ptCol[3]),
                      pch=16, size = unit(1, "char"))
  }
  upViewport()
  return(lpc)
}#END plotLoessPred_
