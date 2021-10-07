library(dplyr)
here::i_am("analysis/AnalyzeOptData.R")

btOut <- readRDS(file=here::here("data", "LongBoTorchOut.rds"))
btOut <- readRDS(file=here::here("data", "BoTorchOut151116416.rds"))
train_x <- btOut[[1]]
train_obj <- btOut[[2]]
traces <- btOut[[3]]

# Figure out the ranges across all optimizations you want to show
# Needed for plotting
showOpt <- 1:6
budg1 <- 1; budg2 <- 2
range1 <- range2 <- c(0.4, 0.4)
for (wo in showOpt){
  budg <- train_x[[wo]][[1]]
  nr <- c(floor(min(budg[,budg1])*50-0.5)/50,
            ceiling(max(budg[,budg1])*50+0.5)/50)
  range1 <- range(c(range1, nr))
  nr <- c(floor(min(budg[,budg2])*50-0.5)/50,
          ceiling(max(budg[,budg2])*50+0.5)/50)
  range2 <- range(c(range2, nr))
}

# Figure out how to recombobulate all this
# I want to see, in batches of 100 sim, how the optimum moves
# I think for now, I will be using LOESS

plotUpto <- function(whichOpt, nSim, degree=1){
  budg <- train_x[[whichOpt]][[1]]
  budg <- cbind(budg, 1 - rowSums(budg))
  colnames(budg) <- c("percPIC", "percSDN", "percCET", "percPYT")
  gain <- train_obj[[whichOpt]][[1]]
  colnames(gain) <- "response"
  resultMat <- dplyr::as_tibble(cbind(budg, gain))

  plotLoessPred_(resultMat, nSim, xlim=range1, ylim=range2,
                 plotMn=F, plotHiMn=F, plotHiPredGain=T,
                 ptCol=c(0,0,whichOpt+1), giveRange=F,
                 degree=degree)
}

for (whichOpt in 1:6){
  for (nSim in 36 + 0:30 * 50){
    plotUpto(whichOpt=whichOpt, nSim=nSim)
  }
}

# Repeats of the shorter optimization number of intervals

btOut <- readRDS(file=here::here("data", "ShrtBoTorchOut.rds"))
train_x <- btOut[[1]]
train_obj <- btOut[[2]]
traces <- btOut[[3]]

# Figure out how to recombobulate all this
# I want to see, in batches of 100 sim, how the optimum moves
# I think for now, I will be using LOESS

getLoessForOpt <- function(whichOpt){
  budg <- train_x[[whichOpt]][[1]]
  budg <- cbind(budg, 1 - rowSums(budg))
  colnames(budg) <- c("percPIC", "percSDN", "percCET", "percPYT")
  gain <- train_obj[[whichOpt]][[1]]
  colnames(gain) <- "response"
  resultMat <- dplyr::as_tibble(cbind(budg, gain))

  BreedSimCost::loessPredCount(resultMat)
}

allLoPred <- lapply(1:length(train_x), getLoessForOpt)

allHiPred <- lapply(allLoPred, function(lpo) return(lpo$percHiPredGain))
ahpMat <- matrix(unlist(allHiPred), ncol=4, byrow=T)
colnames(ahpMat) <- c("percPIC", "percSDN", "percCET", "percPYT")
indyVar <- tibble(founderPop=rep(1:6, each=2), optRepeat=rep(1:2, 6))
optEnds <- bind_cols(indyVar, as_tibble(ahpMat))
optEnds <- optEnds %>% dplyr::mutate(founderPop=as.factor(founderPop),
                                     optRepeat=as.factor(optRepeat))
optMod <- lm(percPIC ~ founderPop, data=optEnds)
summary(optMod)
anova(optMod)
optMod <- lm(percSDN ~ founderPop, data=optEnds)
summary(optMod)
anova(optMod)
optMod <- lm(percCET ~ founderPop, data=optEnds)
summary(optMod)
anova(optMod)
optMod <- lm(percPYT ~ founderPop, data=optEnds)
summary(optMod)
anova(optMod)

# Something I would like to do
# 1. Interleave the results from each BO so that iterations from different BOs
# are next to each other
# 2. Over some interval, run the predLOESS and determine where the maximum
# predicted response is and how it moves over time.  I suppose I could use the
# multivariate normal to also try to predict the maximum.  I would need to be
# able to extract the parameters of the Matern or Gaussian kernel from BoTorch
# which I don't know how to do.
# 3. It would be pretty cool to do this separately for each optimization to see
# if they converge (fingers crossed!)
