# Source this to retrieve stored optimization iterations
if (!exists("init_num")) init_num <- 1
print(paste("Get optimization iterations from Initialization", init_num))

initDat <- readRDS(paste0("data/BoTorchOut", init_num, ".rds"))
budgets <- initDat[[1]]
gains <- initDat[[2]]
budgets <- as.matrix(budgets)
budgets <- budgets[,-4]
