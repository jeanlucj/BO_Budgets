
## ----Initialize program-----------------------------------------------
bsd <- initializeProgram("data/FounderCtrlFile.txt",
                         "data/SchemeCtrlFile.txt",
                         "data/CostsCtrlFile.txt",
                         "data/OptimizationCtrlFile.txt")

budget_constraints <- bsd$initBudget[c("minPICbudget", "minLastStgBudget")]
budget_constraints <- c(budget_constraints, bsd$initBudget[grep("ratio", names(bsd$initBudget))])
