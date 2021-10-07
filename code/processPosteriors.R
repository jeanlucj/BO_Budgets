# Process the outputs from getPosteriors.py

posteriors <- list(bestBudget=bestBudget, maxPredGain=maxPredGain,
                   postVarAtMax=postVarAtMax)

if (exists("predGains")){
  posteriors <- c(posteriors, 
                  list(predBudgets=predBudgets, predGains=predGains,
                       predVars=predVars)
                  )
}

saveRDS(posteriors, file=paste0("posteriors", init_num, ".rds"))
