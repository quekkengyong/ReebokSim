#this is a script with multiple simulation functions to derive values for the Reebok case
#functions starting with "iteration" are the individual iterations
#functions starting with "simulation" are simulation functions

#param runs is for the next function, simulate()
iteration<-function(runs,brady,law,brown,vinat,brusc,smith,blanks){
  #generate demand numbers
  D.Brady<-floor(max(rnorm(1,mean=30763,sd=13843),0))
  D.Law<-floor(max(rnorm(1,mean=10569,sd=4756),0))
  D.Brown<-floor(max(rnorm(1,mean=8159,sd=3671),0))
  D.Vinat<-floor(max(rnorm(1,mean=7270,sd=4362),0))
  D.Brusc<-floor(max(rnorm(1,mean=5526,sd=3316),0))
  D.Smith<-floor(max(rnorm(1,mean=2118,sd=1271),0))
  D.Blanks<-floor(max(rnorm(1,mean=23275,sd=10474),0))
  #Sales Figures, BtoD is Blanks converted to Dressed
  S.Dressed<-min(brady,D.Brady)+min(law,D.Law)+min(brown,D.Brown)+min(vinat,D.Vinat)+min(brusc,D.Brusc)+min(smith,D.Smith)
  S.Blanks<-min(blanks,D.Blanks)
  S.BtoD<-min((blanks-D.Blanks),(D.Brady+D.Law+D.Brown+D.Vinat+D.Brusc+D.Smith-S.Dressed))
  #Leftover
  L.Dressed<-brady+law+brown+vinat+brusc+smith-S.Dressed
  L.Blanks<-blanks-S.Blanks-S.BtoD
  #Profit Calc
  Profit<-13.10*S.Dressed+12.1*S.Blanks+12.1*S.BtoD-3.9*L.Dressed-1.04*L.Blanks
  return(Profit)
}

#Simulation Function. Iters= number of iterations
#Returns the distribution of profits
simulate<-function(iters,brady,law,brown,vinat,brusc,smith,blanks){
  results <- sapply(1:iters,iteration,brady,law,brown,vinat,brusc,smith,blanks)
  avgprofit <- sum(results) / iters
  stdev<-sd(results)
  medn<-median(results)
  maxi<-max(results)
  mini<-min(results)
  print(paste("Average Profit:",avgprofit))
  print(paste("Standard Deviation:",stdev))
  print(paste("Median:",medn))
  print(paste("Maximum:",maxi))
  print(paste("Minimum:",mini))
}

#this function obtains all the reported profits over all iterations
simulateResults<-function(iters,brady,law,brown,vinat,brusc,smith,blanks){
  results <- sapply(1:iters,iteration,brady,law,brown,vinat,brusc,smith,blanks)
  return(results)
}

#this iteration returns Leftover Dressed
iterationLD<-function(runs,brady,law,brown,vinat,brusc,smith,blanks){
  #generate demand numbers
  D.Brady<-floor(max(rnorm(1,mean=30763,sd=13843),0))
  D.Law<-floor(max(rnorm(1,mean=10569,sd=4756),0))
  D.Brown<-floor(max(rnorm(1,mean=8159,sd=3671),0))
  D.Vinat<-floor(max(rnorm(1,mean=7270,sd=4362),0))
  D.Brusc<-floor(max(rnorm(1,mean=5526,sd=3316),0))
  D.Smith<-floor(max(rnorm(1,mean=2118,sd=1271),0))
  D.Blanks<-floor(max(rnorm(1,mean=23275,sd=10474),0))
  #Sales Figures, BtoD is Blanks converted to Dressed
  S.Dressed<-min(brady,D.Brady)+min(law,D.Law)+min(brown,D.Brown)+min(vinat,D.Vinat)+min(brusc,D.Brusc)+min(smith,D.Smith)
  S.Blanks<-min(blanks,D.Blanks)
  S.BtoD<-min((blanks-D.Blanks),(D.Brady+D.Law+D.Brown+D.Vinat+D.Brusc+D.Smith-S.Dressed))
  #Leftover
  L.Dressed<-brady+law+brown+vinat+brusc+smith-S.Dressed
  L.Blanks<-blanks-S.Blanks-S.BtoD
  #Profit Calc
  #Profit<-13.10*S.Dressed+12.1*S.Blanks+12.1*S.BtoD-3.9*L.Dressed-1.04*L.Blanks
  #return(Profit)
  return(L.Dressed)
}

#this iteration returns Leftover blanks
iterationLB<-function(runs,brady,law,brown,vinat,brusc,smith,blanks){
  #generate demand numbers
  D.Brady<-floor(max(rnorm(1,mean=30763,sd=13843),0))
  D.Law<-floor(max(rnorm(1,mean=10569,sd=4756),0))
  D.Brown<-floor(max(rnorm(1,mean=8159,sd=3671),0))
  D.Vinat<-floor(max(rnorm(1,mean=7270,sd=4362),0))
  D.Brusc<-floor(max(rnorm(1,mean=5526,sd=3316),0))
  D.Smith<-floor(max(rnorm(1,mean=2118,sd=1271),0))
  D.Blanks<-floor(max(rnorm(1,mean=23275,sd=10474),0))
  #Sales Figures, BtoD is Blanks converted to Dressed
  S.Dressed<-min(brady,D.Brady)+min(law,D.Law)+min(brown,D.Brown)+min(vinat,D.Vinat)+min(brusc,D.Brusc)+min(smith,D.Smith)
  S.Blanks<-min(blanks,D.Blanks)
  S.BtoD<-min((blanks-D.Blanks),(D.Brady+D.Law+D.Brown+D.Vinat+D.Brusc+D.Smith-S.Dressed))
  #Leftover
  L.Dressed<-brady+law+brown+vinat+brusc+smith-S.Dressed
  L.Blanks<-blanks-S.Blanks-S.BtoD
  #Profit Calc
  #Profit<-13.10*S.Dressed+12.1*S.Blanks+12.1*S.BtoD-3.9*L.Dressed-1.04*L.Blanks
  #return(Profit)
  return(L.Blanks)
}

#this simulation returns Leftover Dressed
simulateLD<-function(iters,brady,law,brown,vinat,brusc,smith,blanks){
  results <- sapply(1:iters,iterationLD,brady,law,brown,vinat,brusc,smith,blanks)
  avgLD <- sum(results) / iters
  print(paste("Avg Leftover Dressed:",avgLD))
}

#this simulation returns Leftover Blanks
simulateLB<-function(iters,brady,law,brown,vinat,brusc,smith,blanks){
  results <- sapply(1:iters,iterationLB,brady,law,brown,vinat,brusc,smith,blanks)
  avgLB <- sum(results) / iters
  print(paste("Avg Leftover Blanks:",avgLB))
}

#iteration for dressed stockout
iterationSO<-function(runs,brady,law,brown,vinat,brusc,smith,blanks){
  #generate demand numbers
  D.Brady<-floor(max(rnorm(1,mean=30763,sd=13843),0))
  D.Law<-floor(max(rnorm(1,mean=10569,sd=4756),0))
  D.Brown<-floor(max(rnorm(1,mean=8159,sd=3671),0))
  D.Vinat<-floor(max(rnorm(1,mean=7270,sd=4362),0))
  D.Brusc<-floor(max(rnorm(1,mean=5526,sd=3316),0))
  D.Smith<-floor(max(rnorm(1,mean=2118,sd=1271),0))
  D.Blanks<-floor(max(rnorm(1,mean=23275,sd=10474),0))
  #Sales Figures, BtoD is Blanks converted to Dressed
  S.Dressed<-min(brady,D.Brady)+min(law,D.Law)+min(brown,D.Brown)+min(vinat,D.Vinat)+min(brusc,D.Brusc)+min(smith,D.Smith)
  S.Blanks<-min(blanks,D.Blanks)
  S.BtoD<-min((blanks-D.Blanks),(D.Brady+D.Law+D.Brown+D.Vinat+D.Brusc+D.Smith-S.Dressed))
  #Leftover
  L.Dressed<-brady+law+brown+vinat+brusc+smith-S.Dressed
  L.Blanks<-blanks-S.Blanks-S.BtoD
  return(ifelse(L.Blanks==0,1,0))
}

#simulation for dressed stockout
simulateSO<-function(iters,brady,law,brown,vinat,brusc,smith,blanks){
  results <- sapply(1:iters,iterationSO,brady,law,brown,vinat,brusc,smith,blanks)
  avgSO <- sum(results) / iters
  print(paste("Blank Stockout Rate:",avgSO))
}
#code to be executed for each model:
#simple newsvendor model
#simulate(100000,41017,14092,10878,10501,7982,3059,38052)
#model 3
##m3<-simulateResults(100000,21817,7495,5786,4451,3383,1296,70555)
#model 2
##m2<-simulateResults(100000,19313,6635,5123,3662,2783,1067,87000)
#model 1
simulate(100000,24834,8532,6587,5402,4106,1574,63749)
##df<-cbind(m0,m1,m2,m3)
##boxplot(df,use.cols=T)
