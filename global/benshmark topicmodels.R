library(microbenchmark)
library(tmca.unsupervised)
load(file="/home/christian/R/reuters3000speedtestset.rdata")


#Number Of Rund to avarage on
runs = 5

#Create the data frame
header = list(expr = 0., min = 0., lq = 0., mean = 0., median = 0., uq = 0., max = 0. , neval = 0.)
evaluation = data.frame(header)
attr(evaluation, "Units") = "seconds"


# Speed test every registered algorithm.
for (funcname in names(tmca.unsupervised::register)){
  t <- tmodel$new(method = funcname)
  t$.__enclos_env__$private$silent = T
  t$input_preprocessed(dtm = reuters3000speedtestset)
  ev = microbenchmark::microbenchmark(t$create_tm(),times = runs, unit = "s")
  evaluation[nrow(evaluation)+1,] = summary( ev)[1,]
  rownames(evaluation)[nrow(evaluation)] = funcname
}

cat("Speedtest Topicmodelling Algorithms\n")
cat("Number Of Documents:\t", dim(reuters3000speedtestset)[1], "\n")
cat("Number of Words:\t", dim(reuters3000speedtestset)[2], "\n")
cat("Units:\tSeconds\n")
print(evaluation[-1,])
