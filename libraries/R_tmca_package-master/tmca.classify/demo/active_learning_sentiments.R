# For parallelization: register backends
# if(.Platform$OS.type == "unix") {
#   require(doMC)
#   registerDoMC(8)
# } else {
#   require(doParallel)
#   workers <- makeCluster(4, type="SOCK")
#   registerDoParallel(workers)
# }

require("tmca.classify")

# Sentiment data example
# ----------------------

data("sentiments")

corpus <- sentiment_data$SENTENCE
gold_labels <- sentiment_data$LABEL

# Repeat active learning experiments
nRepeats <- 5
initial_training_size <- 50
last_iters <- NULL
for (i in 1:nRepeats) {
  print(i)
  print("-------------")
  # initialize classification object
  experiment <- tmca_classify(corpus = corpus, gold_labels = gold_labels)
  # set same data as validation set to test progress
  # of classification performance during iterations
  experiment$set_validation_AL_corpus()
  # run active learning
  experiment$create_initial_trainingset(initial_training_size)
  experiment$active_learning(stop_threshold = 0.99, positive_class = "Positive")
  experiment$plot_progress()
  last_iter <- tail(experiment$progress, 1)
  last_iter_val <- tail(experiment$progress_validation[, c("F", "kappa")], 1)
  last_iters <- rbind(last_iters, c(last_iter, last_iter_val))
}
View(last_iters)

# AL should stop after around 80 to 90 iterations and achive a kappa ~ 0.8
# Results w.r.t. initial_training_size:
# * small start set -> better kappa + more iterations
# * large start set -> worse/more instable kappa + less iterations


# Proportional classification
# --------------
final_sentiments <- experiment$classify()
portal <- sentiment_data$SOURCE
tmca_proportions(final_sentiments, gold_labels, facets = portal, positive_class = "Positive", verbose = T)

