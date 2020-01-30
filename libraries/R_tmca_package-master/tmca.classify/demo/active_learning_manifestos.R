require("tmca.classify")

# For parallelization: register backends
# if(.Platform$OS.type == "unix") {
#   require(doMC)
#   registerDoMC(8)
# } else {
#   require(doParallel)
#   workers <- makeCluster(4, type="SOCK")
#   registerDoParallel(workers)
# }

# Manifesto data example
# ----------------------
initial_training_size <- 200
current_class <- "411"

# read manifesto data
data("manifestos")

# prepare data for LDA features
pseudoc_length <- 25
n <- nrow(manifesto_data)
pseudo_docs_idx <- rep(1:ceiling(n / pseudoc_length), each = pseudoc_length, length.out = n)
manifesto_pseudo_docs <- aggregate(manifesto_data$content, by = list(pseudo_doc = pseudo_docs_idx), paste, collapse = " ")

# select code for binary classification
manifesto_data$category <- ifelse(manifesto_data$cmp_code == current_class, current_class, "Other")
manifesto_data$category <- factor(manifesto_data$category, levels = c("Other", current_class))

# initialize classification object
experiment <- tmca_classify(corpus = manifesto_data$content, gold_labels = manifesto_data$category)
# extract LDA features
experiment$extract_features_lda(manifesto_pseudo_docs$x, K = 50, iter = 250)
# set same data as validation set to test progress
# of classification performance during iterations
experiment$set_validation_AL_corpus()
# run active learning
# experiment$reset_active_learning()
experiment$create_initial_trainingset(initial_training_size)
experiment$active_learning(stop_threshold = 0.99, positive_class = current_class)
experiment$plot_progress()


final_categories <- experiment$classify()
manifestos <- factor(paste(manifesto_data$country, manifesto_data$party, manifesto_data$year, sep = "_"))
tmca_proportions(final_categories, manifesto_data$category, facets = manifestos, positive_class = current_class)
