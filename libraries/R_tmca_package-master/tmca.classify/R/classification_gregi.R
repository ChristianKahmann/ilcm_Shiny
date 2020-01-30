#' TMCA classification
#'
#' This class wraps functions around LiblineaR to conduct active
#' learning and standard text classification in a social science
#' scenario, especially to  allow for accurate predictions of
#' category proportions and their changes in large data sets.
#' At the moment, only binary classification is supported.
#'
#' 1. The object is initialized given a corpus and a factor
#' containing category information. A corpus is simply
#' a character vector containing all documents. The labels
#' factor has to be of the same length as the corpus. Its levels
#' represent the categories (e.g. "Positive" and "Negative").
#' Unknown labels need to be encoded as NA values in the factor.
#'
#' To use the class in an experiment setting, e.g. to evaluate
#' active learning performance (and different pre-processing or
#' query selection strategies), an object can be initialized
#' further by a factor of gold labels considered as truth values
#' for document categories in subsequent steps.
#'
#' 2. The initialization invokes ngram-features extraction. When
#' useful, additional LDA features can be extracted.
#'
#' 3. An inital training set is sampled and labelled. In the
#' standard setting, a human annotator is asked for query labels.
#' In the experiment setting, labels are taken from the gold
#' labels.
#'
#' 4. Active learning is performed an stops at a defined
#' stability threshold criterion. Again, in the
#' standard setting, a human annotator is asked for query labels.
#' In the experiment setting, labels are taken from the gold
#' labels.
#'
#' For the usual evaluation scenario it makes sense to set the entire
#' corpus also as validation corpus instead of using hold out data.
#' This allows the active learning process to learn from all data.
#' For this, one can set `set_validation_AL_corpus()` before starting
#' active learning experiments.
#'
#' @field corpus character vector containing documents.
#' @field labels factor (optional) may contain previously made annotation.
#' Is supposed to be a factor with two levels. Unlabeled instances need
#' to be encoded as NA values.
#' @field gold_labels factor  (optional) may contain previously made annotation.
#' Is supposed to be a factor with two levels. Unlabeled instances need
#' to be encoded as NA values.
#' @field iteration numeric counts iterations of active learning.
#' @field progress data.frame keeps record of learning progress.
#' @field progress_examples list keeps record of newly learned examples (ids).
#' @field progress_validation data.frame. keeps record of learning progress on a validation set
#' @field stop_words character vector for words to remove during ngram feature extraction.
#' Default value is a list for English stopwords.
#' @field negation_words two-column data.frame for pairs of strings and replacements to better
#' capture negation (e.g. containing aren't | are not). Default value is a list for English
#' negation terms.
#' @field language character language code to select correct stopword lists and stemmers. Default = "en"
#' @field dfm_ngram Matrix extracted ngram-features.
#' @field dfm_lda Matrix extracted LDA features.
#' @field model_svm list SVM model from currently labelled set.
#' @field model_lda LDA_Gibbs model from a given reference corpus.
#' @field lda_most_frequent_term character for internal use.
#' @field validation_corpus character validation (hold out) set.
#' @field validation_labels factor validation (hold out) labels.
#' @field validation_dfm_ngram Matrix validation (hold out) ngram features.
#' @field validation_dfm_lda Matrix validation (hold out) LDA features.
#'
#' @return tmca classification object to run classification / active learning
#' @export tmca_classify
#' @exportClass tmca_classify
#' @importFrom foreach %dopar%
#' @importFrom foreach foreach
#'
#' @examples
#' my_corpus <- c("It's brilliant", "Me no likey.", "It was brilliant.", "Soo bad.", "It was great", "I love it!!!", "Not good!")
#' my_labels <- factor(rep(NA, length(my_corpus)), levels = c("Positive", "Negative"))
#' my_classification <- tmca_classify(corpus = my_corpus, labels = my_labels)
#' my_classification$create_initial_trainingset(n = 4)
#' my_classification$active_learning(batch_size = 1)
tmca_classify <- setRefClass(
  "tmca_classify",
  fields = list(
    corpus = "character",
    labels = "factor",
    gold_labels = "factor",
    iteration = "numeric",
    progress = "data.frame",
    progress_examples = "list",
    progress_validation = "data.frame",
    stop_words = "character",
    negation_words = "data.frame",
    language = "character",
    dfm_ngram = "Matrix",
    dfm_lda = "Matrix",
    model_svm = "list",
    model_lda = "LDA_Gibbs",
    lda_most_frequent_term = "character",
    validation_corpus = "character",
    validation_labels = "factor",
    validation_dfm_ngram = "Matrix",
    validation_dfm_lda = "Matrix"
  ),
  methods = list(
    # dfm: document feature matrix
    # labels: factor with class labels 0 - Negative class, 1 - Positive class, NA - unlabeled
    initialize = function(corpus, labels = NULL, iteration = 1, gold_labels = factor(), stop_words = NULL, negation_words = NULL, language = "en", minimum_frequency = 2) {
      "Creation of classification object from corpus.
      Corpus is supposed to be a character vector.
      More options for feature extraction are possible."
      corpus <<- trimws(corpus)
      if(is.null(labels) & length(gold_labels) == 0) {
        stop("Provide either labels or gold_labels vector.")
      }
      if (!is.null(labels)) {
        if (length(labels) != length(corpus)) {
          stop("Labels vector must have equal length as corpus. Use NA values to encode unknown labels.")
        }
        labels <<- labels
      } else {
        labels <<- factor(rep(NA, length(gold_labels)), levels = levels(gold_labels))
      }
      if (length(gold_labels) > 0 & length(.self$labels) != length(gold_labels)) {
        stop("Labels vector must have equal length as gold_labels. Use NA values to encode unknown labels.")
      }
      gold_labels <<- gold_labels
      iteration <<- iteration
      progress <<- data.frame(row.names = F, stringsAsFactors = F)
      progress_validation <<- data.frame(row.names = F, stringsAsFactors = F)
      progress_examples <<- list()
      if (is.null(stop_words)) {
        sw_file <- paste0(system.file(package = "tmca.classify"),"/resources/stopwords_en.txt")
        stop_words <<- readLines(sw_file, encoding = "UTF-8")
      }
      # browser()
      if (is.null(negation_words)) {
        neg_file <- paste0(system.file(package = "tmca.classify"),"/resources/negation_en.txt")
        negation_words <<- read.csv(neg_file, sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
      }
      language <<- language
      dfm_ngram <<- extract_features_ngram(minimum_frequency = minimum_frequency)
      dfm_lda <<- Matrix(0)
      lda_most_frequent_term <<- "the"
      validation_labels <<- factor()
      validation_dfm_lda <<- Matrix(0)
    },
    active_learning = function(
      # corpus,
      # dfm,
      # labels,
      batch_size = 10L,
      max_iterations = 200,
      tune_C = FALSE,
      cross = NULL,
      stop_threshold = 0.99,
      stop_window = 3,
      type = 7,
      verbose = TRUE,
      positive_class = NULL
    ) {
      "Active learning for classification. If gold labels are present,
      experiment mode is conducted. Otherwise, the user oracle is asked
      to decide on selected examples."

      dfm <- get_dfm()

      # sanity checks
      if (length(levels(labels)) != 2) {
        stop("This function is for binary classification only")
      }
      if (nrow(dfm) != length(labels)) {
        stop("Length of labels and nrows of dfm do not match")
      }
      if (is.null(positive_class)) {
        positive_class <- levels(labels)[2]
        message(paste0("No positive class label given. Assuming ", positive_class), immediate. = T)
      }

      label_names <- levels(labels)
      previous_predictions <- factor(rep(levels(labels)[1], length(labels)), levels = levels(labels))
      stop_active_learning <- FALSE

      while (!stop_active_learning) {

        # split data
        labeled_lidx <- !is.na(labels)
        s_dfm <- Matrix_to_SparseM(dfm[labeled_lidx, ])
        s_labels <- labels[labeled_lidx]
        u_dfm <- Matrix_to_SparseM(dfm[!labeled_lidx, ])
        u_labels <- labels[!labeled_lidx]
        u_labels_idx <- which(!labeled_lidx)

        # get heuristic C-weights
        c_weights <- table(s_labels) / length(s_labels)
        c_weights <- abs(c_weights - 1)

        # Tune C
        if (tune_C == T) {
          optimal_c <- optimize_c(s_dfm, s_labels, type = 7)
        } else {
          optimal_c <- 1
        }

        # Train linear SVM
        model <- LiblineaR(
          s_dfm,
          s_labels,
          wi = c_weights,
          cost = optimal_c,
          type = type
        )

        oracle <- select_queries(model, u_dfm, u_labels_idx, batch_size, verbose = verbose)
        labels[oracle$selected_queries] <<- oracle$oracle_decisions

        # predict all instances (for stopping criterion)
        predicted_labels_su <- predict(model, Matrix_to_SparseM(dfm))

        # simple statistics
        n_pos <- sum(labels == positive_class, na.rm = T)
        n_neg <- sum(labels != positive_class, na.rm = T)
        eval_metrics <- data.frame(iteration, n_pos, n_neg, stringsAsFactors = F)

        # Stopping
        if (iteration == 1) {
          stabililty <- 0
        } else {
          stabililty <- irr::kappa2(cbind(predicted_labels_su$predictions, previous_predictions))$value
        }
        previous_predictions <- predicted_labels_su$predictions

        eval_metrics <- cbind(eval_metrics, stabililty)

        # evaluation metrics (A, P, R, S, kappa, alpha)
        if (is.numeric(cross)) {
          labeled_lidx <- !is.na(labels)
          s_dfm <- Matrix_to_SparseM(dfm[labeled_lidx, ])
          s_labels <- labels[labeled_lidx]
          cv_res <- cross_validation(s_dfm, s_labels, n_folds = cross, cost = optimal_c, type = type, positive_class = positive_class)
          cv_res <- as.data.frame(cv_res)
          eval_metrics <- cbind(eval_metrics, t(cv_res))
        }

        # validation set evaluation
        if (length(validation_labels) > 0) {
          v_predicted <- classify(get_dfm(validation = T))
          v_result <- tmca_fscore(v_predicted, validation_labels, positive_class = positive_class)
          progress_validation <<- rbind(progress_validation, v_result)
        }

        progress <<- rbind(progress, eval_metrics)
        rownames(progress) <<- NULL

        progress_examples <<- c(progress_examples, list(oracle$selected_queries))

        if (verbose) {
          print(tail(progress, 1))
          if (length(validation_labels) > 0) {
            print(tail(progress_validation, 1))
          }
        }

        if (stop_criterion_matches(progress$stabililty, threshold = stop_threshold, window = stop_window)) {
          message("Stability criterion reached. Stopping active learning.")
          stop_active_learning <- TRUE
        }

        if (iteration >= max_iterations) {
          message("Max iterations reached. Stopping active learning.")
          stop_active_learning <- TRUE
        }

        if (sum(labeled_lidx) == length(labels)) {
          message("No new examples to learn. Stopping active learning.")
          stop_active_learning <- TRUE
        }

        iteration <<- iteration + 1

      }
    },
    select_queries = function(model, u_dfm, u_labels_idx, batch_size, verbose = 1, strategy = "LC") {
      "Select queries for the (human) oracle by different strategies."
      if (strategy == "random") {
        # select random
        selected_queries <- sample(u_labels_idx, batch_size)

      } else if (strategy == "LC") {
        # select least certain

        # predict unlabeled instances
        predicted_labels_u <- predict(model, u_dfm, proba = T)
        # predicted_labels_u <- predict(model, u_dfm, decisionValues = T)

        # select examples near hyperplane
        boundary_distances <- abs(predicted_labels_u$probabilities[, 1] - 0.5)
        # boundary_distances <- abs(predicted_labels_u$decisionValues[, 1])
        uncertain_decisions <- order(boundary_distances)[1:batch_size]
        selected_queries <- u_labels_idx[uncertain_decisions]

      } else if (strategy == "MC") {
        # select most certain

      } else if (strategy == "LCB") {
        # browser()
        pp <- sum(s_labels == positive_class) / length(s_labels)
        pmax <- mean(c(0.5, 1 - pp))
        predicted_labels_u <- predict(model, u_dfm, proba = T)
        prob_positive <- predicted_labels_u$probabilities[, positive_class]
        lidx <- prob_positive < pmax
        uncertain_decisions <- rep(0, length(predicted_labels_u))
        uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
        uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
        
        # order and select
        uncertain_decisions <- order(uncertain_decisions, decreasing = T)[1:batch_size]
        selected_queries <- u_labels_idx[uncertain_decisions]
        
      } else if (strategy == "LCBMC") {
        # browser()
        pp <- sum(s_labels == positive_class) / length(s_labels)
        pmax <- mean(c(0.5, 1 - pp))
        predicted_labels_u <- predict(model, u_dfm, proba = T)
        prob_positive <- predicted_labels_u$probabilities[, positive_class]
        lidx <- prob_positive < pmax
        current_uncertain_decisions <- rep(0, length(predicted_labels_u))
        current_uncertain_decisions[lidx] <- prob_positive[lidx] / pmax
        current_uncertain_decisions[!lidx] <- (1 - prob_positive[!lidx]) / (1 - pmax)
        
        w0 <- 1 / length(s_labels)
        uncertain_decisions <- current_uncertain_decisions - w0 * last_AL_uncertainty[as.character(u_labels_idx)]
        last_AL_uncertainty[as.character(u_labels_idx)] <<- current_uncertain_decisions
        
        # order and select
        uncertain_decisions <- order(uncertain_decisions, decreasing = T)[1:batch_size]
        selected_queries <- u_labels_idx[uncertain_decisions]
        
      } else {
        stop("Unknown query selection strategy")
      }

      # oracle
      if (length(gold_labels) == length(labels)) {
        oracle_decisions <- query_oracle_experiment(selected_queries)
      } else {
        oracle_decisions <- query_oracle_human(selected_queries)
      }

      if (verbose > 1) {
        cat("Selected queries:\n")
        cat(paste(oracle_decisions, "-", corpus[selected_queries], "\n"))
      }

      return(list(
        selected_queries = selected_queries,
        oracle_decisions = oracle_decisions
      ))

    },
    query_oracle_human = function(idx) {

      viewer <- getOption("viewer")

      decisions <- factor(rep(0, length(idx)), levels = levels(labels))

      for (d in 1:length(idx)) {
        doc_id <- idx[d]

        if (!is.null(viewer)) {
          doc_html <- as.character(corpus[[doc_id]])
          doc_html <- gsub("\n", "<br/>", doc_html)
          htmltools::html_print(htmltools::HTML(paste0("<h2>ID ", doc_id, "</h2><p>", doc_html, "<p>")))
        } else {
          cat("ID", doc_id, "\n", "-----------------", as.character(corpus[[d]]), "\n")
        }

        decisions[d] <- levels(labels)[read_human_decision()]

      }

      return(decisions)
    },
    read_human_decision = function(show_label_legend = T) {
      if (show_label_legend) {
        cat("\n")
        for (i in 1:length(levels(labels))) {
          cat(paste0("   [", i, "] ", levels(labels)[i], "\n"))
        }
      }

      n <- readline(prompt="Label for document: ")
      n <- as.integer(n)
      if (is.na(n) | !is.na(n) & (n < 1 | n > 2)) {
        abort <- readline(prompt = "Could not interpret selection. Abort? [yes|no]: ")
        if (tolower(substr(abort, 0, 1)) == "y") {
          stop("Aborted\n")
        } else {
          n <- read_human_decision()
        }
      }

      return(n)
    },
    query_oracle_experiment = function(idx) {
      return(gold_labels[idx])
    },
    stop_criterion_matches = function(v, window = 2, threshold = 0.99) {
      "Stopping criterion for active learning: Stability (see Bloodgood; Vijay-Shanker 2009)"
      b <- v[!is.na(v)] > threshold
      if (length(b) < window) return(0)
      r <- sapply(1:(length(b) - window + 1), FUN = function(x) {
        if (all(b[x:(x + window - 1)])) {
          T
        } else {
          F
        }
      })
      if (!any(r)) {
        return(0)
      }
      return(which(r)[1] + window - 1)
    },
    get_k_fold_logical_indexes = function(j, k, n) {
      if (j > k) stop("Cannot select fold larger than nFolds")
      fold_lidx <- rep(FALSE, k)
      fold_lidx[j] <- TRUE
      fold_lidx <- rep(fold_lidx, length.out = n)
      return(fold_lidx)
    },
    cross_validation = function(cv_dfm, cv_labels, n_folds = 10, cost = 1, type = 7, positive_class = NULL) {
      "N-fold cross validation for classification. Classifcation data is split into n folds.
      Training is conducted on n-1 folds and the resulting model is evaluated on the remaining fold.
      The process is repeated n_fold times with changing test folds. Mean evaluation measures
      are returned as result."
      if (is.null(positive_class)) {
        # assume second class is positive_class
        # minority_class <- which.min(table(cv_labels))
        # positive_class <- levels(cv_labels)[minority_class]
        positive_class <- levels(cv_labels)[2]
        warning(paste0("No positive class name given. Assume ", positive_class, " as positive class"))
      }
      if (class(cv_dfm) == "dgCMatrix") {
        cv_dfm <- Matrix_to_SparseM(cv_dfm)
      }
      evaluationMeasures <- data.frame()
      for (j in 1:n_folds) {
        current_fold <- get_k_fold_logical_indexes(j, n_folds, nrow(cv_dfm))

        trainingSet <- cv_dfm[!current_fold, ]
        trainingLabels <- cv_labels[!current_fold]

        # get heuristic C-weights
        c_weights <- table(trainingLabels) / length(trainingLabels)
        c_weights <- abs(c_weights - 1)

        model <- LiblineaR(
          trainingSet,
          trainingLabels,
          wi = c_weights,
          cost = cost,
          type = type)

        testSet <- cv_dfm[current_fold, ]
        testLabels <- cv_labels[current_fold]
        predictedLabels <- predict(model, testSet)$predictions

        # collect n_folds evaluation results
        kthEvaluation <- tmca_fscore(predictedLabels, testLabels, positive_class = positive_class)
        evaluationMeasures <- rbind(evaluationMeasures, kthEvaluation)
      }
      # print(evaluationMeasures)
      evaluationMeans <- colMeans(evaluationMeasures)
      evaluationMeans[1] <- sum(evaluationMeasures[, 1])
      return(evaluationMeans)
    },
    optimize_C = function(trainingDTM, trainingLabels, plot_graph = F) {
      "C-parameter optimization by testing different values
      (0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)."
      cParameterValues <- c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 3 , 10, 30, 100)
      fValues <- NULL
      for (cParameter in cParameterValues) {
        print(paste0("C = ", cParameter))
        evalMeasures <- k_fold_cross_validation(trainingDTM, trainingLabels, cost = cParameter)
        fValues <- c(fValues, evalMeasures["F"])
      }
      if (plot_graph) {
        plot(fValues, type="o", col="green", xaxt="n")
        axis(1,at=1:length(cParameterValues),labels=cParameterValues)
      }
      bestC <- cParameterValues[which.max(fValues)]
      print(paste0("Best C value: ", bestC, ", F1 = ", max(fValues)))
      return(bestC)
    },
    extract_ngrams = function(text, useStemming = TRUE, useBigrams = TRUE, removeSW = FALSE, lower = TRUE, replaceNumbers = TRUE) {
      "Extracts ngram word features by regex tokenizer. Preprocessing:
      negation word normalization, stemming, bigrams,
      stop word removal, lower case reduction, number replacement. Set
      language slot to use correct stemmer and stop word/negation lists."

      # lower case
      if (lower) {
        # capitalized_full <- stringi::stri_extract_all_regex(text, "\\p{Lu}{2,}", simplify = T)
        # capitalized_start <- stringi::stri_extract_all_regex(text, "\\p{Lu}\\p{L}+", simplify = T)
        text <- tolower(text)
      }

      # replace negation terms
      if (!is.null(negation_words)) {
        text <- stringi::stri_replace_all_fixed(text, negation_words$term, negation_words$replacement, vectorize_all = F)
      }

      # replace numbers
      if (replaceNumbers) {
        numbers <- stringi::stri_extract_all_regex(text, "\\d+", simplify = T)
        text <- stringi::stri_replace_all_regex(text, "\\d", "#", vectorize_all = F)[[1]]
      } else {
        numbers <- c()
      }

      # tokenize with regex
      wordsInS <- trimws(regmatches(text, gregexpr("[\\p{L}-]+|[^\\p{L}\\s]+", text, perl = T))[[1]])

      # remove stop words
      if (removeSW) {
        wordsInS <- wordsInS[!(wordsInS %in% stop_words)]
      }

      # stemming
      if (useStemming) {
        wordsInS <- SnowballC::wordStem(wordsInS, language = language)
      }

      features <- c(table(numbers))

      # extract unigrams
      features <- c(features, table(wordsInS))

      # extract bigrams
      if (useBigrams & length(wordsInS) > 1) {
        bigrams <- c()
        for (i in 1:(length(wordsInS)-1)) {
          bigram <- paste0(c(wordsInS[i], wordsInS[(i+1)]), collapse="_")
          bigrams <- c(bigrams, bigram)
        }
        features <- c(features, table(bigrams))

        # if stop words are not removed, still do it and concat new bigrams
        if (!removeSW) {
          wordsNotSW <- wordsInS[!(wordsInS %in% stop_words)]
          if (length(wordsNotSW) > 1) {
            bigramsNSW <- c()
            for (i in 1:(length(wordsNotSW)-1)) {
              bigram <- paste0(c(wordsNotSW[i], wordsNotSW[(i+1)]), collapse="_")
              if (!(bigram %in% names(features))) {
                bigramsNSW <- c(bigramsNSW, bigram)
              }
            }
            features <- c(features, table(bigramsNSW))
          }
        }
      }

      # remove empty feature
      features <- features[!names(features) %in% c("", " ")]
      return(features)
    },
    extract_features_ngram = function(
      text_corpus = .self$corpus,
      TRAIN = TRUE,
      minimum_frequency = 2,
      removeSW = F,
      bigrams = T,
      binary_dfm = FALSE,
      feature_dictionary = colnames(.self$dfm_ngram)) {
      "Extracts ngrams from the corpus. For using parallelization register
      a suitable parallel backend.
      # For parallelization: register backends
      # if(.Platform$OS.type == \"unix\") {
      #   require(doMC)
      #   registerDoMC(8)
      # } else {
      #   require(doParallel)
      #   workers <- makeCluster(4, type=\"SOCK\")
      #   registerDoParallel(workers)
      # }"

      text_corpus <- as.character(text_corpus)

      if (TRAIN) {
        feature_dictionary = character()
        message("Extracting new features. This may take a while.")
      } else {
        message(paste0("Extracting features using dictionary (vocabulary size ", length(feature_dictionary), ")"))
      }

      # extract features from CMP data
      corpusLength <- length(text_corpus)

      if (exists("globalVocabHash")) {
        clear(globalVocabHash)
      } else {
        globalVocabHash <- hash::hash()
      }

      if (!TRAIN) {
        globalVocabHash <- hash::hash(feature_dictionary, 0)
      }

      countEmptyFeatureVectors <- 0

      featureList <- foreach(sentNumber = 1:corpusLength, .export = "extract_ngrams") %dopar% {

        currentFeatureVector <- extract_ngrams(text_corpus[sentNumber], removeSW = removeSW, useBigrams = bigrams)

        if (length(currentFeatureVector) > 0) {

          if (!TRAIN) {
            # RESTRICT TO VOCAB
            knownFeatures <- hash::has.key(names(currentFeatureVector), globalVocabHash)
            if (any(knownFeatures)) {
              currentFeatureVector <- currentFeatureVector[knownFeatures]
            } else {
              currentFeatureVector <- NULL
              warning(paste0("No features extracted for: ", text_corpus[sentNumber]))
            }
          }

        } else {
          currentFeatureVector <- NULL
        }

        if (binary_dfm) {
          count <- 1
        } else {
          count <- as.integer(currentFeatureVector)
        }

        if (is.null(currentFeatureVector)) {
          currentFeatureVector <- table("EMPTY_DOC")
          count <- 0
        }

        dt <- data.table::data.table(id = sentNumber, token = names(currentFeatureVector), count = count)
      }

      # combine list of features into feature matrix

      message(paste0("Create feature matrix for ", length(featureList), " documents."))

      combinedDT <- data.table::rbindlist(featureList)

      if (TRAIN) {
        vocabFactor <- factor(combinedDT$token)
      } else {
        vocabFactor <- factor(combinedDT$token, levels = feature_dictionary)
      }

      featureNames <- levels(vocabFactor)
      col_idx <- as.integer(vocabFactor)
      feature_count <- combinedDT$count

      # handle empty documents: set count for first term to 0
      na_col_idx <- is.na(col_idx)
      col_idx[na_col_idx] <- 1
      feature_count[na_col_idx] <- 0

      # convert to sparse matrix
      featureMatrixFull <- Matrix::sparseMatrix(i = combinedDT$id, j = col_idx, x = feature_count,
                                                dims = c(length(featureList), length(featureNames)),
                                                dimnames = list(1:length(featureList), featureNames))

      if (TRAIN) {
        # reduce feature set size
        featuresToKeep <- Matrix::colSums(featureMatrixFull) >= minimum_frequency
        featureMatrixFull <- featureMatrixFull[, featuresToKeep]
      }

      if (identical(text_corpus, .self$corpus)) {
        dfm_ngram <<- featureMatrixFull
      } else {
        return(featureMatrixFull)
      }
    },
    
    classify = function(
      dfm_target = .self$get_dfm(),
      tune_C = FALSE,
      cross = NULL,
      type = 7,
      verbose = TRUE,
      positive_class = NULL
    ) {
      "Perform classification using the currently labelled instances as training data.
      Returns the classifier decisions as vector. If no target feature matrix (dfm_target)
      is given, the dfm of the current classification object is assumed as default."

      dfm <- get_dfm()

      # get labeled training data
      labeled_lidx <- !is.na(labels)
      s_dfm <- Matrix_to_SparseM(dfm[labeled_lidx, ])
      s_labels <- labels[labeled_lidx]

      # get heuristic C-weights
      c_weights <- table(s_labels) / length(s_labels)
      c_weights <- abs(c_weights - 1)

      # Tune C
      if (tune_C == T) {
        optimal_c <- optimize_c(s_dfm, s_labels, type = 7)
      } else {
        optimal_c <- 1
      }

      # Train linear SVM
      model <- LiblineaR(
        s_dfm,
        s_labels,
        wi = c_weights,
        cost = optimal_c,
        type = type
      )

      # predict unlabeled instances (target)
      predicted_labels <- predict(model, Matrix_to_SparseM(dfm_target))
      return(predicted_labels$predictions)
    },
    create_initial_trainingset = function(
      n = 100
    ) {
      "Creates an initial training set for active learning. If gold labels
      are present, an experiment setting is assumed and n true labels are
      sampled from the gold labels. If no gold labels are present, a (human)
      annotator is asked to judge n samples."

      unlabeled_lidx <- which(is.na(labels))
      if (length(unlabeled_lidx) < n) {
        labels[unlabeled_lidx] <<- gold_labels[unlabeled_lidx]
        message("There were less unlabeled texts than the selected size n. Fully annotated set created.")
        return()
      }
      if (length(unlabeled_lidx) < length(labels)) {
        m <- length(labels) - unlabeled_lidx
        message(paste0("Apparently there are already ", m,
                       " labeled instances. ", n,
                       " additional instances will be presented for annotation."))
      }

      # print(oracle_idx)
      if (length(gold_labels) == length(labels)) {
        # choose n/2 positive, and n/2 negative examples from gold_labels
        examples_class1_idx <- which(gold_labels == levels(gold_labels)[1])
        examples_class2_idx <- which(gold_labels == levels(gold_labels)[2])
        examples_class1_idx <- setdiff(examples_class1_idx, !unlabeled_lidx)
        examples_class2_idx <- setdiff(examples_class2_idx, !unlabeled_lidx)
        class1_sample <- sample(examples_class1_idx, ceiling(n / 2))
        class2_sample <- sample(examples_class2_idx, floor(n / 2))
        labels[class1_sample] <<- levels(gold_labels)[1]
        labels[class2_sample] <<- levels(gold_labels)[2]
        oracle_progress <- c(class1_sample, class2_sample)
      } else {
        # oracle

        oracle_idx <- sample(unlabeled_lidx, n)
        oracle_decisions <- query_oracle_human(oracle_idx)
        labels[oracle_idx] <<- oracle_decisions
        oracle_progress <- oracle_idx
      }
      progress_examples <<- c(progress_examples, list(oracle_progress))
    },
    extract_features_lda = function(lda_corpus, TRAIN = T, K = 50, n_repeat = 20, iter = 500, verbose = 25) {
      "Create K latent semantic features from an LDA topic model (Phan et al. 2011)."
      if (TRAIN) {
        lda_corpus <- trimws(lda_corpus)
        reference_dtm <- extract_features_ngram(lda_corpus, TRAIN = T, minimum_frequency = 3, removeSW = T, bigrams = F)
        # print(dim(reference_dtm))
        reference_dtm <- prune_dfm(reference_dtm, minimum_threshold = 0.0005)
        reference_dtm <- reference_dtm[Matrix::rowSums(reference_dtm) > 0, ]
        # print(dim(reference_dtm))
        message("Computing LDA model on reference corpus")
        model_lda <<- topicmodels::LDA(reference_dtm, k = K, method = "Gibbs", control = list(iter = iter, alpha = 0.1, delta = 0.1, verbose = verbose))
        if (verbose > 0) {
          print(topicmodels::terms(model_lda, 10))
        }
        message("Inference of topic proportions for classification corpus")
        dtm <- extract_features_ngram(text_corpus = .self$corpus, TRAIN = F, feature_dictionary = colnames(reference_dtm))
        lda_most_frequent_term <<- names(sort(Matrix::colSums(dtm), decreasing = T)[1])
      } else {
        model_posterior <- topicmodels::posterior(model_lda)
        features <- colnames(model_posterior$terms)
        dtm <- extract_features_ngram(text_corpus = lda_corpus, TRAIN = F, feature_dictionary = features)
      }

      # handle empty documents: set most frequent term to 1
      empty_docs <- Matrix::rowSums(dtm) == 0
      if (any(empty_docs)) {
        dtm[empty_docs, lda_most_frequent_term] <- 1
      }

      for (i in 1:n_repeat) {
        message(paste0("Inference run ", i, " of ", n_repeat))
        ldaPosterior <- topicmodels::posterior(model_lda, dtm, control = list(iter = 200))
        if (i == 1) {
          theta <- ldaPosterior$topics
        } else {
          theta <- theta + ldaPosterior$topics
        }
      }
      theta <- theta / n_repeat

      if (TRAIN) {
        dfm_lda <<- Matrix(theta)
      } else {
        # return(Matrix(theta))
        validation_dfm_lda <<- Matrix(theta)
      }

    },
    get_dfm = function(validation = FALSE) {
      "Retrieves the current feature matrix. Combines ngram-features
      with LDA features, if both are present."
      if (!validation) {
        dfm <- dfm_ngram
        if (nrow(dfm_lda) > 1) dfm <- cBind(dfm, dfm_lda)
      } else {
        dfm <- validation_dfm_ngram
        if (nrow(validation_dfm_lda) > 1) dfm <- cBind(dfm, validation_dfm_lda)
      }
      return(dfm)
    },
    set_validation_holdout_corpus = function(v_corpus, v_labels) {
      "Sets validation hold out set to the given v_corpus and
      v_labels. This is not advised when using active learning.
      Since good examples cannot be learned from the hold
      out set, classification performance will be drastically
      lowered."
      validation_corpus <<- v_corpus
      validation_labels <<- v_labels
      validation_dfm_ngram <<- extract_features_ngram(validation_corpus, TRAIN = F)
      if (nrow(dfm_lda) > 1) {
        validation_dfm_lda <<- extract_features_lda(lda_corpus = v_corpus, TRAIN = F, iter = 100)
      }
    },
    set_validation_AL_corpus = function() {
      "Sets validation hold out set to the same data as the
      primary classification set. This is for evaluation of
      progress of active learning."
      if (any(is.na(gold_labels)) | length(gold_labels) == 0) {
        stop("Cannot set validation data from base data. No gold labels given.")
      }
      validation_labels <<- gold_labels
      validation_dfm_ngram <<- dfm_ngram
      if (nrow(dfm_lda) > 1) {
        validation_dfm_lda <<- dfm_lda
      }
    },
    plot_progress = function() {
      "Plot the progress of active learning. If a validation set is given
      evaluation metrics on this validation set are plotted.
      For the usual evaluation scenario it makes sense to set the entire
      corpus also as validation corpus instead of using hold out data.
      This allows the active learning process to learn from all data."
      if (nrow(progress_validation) > 1) {
        plot(progress$stabililty, type = "l", ylim = c(0, 1), col = "black", xlab = "Iteration", ylab = "stability", main = "Active learning progress")
        lines(progress_validation$F, type = "l", col = "green")
        lines(progress_validation$S, type = "l", col = "blue")
        lines(progress_validation$kappa, type = "l", col = "orange")
        legend("bottom", legend=c("stability", "F1", "Specificity", "Kappa"), lty=1, col = c("black", "green", "blue", "orange"), bty="n", horiz=TRUE, cex = 0.8)
      } else {
        plot(progress$stabililty, type = "l", ylim = c(0, 1), col = "black", xlab = "Iteration", ylab = "stability", main = "Active learning progress")
      }
    },
    reset_active_learning = function(new_gold_labels = factor()) {
      "Reset labels, progress records and iteration count.
      This is useful for AL experimentation, when feature
      generation is costly."
      if (length(new_gold_labels) > 0) {
        gold_labels <<- new_gold_labels
      }
      labels <<- factor(rep(NA, length(gold_labels)), levels = levels(gold_labels))
      iteration <<- 1
      progress <<- data.frame(row.names = F, stringsAsFactors = F)
      progress_validation <<- data.frame(row.names = F, stringsAsFactors = F)
      progress_examples <<- list()
    }
  )
)

#' Feature matrix pruning
#'
#' Prunes a document feature matrix (dfm) using relative thresholds
#'
#' @param dfm a sparse matrix (Matrix)
#' @param minimum_threshold minimum document frequency threshold for relative pruning
#' @param maximum_threshold maximum document frequency threshold for relative pruning
#'
#' @return A pruned sparse Matrix
#' @export
#'
#' @examples
#' # example feature matrix
#' m <- Matrix(round(replicate(10, abs(rnorm(20))) * 10))
#' colnames(m) <- as.character(1:10)
#' dim(m)
#' n <- prune_dfm(m)
#' dim(n)
#'
prune_dfm = function(dfm, minimum_threshold = 0.005, maximum_threshold = 1) {
  document_frequency <- Matrix::colSums(dfm > 0)
  threshold_min_abs <- nrow(dfm) * minimum_threshold
  threshold_max_abs <- nrow(dfm) * maximum_threshold
  features_to_keep <- names(which(document_frequency > threshold_min_abs & document_frequency < threshold_max_abs))
  dfm <- dfm[, features_to_keep]
  return(dfm)
}



