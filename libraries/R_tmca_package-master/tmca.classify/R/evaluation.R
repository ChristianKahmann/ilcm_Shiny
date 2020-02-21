#' Classification evaluation scores
#' F1, Cohen's kappa, Krippendorff's alpha to compare two label vectors, prediction and truth.
#' Micro average: TP, FP, FN over all category decisions first, then F1
#' Macro average: F1 over each individual categories first, then average
#' @param prediction vector (factor) of predicted labels
#' @param truth vector (factor) of true labels
#' @param positive_class label (level) of positive class (if not given, the minority class
#' in true labels is assumed as positive)
#' @param evaluate_irr compute alpha and kappa agreement statistics (requires irr package)
#'
#' @return Evaluation metrics: Precision, Recall, Specificity, Accuracy, F1-score, Alpha, Kappa
#' @export
#'
#' @examples
#' truth <- factor(c("P", "N", "N", "N"))
#' prediction <- factor(c("P", "P", "P", "N"))
#' tmca_fscore(prediction, truth, positive_class = "P")
tmca_fscore <- function(prediction, truth, positive_class = NULL, evaluate_irr = TRUE) {

  if (length(prediction) != length(truth)) {
    stop("F.measure: lengths of true and predicted labels do not match.")
  }

  # PREPARE DATA
  all_predictions <- as.vector(prediction)
  all_truths <- as.vector(truth)
  classes <- levels(factor(c(all_predictions, all_truths)))

  # Macro F1
  # --------

  if (length(classes) == 2) {
    # Binary classification

    if (is.null(positive_class)) {
      # assume positive_class is minority class
      minority_class <- which.min(table(truth))
      positive_class <- classes[minority_class]
      warning(paste0("No positive class name given. Assume ", positive_class, " as positive class"))
    }

    pred <- ifelse(all_predictions == positive_class, 1, 0)
    labels <- ifelse(all_truths == positive_class, 1, 0)

    evaluation <- F.measure_bin(pred, labels)

    if (evaluate_irr) {
      evaluation <- c(evaluation, tmca_irr_statistics(all_predictions, all_truths))
    }

    evaluation <- t(as.data.frame(evaluation))

    return(evaluation)

  } else {
    # Multi-class evaluation

    # MICRO
    TP_micro <- 0
    FP_micro <- 0
    FN_micro <- 0

    # Macro
    results <- matrix(0, nrow = length(classes), ncol = 6)
    rownames(results) <- classes
    colnames(results) <- c("N", "P", "R", "S", "F", "A")

    # Determine F1 per class
    for (current_class in classes) {

      pred <- ifelse(all_predictions == current_class, 1, 0)
      labels <- ifelse(all_truths == current_class, 1, 0)

      result_macro <- F.measure_bin(pred, labels)

      TP_micro <- TP_micro + sum(pred[pos.labels] == 1)
      FP_micro <- FP_micro + sum(pred[neg.labels] == 1)
      FN_micro <- FN_micro + sum(pred[pos.labels] == 0)

      results[current_class, ] <- result_macro

    }

    # Micro F1
    if ((TP_micro + FP_micro) == 0) {
      precision_micro <- 0
    } else {
      precision_micro <- TP_micro / (TP_micro + FP_micro)
    }
    if ((TP_micro + FN_micro) == 0) {
      recall_micro <- 0
    } else {
      recall_micro <- TP_micro / (TP_micro + FN_micro)
    }

    f1_micro <- 2 *(precision_micro * recall_micro) / (precision_micro + recall_micro)
    result_micro <- c(precision_micro, recall_micro, f1_micro )
    names(result_micro) <- c("P", "R", "F")

    if (evaluate_irr) {
      result_micro <- c(result_micro, tmca_irr_statistics(all_predictions, all_truths))
    }

    results <- as.data.frame(results)
    result_micro <- t(as.data.frame(result_micro))

    fList <- list(macro = results, micro = result_micro)
    return(fList)
  }
}



#' Classification evaluation scores for binary classification
#'
#' @param pred vector (factor) of predicted labels
#' @param labels vector (factor) of true labels
#'
#' @return Evaluation metrics: Precision, Recall, Specificity, Accuracy, F1-score, Alpha, Kappa
#' @export
#'
#' @examples
#' F.measure_bin(c(0,0,0,1), c(0,1,0,1))
F.measure_bin <- function(pred, labels) {

  neg_labels <- which(labels == 0)
  pos_labels <- which(labels == 1)
  n_pos <- length(pos_labels)

  TP <- sum(pred[pos_labels] == 1)
  FP <- sum(pred[neg_labels] == 1)
  FN <- sum(pred[pos_labels] == 0)
  TN <- sum(pred[neg_labels] == 0)
  accuracy <- (TP + TN) / length(labels)
  if ((TP + FP) == 0) {
    precision <- 0
  } else {
    precision <- TP / (TP + FP)
  }
  if ((TP + FN) == 0) {
    recall <- 0
  } else {
    recall <- TP / (TP + FN)
  }
  if ((TN + FP) == 0) {
    specificity <- 0
  } else {
    specificity <- TN / (TN + FP)
  }
  if ((precision + recall) == 0) {
    F1 <- 0
  } else {
    F1 <- 2 * (precision * recall) / (precision + recall)
  }
  result <- c(n_pos, precision, recall, specificity, F1, accuracy)
  names(result) <- c("N", "P", "R", "S", "F", "A")
  return(result)
}


#' Inter-rater agreement statistics
#'
#' Kappa and alpha for irr
#'
#' @param pred vector (factor) of predicted labels
#' @param labels vector (factor) of true labels
#'
#' @return
#' @export
#'
#' @examples
#' random1 <- sample(0:1, 1000, replace = T)
#' random2 <- sample(0:1, 1000, replace = T)
#' tmca_irr_statistics(random1, random2)
tmca_irr_statistics <- function(pred, labels) {
  kappa <- irr::kappa2(data.frame(pred, labels))$value
  # disable warnings for NaN values
  tmp_warn <- options("warn")
  options(warn = -1)
  alpha <- irr::kripp.alpha(t(data.frame(pred, labels)))$value
  options(warn = tmp_warn$warn)
  result <- c(kappa = kappa, alpha = alpha)
  return(result)
}


#' Proportional classification evaluation
#'
#' @param predicted_labels vector (factor) of predicted labels
#' @param true_labels vector (factor) of true labels
#' @param facets factor with data facets to group labels
#' @param positive_class name / level of the positive class
#'
#' @return list with evaluation metrics: root-mean-square deviation, Pearson's r, and predicted/true proportions per facet
#' @export
#'
#' @examples
tmca_proportions <- function(predicted_labels, true_labels, facets = factor(), positive_class = NULL, verbose = F) {
  if (length(predicted_labels) != length(true_labels)) {
    stop("Lengths of true and predicted labels do not match.")
  }
  if (length(predicted_labels) != length(facets) & length(facets) > 0) {
    stop("Lengths of labels and facets do not match.")
  }

  if (length(facets) > 0) {
    predicted_proportions <- NULL
    true_proportions <- NULL
    for (l in levels(facets)) {
      lidx <- facets == l
      facet_pred_prop <- table(predicted_labels[lidx]) / sum(lidx)
      facet_true_prop <- table(true_labels[lidx]) / sum(lidx)
      predicted_proportions <- c(predicted_proportions, facet_pred_prop[positive_class])
      true_proportions <- c(true_proportions, facet_true_prop[positive_class])
    }
  } else {
    predicted_proportions <- table(predicted_labels) / length(predicted_labels)
    true_proportions <- table(true_labels) / length(true_labels)
  }

  df <- data.frame(predicted_proportions, true_proportions)
  rownames(df) <- levels(facets)

  if (verbose) {
    print(df)
  }

  res_rmsd <- rmsd(predicted_proportions, true_proportions)
  res_r <- cor.test(predicted_proportions, true_proportions, na.action = na.omit)
  result = list(
    rmsd = res_rmsd,
    persons_r = res_r$estimate,
    proportions = df
  )
  return(result)
}

#' Root mean-square deviation
#'
#' @param y1 numeric data series 1
#' @param y2 numeric data series 2
#'
#' @return numeric RMSD value
#' @export
#'
#' @examples
rmsd <- function(y1, y2) {
  if ((length(y1) != length(y2)) | length(y1) == 0)
    stop("length of arguments does not match / is not greater than 0")
  keepIdx <- !is.na(y1)
  y1 <- y1[keepIdx]
  y2 <- y2[keepIdx]
  result <- sqrt(sum((y1 - y2) ^ 2) / length(y1))
  return(result)
}

# tmca_fscore(c(0,0,0,1), c(0,1,1,1))
