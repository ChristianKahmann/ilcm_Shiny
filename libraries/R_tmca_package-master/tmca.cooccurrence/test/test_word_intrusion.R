load("data/example_model_LDA.RData")

ldaPosterior <- posterior(lda)
dim(ldaPosterior$terms)

tm_word_intrusion(ldaPosterior$terms)


tm_word_intrusion(ldaPosterior$terms, chance_correction = F)


tm_word_intrusion(ldaPosterior$terms, k = 1)

