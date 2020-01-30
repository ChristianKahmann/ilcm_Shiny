load("data/example_model_LDA.RData")

ldaPosterior <- posterior(lda)
dim(ldaPosterior$terms)

tm_topic_intrusion(ldaPosterior$terms, ldaPosterior$topics)

tm_topic_intrusion(ldaPosterior$terms, ldaPosterior$topics, chance_correction = F)

tm_topic_intrusion(ldaPosterior$terms, ldaPosterior$topics, document = 1)

