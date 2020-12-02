import gensim.models
from gensim.models.wrappers import DtmModel
import gensim.corpora
import gensim.matutils
import numpy as np
import pickle 
corpus = pickle.load( open("collections/tmp/test_corpus","rb"))
dictionary = pickle.load( open("collections/tmp/test_dictionary","rb"))
num_topics = pickle.load( open("collections/tmp/num_topics","rb"))
time_slices = pickle.load( open("collections/tmp/time_slices","rb"))
path_to_dtm_binary = "global/dtm-linux64"
model = DtmModel(
  path_to_dtm_binary, corpus=corpus, id2word=dictionary,
  time_slices=time_slices,num_topics=num_topics,mode="fit"
)

