#Part 5b: Deep Learning with Keras (TensorFlow)
#TESTING if TensorFlow works using a simple classification model
#This code was tested on R version 4.1.2 
#Note: it may not be easy to install Keras 
#       if you have a previous version of
#       Miniconda or Anaconda (Python) installed
# https://keras.io/

rm(list=ls(all=TRUE))

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

initializeKeras<-function(first.time=F){
  
  if(Sys.info()["sysname"]=="Windows") {
    usePackage("reticulate")
  }
  
  usePackage("keras")
  if(first.time){
    reticulate::install_miniconda()
    library(keras);keras::install_keras()
  }
  
  py_discover_config()
  py_available(initialize = FALSE)
  import("numpy")
  import("keras")
  
  stopifnot(py_module_available('keras'))
  
} #initializeKeras

#If this is the first time you run Keras, then run:
# initializeKeras(first.time=T)
# Answer [Y]es if asked to install Miniconda
# otherwise, run the code below - you may see a lot of warnings (in red)

initializeKeras()
usePackage('matrixStats')

#SIMPLE TEST TO MAKE SURE THAT KERAS WORKS
tensorflow::tf$random$set_seed(1) # reset the pseudo-random number generator for replicability

set.seed(1) # reset the pseudo-random number generator for replicability
data = iris[sample(nrow(iris)),] #shuffle

y.cat = data[, "Species"]
x.raw = data[,1:4]

# scale to [0,1] - this is a common transformation for NN
scale <- function(x){
  (x-min(x.raw))/(max(x.raw) - min(x.raw))
}

x.transf = as.matrix(apply(x.raw, 2, scale))

# one hot encode classes
y.transf = to_categorical(as.integer(y.cat) - 1)

# withhold the first 10 for testing accuracy
# since we had shuffled this is the same as random
x=x.transf[11:nrow(x.transf),]
y=y.transf[11:nrow(y.transf),]

# a 5-layer deep-learning model specification
model = keras_model_sequential() # for example
# https://keras.io/api/layers/
model %>%
  layer_dense(input_shape = ncol(x), units = 10, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dropout(0.2) %>%
  layer_dense(units = 3, activation = "softmax")

#categorization-specific compilation
# https://keras.io/api/losses/
# https://keras.io/api/optimizers/

model %>%
  compile(
    loss = "categorical_crossentropy",
    optimizer = "adagrad",
    metrics = "accuracy"
  )

summary(model)

#https://github.com/satijalab/seurat/issues/958

# stop after 5 epochs with no improvement
callbacks = list(keras::callback_early_stopping(patience=5,mode="auto"))

# deep neural network model fitting  
fit = model %>%
  keras::fit(
    x = x,
    y = y,
    shuffle = T,
    validation_split = 0.3,
    epochs = 200,
    callbacks=callbacks,
    batch_size = 5
  )

# https://keras.io/api/metrics/

# graphical evaluation
fit %>% plot() # what can you see on this plot?

# numeric evaluation
model %>% evaluate(x, y)

# Use the trained dnn to predict
new.x<-x.transf[1:10,1:4]
new.y<-model %>% predict(new.x)

# transform to one-hot encoding (max score wins)
new.y.ohe<-(new.y==rowMaxs(new.y))

# transform the output values back to categories 
new.y.hat<-levels(y.cat)[max.col(new.y.ohe)]

cat("Predicted categories:",max.col(new.y.ohe),"\n")
y.hat<-y.cat[1:10]
cat("Actual categories:",y.hat,"\n")
accy<-length(which(new.y.hat==y.hat))/length(y.hat)
cat("Test accuracy: ",accy*100,"%\n",sep="")
