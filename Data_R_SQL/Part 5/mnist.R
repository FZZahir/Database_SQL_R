#Part 5d: Image Recognition - MNIST
#This is based on https://github.com/rstudio/keras/blob/master/vignettes/examples/mnist_cnn.R

#Note: I had to remove and reinstall reticulate to make this work, I was getting  a "no shape parameter" error

rm(list=ls(all=TRUE))

# Setup parameters
batch_size <- 128 # a parameter for DNN learning
num_classes <- 10 # there are 10 digits
epochs <- 12 # number of learning epochs
img_rows<-28
img_cols<-28
input_shape <- c(img_rows, img_cols, 1)
define.and.train.model<-T # F means we will load the model from a file

# we will need these later
model.file.name<-"c:/Temp/mnist_trained_1.h5"
#Changing the suffix from dl=0 to dl=1 allows reading this file directly from DropVox
own.digits.file.name<-'https://www.dropbox.com/s/58g32xai4z0sejn/myMNIST.csv?dl=1'

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

usePackage('ggplot2')
usePackage('reshape2')
usePackage('keras')

# initialize data directory
#http://yann.lecun.com/exdb/mnist/
mnist <- dataset_mnist()  

# Let's take a look
nrow(mnist$train$x)
mnist$train$x[1,,] # first "row"
mnist$train$x[1,1:28,1:11] 

nrow(mnist$train$y)
head(mnist$train$y) # the first 6 observations
table(mnist$train$y) # how many samples per digit

# convert training data intensities to 0-1 range
range01 <- function(M){(M-min(M))/(max(M)-min(M))}

mnist$train$x <- range01(mnist$train$x)
mnist$test$x <- range01(mnist$test$x)

# try plotting the pixel intensities for a random sample of 36 images
n <- 36
indices <- sample(nrow(mnist$train$x), size = n)
data <- mnist$train$x[indices,, ]
data <- array(aperm(data, c(1,3,2)), dim = c(n, 28, 28))
melted <- melt(data, varnames = c("image", "x", "y"), value.name = "intensity")
ggplot(melted, aes(x = x, y = y, fill = intensity)) +
  geom_tile() +
  scale_fill_continuous(name = "Pixel Intensity") +
  scale_y_reverse() +
  facet_wrap(~ image, nrow = sqrt(n), ncol = sqrt(n)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "MNIST Image Data",
    subtitle = "Visualization of a sample of images contained in MNIST data set.",
    x = NULL,
    y = NULL
  )


x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')
cat('train min:',min(x_train),' max:',max(x_train),'\n')
cat('test min:',min(x_test),' max:',max(x_test),'\n')

# Convert class vectors to binary class matrices
# one hot encode (OHE) classes / create DummyFeatures
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)


# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))

if(define.and.train.model){
  #use_session_with_seed(1,disable_gpu=F,disable_parallel_cpu = T,quiet=T)
  tensorflow::tf$random$set_seed(1) # reset the pseudo-random number generator for replicability
  
  # Define model
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'relu',
                  input_shape = input_shape) %>% 
    layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = 'relu') %>% 
    layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
    layer_dropout(rate = 0.25) %>% 
    layer_flatten() %>% 
    layer_dense(units = 128, activation = 'relu') %>% 
    layer_dropout(rate = 0.5) %>% 
    layer_dense(units = num_classes, activation = 'softmax')
  
  # Compile model
  model %>% compile(
    #loss = loss_categorical_crossentropy,
    loss = 'categorical_crossentropy',
    #loss = 'binary_crossentropy',
    optimizer = optimizer_adadelta(),
    metrics = c('accuracy')
  )
  
  #summary(model)
  
  # Train model
  model %>% fit(
    x_train, y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.2
  )
  
  #Save trained model
  save_model_hdf5(object=model,filepath=model.file.name)
} else{
  # Load model - THIS DOES NOT WORK FOR loss_categorical_crossentropy
  # This is why categorical_crossentropy is used
  model<-load_model_hdf5(filepath=model.file.name)
}

# Evaluate model (out of sample)
scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

# Output metrics (out of sample)
cat('Test loss (MNIST):', scores[[1]], '\n')
cat('Test accuracy (MNIST):', scores[[2]], '\n')


#####################################################
### Cover Basic Image Processing BEFORE PROCEEDING ##
#####################################################


# load own hand-written digits (pre-processed)
my_test<-read.csv(own.digits.file.name,row.names=1)
my_x_test<-my_test[,1:(ncol(my_test)-1),drop=F]
my_x_test<-array(as.matrix(my_x_test),dim = c(nrow(my_test), img_rows, img_cols))
my_x_test<-aperm(my_x_test,c(1,3,2))
my_y_test<-my_test[,ncol(my_test),drop=F]

#visualize some of own digits using the same parameters as MNIST
n <- 36
indices <- sample(nrow(my_x_test), size = n)
data <- my_x_test[indices,, ]
data <- array(aperm(data, c(1,3,2)), dim = c(n, img_rows, img_cols))
melted <- melt(data, varnames = c("image", "x", "y"), value.name = "intensity")
ggplot(melted, aes(x = x, y = y, fill = intensity)) +
  geom_tile() +
  scale_fill_continuous(name = "Pixel Intensity") +
  scale_y_reverse() +
  facet_wrap(~ image, nrow = sqrt(n), ncol = sqrt(n)) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Own Image Data",
    subtitle = "Visualization of a sample of images contained in the Own data set.",
    x = NULL,
    y = NULL
  )

# Redefine  dimension of own test inputs
my_x_test <- array_reshape(my_x_test, c(nrow(my_x_test), img_rows, img_cols, 1))
dim(my_x_test)
# use our classifier to predict labels for a subset of the test dataset
my_y_hat_test <- model %>% predict(my_x_test)

# transform to one-hot encoding (max score wins)
usePackage('matrixStats')
my_y_ohe<-(my_y_hat_test==rowMaxs(my_y_hat_test))
predictions<-max.col(my_y_ohe)-1

min(predictions)
max(predictions)

#test accuracy
accuracy.my<-sum(as.numeric(predictions==my_y_test))/nrow(my_y_test)

cat("My hand-written test accuracy: ",accuracy.my*100,"%\n",sep="")

# plot predictions versus actual for small subset
n <- 20
indices <- sample(nrow(my_x_test), n)
classes <- predictions[indices]
data <- my_x_test[indices,, ,1]
data <- array(aperm(data, c(1,3,2)), dim = c(n, img_rows, img_cols))
melted <- melt(data, varnames = c("image", "x", "y"), value.name = "intensity")
melted$class <- classes

image_labels <- setNames(
  sprintf("Predicted: %s\nActual: %s", classes, my_y_test$digit[indices]),
  1:n
)

ggplot(melted, aes(x = x, y = y, fill = intensity)) +
  geom_tile() +
  scale_y_reverse() +
  facet_wrap(~ image, ncol = 5, labeller = labeller(image = image_labels)) +
  theme(
    panel.spacing = unit(0, "lines"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = paste("Own Digits Image Data",'Accuracy=',round(accuracy.my,2)),
    subtitle = "Visualization of a sample of images from the own hand-written set",
    x = NULL,
    y = NULL
  )

#As you can see, the precision of recognizing 
#our set is much lower than the one for the test set in MNIST. 
#More processing (e.g. unskewing) could help.
