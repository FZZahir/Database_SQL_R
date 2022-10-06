#Part 5e: Basic Image Processing
#This is based on https://rrighart.github.io/Digits/
#It shows a basic way of transforming images into an input to a ML algorithm 

rm(list=ls(all=TRUE))

usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

#You can replace the file used here with your own
#I encourage you to do it
#If you do, you will need to adjust scaling and cropping

usePackage('jpeg')
myurl <- "https://raw.githubusercontent.com/RRighart/Digits/master/HandwrittenDigits.JPG" 
z <- tempfile()
download.file(myurl,z,mode="wb")
img <- readJPEG(z,native=F)
file.remove(z)


#This below is one-time
#usePackage('BiocManager');BiocManager::install("EBImage") #update all packages
#https://bioconductor.org/packages/release/bioc/html/EBImage.html
usePackage("EBImage")

#What is this img? A three-channel (RGB) array of 
img[1:3,1:3,1]
img[1:3,1:3,2]
img[1:3,1:3,3]

#show - may take a while
par(mfrow=c(1,1),
    oma = c(0.5,0.5,0.5,0.5) + 0.1,
    mar = c(0,0,0,0) + 0.1)
image(t(apply(img[c(1:dim(img)[1]), c(1:dim(img)[2]), 1], 2, rev)), col=grey.colors(255), axes=F, asp=1)
mtext("Whole image of handwritten digits", cex=0.6, col="red")

#MNIST requires 28*28 pixels per digit
#The example from the blog has 42 usable columns and 56 usable rows of data
#42*28=1176 and 56*28=1568 pixels

ximg<-img[c(1:dim(img)[1]), c(1:dim(img)[2]), 1]
nhsq=42
pix=28
nimg <- resize(ximg, h = nhsq*pix)
dim(nimg)
#truncate to 56 rows
nimg<-nimg[1:1568, ]
dim(nimg)

#segmenting images based on the squares
matsplitter<-function(M, r, c) {
  rg <- (row(M)-1)%/%r+1
  cg <- (col(M)-1)%/%c+1
  rci <- (rg-1)*max(cg) + cg
  N <- prod(dim(M))/r/c
  cv <- unlist(lapply(1:N, function(x) M[rci==x]))
  dim(cv)<-c(r,c,N)
  cv
} 

nimg<-nimg[c(1:dim(nimg)[1]), ]
dat<-matsplitter(nimg, 28, 28) # it will take a moment
class(dat) #an array
dim(dat) #a three-dimensional array

#apply labels
labels=rep(c(NA, rep(seq.int(0, 9, by=1),4), NA), 56)
table(labels) #to check

#remove NAs
ndat<-dat[,,which(!is.na(labels))]
nlabels<-labels[which(!is.na(labels))]

#show some
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in sample(length(nlabels),10)){
  #i<-1
  #t(apply(ndat[, ,i], 2, rev))[1:5,1:5]
  image(t(apply(ndat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
  #image(t(apply(ndat[, ,i], 2, rev)), col=heat.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)
  }

#Create negatives
neg <- function(M,i){
  apply(M, 3, max)[i]-M[,,i]
}

mmat<-array(0,dim=dim(ndat))

for(i in 1:dim(ndat)[3]){
  mmat[,,i]<-neg(ndat,i)
}

#show some negatives
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in sample(length(nlabels),10)){
  image(t(apply(mmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)
  #image(t(apply(mmat[, ,i], 2, rev)), col=heat.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)
}

#Show average images to inspect the amount of noise
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
  tm<-apply(mmat[,,which(nlabels==i)], c(1,2), mean)
  image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)  
  #image(t(apply(tm, 2, rev)), col=heat.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)  
}

#Show pixel-wise standard deviation
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
  tm<-apply(mmat[,,which(nlabels==i)], c(1,2), sd)
  image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)
}

#Scale image intensities
range01 <- function(M){(M-min(M))/(max(M)-min(M))}

scmat<-array(0,dim=dim(mmat))
for(i in 1:dim(mmat)[3]){
  scmat[,,i]<-range01(mmat[,,i])
}

#check scaling
apply(scmat[,,sample(length(nlabels),10)], 3, min)
apply(scmat[,,sample(length(nlabels),10)], 3, max)

#show some scaled negatives
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in sample(length(nlabels),10)){
  image(t(apply(scmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}

#remove background noise using a threshold
thresh <- function(M){ifelse(M<0.2, 0, M)}
thmat<-thresh(scmat)


#show some scaled denoised negatives
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in sample(length(nlabels),10)){
  image(t(apply(thmat[, ,i], 2, rev)), col=grey.colors(255), axes=F, asp=1);   mtext(nlabels[i], cex=0.8, col="red", side=3, line=-1)  
}


#center the images
bmat<-array(0,dim=dim(thmat))
for(i in 1:dim(thmat)[3]){
  temp<-thmat[,,i]
  w<-temp[apply(temp,1,mean)>0,apply(temp,2,mean)>0]
  if(is.null(dim(w))) next
  if(dim(w)[1]<4) next
  if(dim(w)[2]<4) next
  if(dim(w)[1]>26) next
  if(dim(w)[2]>26) next
  bim<-matrix(rep(0,28*28),nrow=28)
  ly=floor(((dim(bim)[1]-dim(w)[1])/2)+0.5)
  uy=ly+dim(w)[1]-1
  lx=floor(((dim(bim)[2]-dim(w)[2])/2)+0.5)
  ux=lx+dim(w)[2]-1
  bim[c(ly:uy),c(lx:ux)]<-w
  bmat[,,i]<-bim
}

#show averages of scaled denoised centered negatives
par(mfrow=c(2,5),
    oma = c(3,3,3,3) + 0.1,
    mar = c(0,0.1,0,0.1) + 0.1)
for(i in 0:9){
  tm<-apply(bmat[,,which(nlabels==i)], c(1,2), mean)
  image(t(apply(tm, 2, rev)), col=grey.colors(255), axes=F, asp=1); mtext(i, cex=0.8, col="red", side=3, line=-1)  
}

#Better plot - random m*m digits
#This is based on https://github.com/rstudio/tfestimators/blob/master/vignettes/examples/mnist.R
usePackage('ggplot2')
usePackage('reshape2')

n <- 9*9
indices <- sample(length(nlabels), size = n)
data <- bmat[, ,indices]
data <- array(aperm(data, c(3,2,1)), dim = c(n, 28, 28))
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
    title = "Image Data",
    subtitle = "Visualization of a sample of images contained in the data set.",
    x = NULL,
    y = NULL
  )

#convert the R array to a matrix (data frame)
ownset<-aperm(bmat, c(3,2,1))
dim(ownset)<-c(dim(bmat)[3],28*28)
ownset<-data.frame(ownset)

ownset[1:2,c(1:4,780:784)]

#attach labels
ownset$digit<-nlabels

ownset[1:2,c(1:5,780:785)]
nrow(ownset)

#Use another filter - remove rows with mean intensity < 0.01
ownset<-ownset[which(rowMeans(ownset[,1:(ncol(ownset)-1)])>=0.01),]
nrow(ownset)

#Save as csv
write.csv(x=ownset,file='C:/Temp/myMNIST.csv')

#Next the file is used to on the model trained using the MNIST
#NOTE: set define.and.train.model<-F