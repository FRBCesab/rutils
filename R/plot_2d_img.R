#' Plot two variables using images png as points
#' 
#' @description
#' Plot two variables using images png as points.Works with any king of png, the best stratgegy 
#' is to use png with no backgrounds.
#' Parameter scale and size are important to tune to make the plot readable and not to heavy
#' Must be plotted (or saved) on a 1:1 dimension 
#' 
#' @param df a `data.frame` with three colums x coord,y coord,img_names
#' @param scale a numeric between 0 and 1 (used to resize the images)
#' @param size the parameter size of the function imager::resize (between -0L and -100L) used to reduce the resolution of the image (proportion between 0 and 100%)
#' @param pathimages path to the images
#' @param cexaxis cex.axis of the plot function 
#' @param cexlab cex.lab of the plot function 
#' @param labelx label of the x axis
#' @param labely label of the y axis
#' @param lm TRUE or FALSE (compute and draw the lm)
#' @param xR x coordinate of the position of the R2 on the plot
#' @param yR y coordinate of the position of the R2 on the plot
#' @param colR color of the R2 on the plot
#' @param cexR size of the R2 on the plot
#' @param colline col of the abline used to illustrate the lm 
#' @param lwline lwd of the abline used to illustrate the lm 
#' @param ltyline lty of the abline used to illustrate the lm 
#' 
#' @return a plot
#' 
#' @examples
#' set.seed(3)
#' df <- cbind.data.frame(x=runif(10, 0, 10),y=runif(10, 0, 10),img_names=lessR::to("img_", 10))
#' pathimages <- system.file("extdata", "plot2dimg", package = "rutils")
#' plot_2d_img <- function(df,scale=0.2,size=-35L,pathimages,cexaxis=1.5, cexlab=2.5,lm=TRUE,xR=1.5,yR=10,colR="gray",cexR=1.5,colline="gray",lwline=2,ltyline=2)
#' 
#' 


plot_2d_img <- function(df,scale,size,pathimages,cexaxis,cexlab,labelx,labely,lm,xR,yR,colR,cexR,colline,lwline,ltyline)
{
   # scale=0.2
   # size = -35L
   # pathimages <- here::here("inst","extdata","plot2dimg")
   # labelx="Variable x"
   # labely="Variable y"
   # cexaxis=1.5
   # cexlab=2.5
   # lm=TRUE
   # xR=1.5
   # yR=10
   # colR="gray"
   # cexR=1.5
   # colline="gray"
   # lwline=2
   # ltyline=2
  
  labx <- colnames(df)[1]
  laby <- colnames(df)[2]
  minx <- min(na.omit(df[,1]))
  maxx <- max(na.omit(df[,1]))*1.2
  miny <- min(na.omit(df[,2]))
  maxy <- max(na.omit(df[,2]))*1.2
  
  par(mar=c(8, 9, 4.1, 2.1),mgp=c(6,2,0))
  plot(c(0,0),type="n",xlim=c(minx,maxx),ylim=c(miny,maxy),xlab=labelx,ylab=labely,cex.lab=cexlab,cex.axis=cexaxis)
  box(lwd=3)

  
  xp=(abs(maxx-minx))*scale
  yp=(abs(maxy-miny))*scale
  
  for (i in 1:nrow(df)) {
    img <- imager::load.image(file.path(pathimages,paste0(df[i,3],".png")))
    img=imager::resize(img, size, size, size_z = -100L,size_c = -100L,interpolation_type =1L)
    img <- as.raster(img)
    img[img=='#FFFFFF']=NA  # pour forcer la transparence
    rasterImage(img, df[i,1], df[i,2], df[i,1]+xp, df[i,2]+yp,interpolate=TRUE)
    rm(img)
  }
  
  if (lm){
    abline(lm(reformulate("x","y"), data = df),lty=ltyline, lwd=lwline,col=colline)
    mods <- summary(lm(reformulate("x","y"), data = df))
    text(x=xR, y=yR, paste0("r2= ",round(mods$r.squared,2)),cex=cexR,adj=0,col=colR)
  }
  
}