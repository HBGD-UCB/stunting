
library(png)
library(grid)
library(gridExtra)

comb2pngs <- function(imgs){
  img1 <-  grid::rasterGrob(as.raster(readPNG(imgs[1])),
                            interpolate = T)
  img2 <-  grid::rasterGrob(as.raster(readPNG(imgs[2])),
                            interpolate = T)

  return(grid.arrange(img1, img2, ncol = 2, top = "Unadjusted estimates                                                                      Adjusted estimates"))
}


setwd("C:/Users/andre/Downloads/sprint_7D_longbow-master (1)/sprint_7D_longbow-master/")

dir<-getwd()

plot_names <- list.files(path=paste(dir,"/unadjusted_binary/batch_report/risk_factor_report_files/figure-html/", sep=""))

#Get an image dimensions
img1<-readPNG("unadjusted_binary/batch_report/risk_factor_report_files/figure-html/paf_forest-1.png")
h<-dim(img1)[1]
w<-dim(img1)[2]


for(i in 1:length(plot_names)){
  png(paste0("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/combined unadj and adj/comb_",plot_names[i]), width=w*2, height=h)
    
  #save file paths
  png1_dest <- paste0("unadjusted_binary/batch_report/risk_factor_report_files/figure-html/", plot_names[i])
  png2_dest <- paste0("adjusted_binary/batch_report/risk_factor_report_files/figure-html/", plot_names[i])
  
  #print combined plots
  comb2pngs(c(png1_dest, png2_dest))
  
  dev.off()
}