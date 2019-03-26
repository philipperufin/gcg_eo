library(raster)
library(RStoolbox)
library(tidyverse)

mrch <- stack('//141.20.140.199/o/WS1819_MSc-EO/intern/S03/data/sr_data/preprocessed/LC081890252014031001T1-SC20170927101754_sr_masked_crop.envi')
mrch_matrix <- as.data.frame(mrch)

july <- stack('//141.20.140.199/o/WS1819_MSc-EO/intern/S03/data/sr_data/preprocessed/LC081890252014071601T1-SC20171024094741_sr_masked_crop.envi')
july_matrix <- as.data.frame(july)

agst <- stack('//141.20.140.199/o/WS1819_MSc-EO/intern/S03/data/sr_data/preprocessed/LC081890252015082001T1-SC20170927120710_sr_masked_crop.envi')
agst_matrix <- as.data.frame(agst)

nvmb <- stack('//141.20.140.199/o/WS1819_MSc-EO/intern/S03/data/sr_data/preprocessed/LC081890252014110501T1-SC20170927102137_sr_masked_crop.envi')
nvmb_matrix <- as.data.frame(nvmb)

n = 10000
mrch_matrix_na <- mrch_matrix[mrch_matrix > -1,]
mrch_sample <- sample_n(mrch_matrix_na, n)

july_matrix_na <- july_matrix[july_matrix > -1,]
july_sample <- sample_n(july_matrix_na, n)

agst_matrix_na <- agst_matrix[agst_matrix > -1,]
agst_sample <- sample_n(agst_matrix_na, n)

nvmb_matrix_na <- nvmb_matrix[nvmb_matrix > -1,]
nvmb_sample <- sample_n(nvmb_matrix_na, n)



points <- sampleRandom(mrch, n, xy=T, sp=T)

mrch_sample <- raster::extract(mrch, points)
july_sample <- raster::extract(july, points)
agst_sample <- raster::extract(agst, points)
nvmb_sample <- raster::extract(nvmb, points)

sample_df <- data.frame(rbind(mrch_sample, july_sample, agst_sample, nvmb_sample))
sample_df$Period <- c(rep('2014/03', n), rep('2014/07', n), rep('2014/08', n), rep('2014/11', n))

sample_df <- data.frame(rbind(mrch_sample, july_sample, nvmb_sample))
sample_df$Period <- c(rep('2014/03', n), rep('2014/07', n), rep('2014/11', n))

ggplot(sample_df, aes(x=Band.3, y=Band.4, color=Period)) +
  geom_point(alpha=0.3, shape=19) +
  scale_x_continuous('Red', limits=c(0,2000)) +
  scale_y_continuous('Near infrared', limits=c(0,6000)) +
  scale_color_manual('Month', values=c('goldenrod1', 'darkolivegreen', 'peru', 'sienna1')) +
  theme_bw()

ggsave('C:/Users/geo_phru/Documents/GCG_EarthObservation/docs/fig/s03_tc.png', last_plot(), width=5, height=5, dpi=300)

ggplot(sample_df, aes(x=Band.3, y=Band.4, color=Period)) +
  geom_point(alpha=0.3, shape=19) +
  scale_x_continuous('Red', limits=c(0,2000)) +
  scale_y_continuous('Near infrared', limits=c(0,6000)) +
  scale_color_manual('Month', values=c('goldenrod1', 'darkolivegreen', 'peru')) +
  facet_wrap('Period', nrow=1) +
  theme_bw()

ggsave('C:/Users/geo_phru/Documents/GCG_EarthObservation/docs/fig/s03_tc_seq.png', last_plot(), width=8, height=3, dpi=300)


march <- stack('//141.20.140.199/o/WS1819_MSc-EO/S03/data/sr_data/LC081890252015082001T1-SC20170927120710/LC081890252015082001T1-SC20170927120710_sr_masked_crop.tif')
ndvi <- (march[[4]] - march[[3]]) / (march[[4]] + march[[3]])
plot(ndvi)

### TC animation for Turkey

img_files <- list.files('M:/LandsatData/Landsat_Turkey/07_FORCE/level2/X0085_Y0066', pattern="BOA.dat$", full.names = T)
cld_files <- list.files('M:/LandsatData/Landsat_Turkey/07_FORCE/level2/X0085_Y0066', pattern="CLD.dat$", full.names = T)
names <- list.files('M:/LandsatData/Landsat_Turkey/07_FORCE/level2/X0085_Y0066', pattern="BOA.dat$")

base <- stack(img_files[1])

n = 5000
points <- sampleRandom(base, n, xy=T, sp=T)

plot(base[[4]])
plot(points, add=T)

saveGIF({
for (i in 1:length(img_files)){

  date <- substr(names[i], start=1, stop=8)
  img  <- stack(img_files[i])
  cld  <- stack(cld_files[i])
  
  values <- as.data.frame(raster::extract(img, points, layer=3, nl=2))
  clouds <- as.data.frame(raster::extract(cld, points, layer=3, nl=2))
  
  names(values) <- c('Band3', 'Band4')
  values <- values[clouds>0,]
  
  p <- ggplot(values, aes(x=Band3, y=Band4)) +
    geom_point(alpha=0.3, shape=19, color='darkolivegreen') +
    scale_x_continuous('Red', limits=c(0,3000)) +
    scale_y_continuous('Near infrared', limits=c(0,7000)) +
    ggtitle(date) +
    theme_bw()
  print(p)
  #ggsave(paste0('C:/Users/geo_phru/Desktop/TC_nIR_red/tc_', date, '.png'), last_plot(), width=4, height=4, dpi=300)
  
}
})



stretch <- matrix(data = c(0,5000,0,4000,0,1500), nrow = 3, ncol = 2, byrow=T)

for (i in 1:length(img_files)){
  
  date <- substr(names[i], start=1, stop=8)
  img  <- stack(img_files[i])
  
  ggRGB(img, r=4, g=5, b=3, stretch='lin', limits=stretch)
  ggsave(paste0('C:/Users/geo_phru/Desktop/TC_nIR_red/img_', date, '.png'), last_plot(), width=4, height=4, dpi=300)
  #png(paste0('C:/Users/geo_phru/Desktop/TC_nIR_red/img_', date, '.png'), width=4, height=4, units='in', res=300)
  #plotRGB(img, r=4, g=5, b=3, stretch='lin')
  #dev.off()
  
}

library('animation')

setwd('C:/Users/geo_phru/Desktop/TC_nIR_red/scatter/')
system("convert -delay 80 *.png example_1.gif")



