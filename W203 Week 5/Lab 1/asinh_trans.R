# origin of the code below
# http://stackoverflow.com/questions/14504869/histogram-with-negative-logarithmic-scale-in-r

library(ggplot2)
library(scales)

limits <- 100
step <- 0.005
demo <- data.frame(x=seq(from=-1*limits,to=limits,by=step))

asinh_trans <- function(){
  trans_new(name = 'asinh', transform = function(x) asinh(x), 
            inverse = function(x) sinh(x))
}

ggplot(demo,aes(x,x))+geom_point(size=2)+
  scale_y_continuous(trans = 'asinh',breaks=c(-100,-50,-10,-1,0,1,10,50,100))+
  theme_bw()
# Further review needed