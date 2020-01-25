library(dplyr)
library(ggplot2)
library(ggpubr)

sin_oscillate <- function(x) {
  if(x == 0) return(0)
  return(sin(1/x))
} 

sin_smooth <- function(x)  sin(x^1.5*10)

p1 <- data.frame(x = seq(from=-0,to=1,length.out=10)) %>%
  ggplot(aes(x=x))+
  stat_function(fun = Vectorize(sin_smooth),n=1000)+
  theme_bw()+
  labs(title = expression(sin(10*x^{3/2})), y = "")

p2<- data.frame(x = seq(from=-0,to=1,length.out=10)) %>%
  ggplot(aes(x=x))+
  stat_function(fun = Vectorize(sin_oscillate),n=1000)+
  theme_bw()+
  labs(title = expression(sin(1/x)), y = "")


ggarrange(p2,p1) %>%
  ggexport(filename = "functions.png",
           width = 800,
           height = 600)


relu <- function(x) max(x,0) - max(x-1,0)
sigmoid <- function(x) 1/(1+exp(-x))
step <-function(x){
  if(x>0 & x<25) return(1)
  else return(0)
}

p1 <- data.frame(x = seq(from=-10,to=10,length.out=10)) %>%
  ggplot(aes(x=x))+
  stat_function(fun = Vectorize(relu),n=1000)+
  theme_bw()+
  labs(title = "max(x,0) - max(x-1,0)", y = "")

p2 <- data.frame(x = seq(from=-10,to=50,length.out=10)) %>%
  ggplot(aes(x=x))+
  stat_function(fun = function(x) sigmoid(x) - sigmoid(x-25),n=1000)+
  stat_function(fun = Vectorize(step),colour='blue',linetype=4)+
  theme_bw()+
  labs(title = "sigmoid(x)-sigmoid(x-25)", y = "")

ggarrange(p2,p1) %>%
  ggexport(filename = "nonsmooth.png",
           width = 800,
           height = 600)

