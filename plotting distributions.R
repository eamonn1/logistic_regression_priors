

rm(list=ls())
require(LaplacesDemon)  # tdist function
require(ggplot2)
x_values <- seq(-15,15, length.out = 1000)
data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun=dst, args=list(nu=3,mu=0,sigma=2.5))


data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun = dt, args = list(df = 3))


 

x_values <- seq(-15,15, length.out = 1000)
data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun=dcauchy, args=list(location = 0, scale = 2.5))



x_values <- seq(-15,15, length.out = 1000)
data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun=dbeta, args=list(shape1 = 1, shape2=1))

data.frame(x_values) %>%
  ggplot(aes(x_values))+
  stat_function(fun=dunif, args=list(min = -10, max=10))


x_values <- seq(-20,20, length.out = 1000)

data.frame(x_values) %>%
  ggplot(aes(x_values))+
 
stat_function(fun=dnorm, args=list(mean=0,sd=5, log = FALSE), col='blue') +
  stat_function(fun=dst, args=list(nu=3,mu=0,sigma=2.5), col='green') +
stat_function(fun=dcauchy, args=list(location = 0, scale = 2.5),  col='red') +
labs( )

xs <- seq(-20,20, by=2)

xs2 <- seq(-4,4, by=2)
prob <- print(exp(xs2)/(1+exp(xs2)), digits=2)
prob <- round(prob,2)
#p2 <- function(x) {formatC(x, format="f", digits=2)}
 

##if (n =1) {

data.frame(x_values) %>%
ggplot(aes(x_values)) +
  stat_function(fun = dnorm,   args=list(mean=0      ,sd=5, log = FALSE), aes(colour = "N(mean=0, sd=2.5)")) + 
  stat_function(fun = dst,     args=list(nu=3,mu=0   ,sigma=2.5),        aes(colour = "t(3df, mean=0, scale=2.5)")) +
  stat_function(fun = dcauchy, args=list(location = 0, scale = 2.5),     aes(colour = "Cauchy(location=0, scale=2.5)")) +
  stat_function(fun = dnorm,   args=list(mean=0,sd=2 , log = FALSE),     aes(colour = "N(mean=0, sd=2)")) +
  stat_function(fun = dnorm,   args=list(mean=0,sd=1 , log = FALSE),     aes(colour = "N(mean=0, sd=1)")) +
   stat_function(fun = dst,     args=list(nu=x,mu=y  ,sigma=z), aes(colour = "t(3df, mean=0, scale=3)")) +
 
    
    
    
  scale_colour_manual("", values = c("red", "blue", "green", "black","darkgreen", "yellow"))  +
  labs(title="Distributions", 
       x = "log odds",
       y = "",
       subtitle =paste0(c("Note probabilites", prob," are equivalent to log odds: -4,-2, 0 ,2, 4 "), collapse=", "),
       caption = "") +
  guides(fill=FALSE) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(.96,.6)) +
  scale_x_continuous("log odds", breaks=xs, labels=xs, limits=c(-20,20)) 
 

#}

#paste(c("The first three notes are: ", notes), collapse=" ")



 
#group1...beta(1,1). group(2)...beta(10,43)

x <- rbeta(n=1000000, shape1= 10, shape2=55)
hist(log(x/(1-x)))
summary(log(x/(1-x)))
sd(log(x/(1-x)))
x <- rnorm(n=1000000,-0.8848,.25 )
hist((x))
summary(x)
 

x <- rbeta(n=1000000, shape1= 10, shape2=43)
hist(log(x/(1-x)))
summary(log(x/(1-x)))
sd(log(x/(1-x)))
x <- rnorm(n=1000000,-1.65,.36 )
hist((x))
summary(x)




