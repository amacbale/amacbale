# Optimization with Simulated Annealing
# Weierstrass function


#install.packages("tictoc")
#library(tictoc)

# Initialize state and temperature
t = 50
init.state = runif(1)
t.min = 0.0001

# annealing schedule and epsilon neighborhood
alpha = .99
eps = .1

# storage vectors for plotting later
history = c()
temp.history = c(t)

# objective function
a = 3
k = 1:5
x = 1
energy <- function(x){
  (sin(pi*k[1]^a*x)/(k[1]*pi) + sin(pi*k[2]^a*x)/(k[2]^a*pi) + sin(pi*k[3]^a*x)/(k[3]^a*pi)
  + sin(pi*k[4]^a*x)/(k[4]^a*pi) + sin(pi*k[5]^a*x)/(k[5]^a*pi))
}

# simulated annealing alg
current.state = init.state
history = append(history,current.state)
#tic()
while(t > t.min){
  new.state = current.state + runif(1,-eps,eps)
  while(new.state > 1 | new.state < 0)
    new.state = current.state + runif(1,-eps,eps)
  if(energy(new.state) - energy(current.state) > 0){
    current.state <- new.state
  }
  else if(exp((energy(new.state)-energy(current.state))/t) > runif(1)){
    current.state <- new.state
  }
  history = append(history,current.state)
  t = t*alpha
  temp.history = append(temp.history,t)
}
#toc()

# demo
Plotz <- function(iter = length(history)) {
  curve(sin(pi*k[1]^a*x)/(k[1]*pi) + sin(pi*k[2]^a*x)/(k[2]^a*pi) + sin(pi*k[3]^a*x)/(k[3]^a*pi)
        + sin(pi*k[4]^a*x)/(k[4]^a*pi) + sin(pi*k[5]^a*x)/(k[5]^a*pi),
        x,from=0,to=1,n=1000,ylab = "f(x)",main = "Objective function")
  text(0.1,.33,labels = c("Temperature: "))
  text(0.1,.29,labels = round(temp.history[1],digits=4))
  Sys.sleep(1.5)
  for(i in 1:iter){
      curve(sin(pi*k[1]^a*x)/(k[1]*pi) + sin(pi*k[2]^a*x)/(k[2]^a*pi) + sin(pi*k[3]^a*x)/(k[3]^a*pi)
            + sin(pi*k[4]^a*x)/(k[4]^a*pi) + sin(pi*k[5]^a*x)/(k[5]^a*pi),
            x,from=0,to=1,n=1000,ylab = "f(x)",main = "Objective function")
      text(0.1,.33,labels = c("Temperature: "))
      text(0.1,.29,labels = round(temp.history[i],digits=4))
      abline(v=history[i])
    }
  }
x11()
Plotz()
current.state
energy(current.state)





