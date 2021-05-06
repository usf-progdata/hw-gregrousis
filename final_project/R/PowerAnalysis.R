library(WebPower)
library(lavaan)
library(simsem)


##gender = x, communal = m, EI = y; values pulled from Helm et al., 2018
mod ="
		y ~ cp*x + start(-.074)*x + b*m + start(-.026)* m
		m ~ a*x + start(0.24)*x
		x ~~ start(1)*x
		m ~~ start(1)*m
		y ~~ start(1)*y
	"
#To specify the indirect effects
mediation = "
		ab := a*b
		abc:= a*b + cp
" 
#Power for mediation based on MC method when bootstrap method is
##used to test the effects:
mediation.boot = wp.mc.sem.boot(model=mod, 
                                indirect=mediation, 
                                nobs=389,
                                nrep=1000, 
                                nboot=2000, 
                                parallel="parallel")

#To print the power analysis results
summary(mediation.boot)


#using simsem
##gender = x, communal = m, EI = y; values pulled from Helm et al., 2018
mod2 ="
		y ~ cp*x + start(-.074)*x + b*m + start(-.026)* m
		m ~ a*x + start(0.24)*x
		x ~~ start(1)*x
		m ~~ start(1)*m
		y ~~ start(1)*y
		
		#indirect
		ab := a*b
		abc := a*b + cp
	"

# specify an analysis model
path <- '
m ~ x
y ~ cp*x + m 
'

# generate data and summarize the outputs of the analysis model
sim.output <- sim(nRep = 1000, 
                  model=path, 
                  n=389, 
                  generate=mod2, 
                  seed=54321, 
                  lavaanfun="sem")

summary(sim.output)

# generate data and summarize the outputs of the analysis model
sim.output2 <- sim(nRep = 1000, 
                  model=mod2, 
                  n=389, 
                  seed=54321, 
                  lavaanfun="sem")

summary(sim.output2)


library(shiny)
runGitHub("mc_power_med", "schoam4")


#Example: Multiple Group Mediation Analysis (Moderated Mediation)

ex3model <- "
		y ~ start(c(0.283, 0.283))*x + c(c1,c2)*x + start(c(0.36, 0.14))*m + c(b1,b2)*m
		m ~ start(c(0.721, 0.721))*x + c(a1,a2)*x
		m =~ c(1,1)*m1 + start(c(0.8, 0.8))*m2 + start(c(0.8, 0.8))*m3
		x ~~ start(c(0.25, 0.25))*x
		y ~~ start(c(0.81, 0.95))*y
		m ~~ start(c(0.87, 0.87))*m
		m1 ~~ start(c(0.36, 0.36))*m1
		m2 ~~ start(c(0.36, 0.36))*m2
		m3 ~~ start(c(0.36, 0.36))*m3
"

# med1 and med2 are the mediation effect for group1 and group2, respectively.
indirect <- "
		med1 := a1*b1
		med2 := a2*b2
		diffmed := a1*b1 - a2*b2
"

bootstrap <- wp.mc.sem.boot(ex3model, indirect, nobs=c(400,200),
                            nrep=2000, nboot=1000, parallel='parallel')
summary(bootstrap) 