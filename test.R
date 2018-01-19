#### Question 1: Principal components ####
dat = read.table("C:/732A97 Multivariate Statistical Methods/lab/T1-9.dat", 

                 
install.packages("diversitree")
library(diversitree)
                 
set.seed(123)
pars = c(0.1, 0.1, 0, 0, 0.1, 0.1)
phy <- tree.bisse(pars,max.taxa=50, x0=NA)
plot(history.from.sim.discrete(phy, 0:1), phy, 
     show.tip.label=FALSE,
     main="True history")
axisPhylo()
                 
lik <- make.bisse(phy, phy$tip.state)
p = starting.point.bisse(phy)
fit = find.mle(lik, p)
coef(fit_bisse)
fit_bisse$lnLik
constant_lik = constrain(bisse_lik, lambda0 ~ lambda1, mu0 ~0,  mu1~0)                 
fit_constant = find.mle(constant_lik, p)
coef(fit_constant)
fit_constant$lnLik
 
  # tree filter
freq <- as.vector(table(phy$tip.state))
if((length(freq) == 1)){ 
if(any(freq/sum(freq) < 2/50) | (length(freq) == 1)){
  return(NA)
}                 
