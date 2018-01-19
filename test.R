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
