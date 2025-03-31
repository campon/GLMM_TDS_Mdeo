library(rstan)  
options(mc.cores = parallel::detectCores())  

source("TFG/ccz17_data.R") # compute data from ccz17
source("TFG/example-models-master/example-models-master/knitr/car-iar-poisson/nb_data_funs.R")

nbs <- nb2graph(W50.17);

N = nbs$N;
node1 = nbs$node1;
node2 = nbs$node2;
N_edges = nbs$N_edges;
y = data$y;
x = data$x;

pois_icar_stan = stan_model("TFG/example-models-master/example-models-master/knitr/car-iar-poisson/pois_icar.stan");
fit_pois_icar = sampling(pois_icar_stan, data=list(N,N_edges,node1,node2,y,x,E), chains=3, warmup=4000, iter=5000, save_warmup=FALSE, verbose = TRUE);

bernoulli_icar_stan = stan_model("TFG/bernoulli_icar.stan");
fit_logi_icar <- sampling(bernoulli_icar_stan, data=list(N=N,N_edges=N_edges,node1=node1,node2=node2,y=y,x=x), 
                          chains=1, warmup=500, iter=1000, save_warmup=FALSE, verbose = TRUE);


ofile = "icar_pois_stan_5K_iters.txt";
capture.output(print(fit_pois_icar, digits=3, probs=c(0.025, 0.975)),file=ofile);
