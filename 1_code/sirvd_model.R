# this is great but need to figure out MSM
## Core equations for transitions between compartments:
update(S) <- S - beta * S * (Is + Iv) / N + omega * R - nu * S + sigma * V
update(Is) <- Is + beta * S * (Is + Iv) / N - gamma * Is + epsilon
update(Iv) <- Iv + (1 - theta) * beta * V * (Is + Iv) / N - gamma_v * Iv 
update(R) <- R + (1 - mu) * gamma * Is + (1 - mu_v) * gamma_v * Iv - omega * R
update(V) <- V + nu * S - sigma * V - (1 - theta) * beta * V * (Is + Iv) / N
update(Ds) <- Ds + mu * gamma * Is
update(Dv) <- Dv + mu_v * gamma_v * Iv

## Total population size, and number of infecteds
I <- Is + Iv
D <- Ds + Dv
N <- S + I + R + V + D

## Initial states
initial(S) <- S_ini
initial(Is) <- I_ini
initial(Iv) <- 0
initial(R) <- 0
initial(V) <- 0
initial(Ds) <- 0
initial(Dv) <- 0

## User defined parameters - default in parentheses:
S_ini <- user(1000) # susceptibles
I_ini <- user(1) # infected
beta <- user(0.3) # infection rate
gamma <- user(0.08) # recovery rate
gamma_v <- user(0.08) # recovery rate for breakthroughs
mu <- user(0.7) # CFR
mu_v <- user(0.35) # CFR for breakthroughs
omega <- user(0.005) # rate of waning immunity
epsilon <- user(0.1) # import case rate
sigma <- user(0.01) # rate of waning immunity among vaccinated
nu <- user(0.005) # vaccination rate 
theta <- user(0.9) # vaccine efficacy
