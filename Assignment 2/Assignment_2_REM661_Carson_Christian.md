Psuedocode Assignment 2 - Example of Taylor et al’s (2005) method for
estimating growth parameters in fish populations
================
02/13/2024

Taylor et al. (2005) developed a method for estimating growth parameters
in fish populations based on the von Bertalanffy growth model, which is
a widely used model for describing the growth of fish. The model is
given by:

\#equation for Bertalanffy growth model
$$L(a) = L_{\infty} (1 - e^{-K(a - t_0)})$$

- Where $(a)$, is the age of the fish

- $L(a)$ is the length of the fish at age $a$

- $L_{\infty}$ is the asymptotic length

- $K$ is the growth coefficient

- $t_0$ is the age at which the fish is of zero length.

# Parameters

Linf \<- 100 K \<- 0.5 t0 \<- -0.1 S \<- 0.8 mu \<- 55 sig \<- 15 CV \<-
0.1

Age \<- 1:20 x \<- seq( from=5, to=120, by=5 ) La \<- Linf \* ( 1-exp(
-K*( Age-t0 ))) sig_a \<- La*CV

Pa \<- S^( Age-1 )/sum( S^( Age-1 ))

sx \<- 1 / ( 1 + exp( -( x-mu )/sig ))

P_x_a \<- matrix( nrow=length(x), ncol=length(Age) ) Z \<- vector(
length=length(x) )

for( i in 1:length(x) ){ P_x_a\[i,\] \<- pnorm( x\[i\]+2.5, mean=La,
sd=sig_a ) - pnorm( x\[i\]-2.5, mean=La, sd=sig_a )

Z\[i\] \<- sum( Pa \* sx\[i\] \* P_x_a\[i,\] ) }

Z \<- Z / sum( Z )

barplot( height=Z, names=x, col=“blue”, border=“red”, xlab=“Length bin
(cm)”, ylab=“Probability”)

LenSamp \<- sample(x=x, size=5000, replace=TRUE, prob=Z )

hist( LenSamp, xlim=range(x), col=“blue”, border=“red” )