#Getting familiar with R again
#Review of Basic Mathmatical Concepts and Introduction to R

#2.1 Variables
#A variable represents a variable quantity, for example,time t.
#It can take one of many values; for example, t = 1, t = 2, t = 3, and so on.
#t could be a real number, such as 1.5, or a complex number, such as 1 + 2i.
#an interval can be denoted like [a, b] or (a, b). We refer to these variables as continuous variables.


#2.1 Exercise
#Use a varaible X to denote the human population on earth. Explain why it varies and give an example of a value it could take.
#create and array of values for X from 1 to 7.5 billion
x <- 1:7500000000 

#2.2 Functions
#A function is a map that assigns value of a dependent variable to each value of an independent variable.
#For example, the function X(t) is a function X of t, where X is the dependant variable and t is the independent variable.

#X(t) = at

#here, the function is expressing that the varible X is proportional to the variable, where the constant of proportionality is the coefficient a.

#similarly, X(t) = at^b, where b is a constant, is a function of t. This would produce a porabola.
#We get a non-linear function when b is not equal to 1.

#finally, a hyperbolic function is X(t) = a/t+b, where a and b are constants. This would produce a hyperbola, which is a non-linear function that is asymptotic to the x and y axis.


