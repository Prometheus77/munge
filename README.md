# munge
Generates synthetic data which mimics joint distributions of original data

# How to install
Within R, make sure that you have the devtools package installed. To check, type:
```
library(devtools)
```
If the command runs without event, then you have it installed. If, however, you get the message:
```
Error in library(devtools) : there is no package called ‘devtools’
```
...then you will need to install that package before loading it:
```
install.packages("devtools")
library(devtools)
```
Once you have completed the above, you may now install this package and load it into memory:
```
install_github(repo = "Prometheus77/munge")
library(munge)
```
In order to generate synthetic data, you simply need to feed an R data frame into the `munge()` function, like so:
```
iris_synth <- munge(iris)
```

For more information on what the `munge()` function does, please read the paper by Cristian Bucila, Rich Caruana, and Alexandru Niculescu-Mizil at: http://www.niculescu-mizil.org/papers/rtpp364-bucila.rev2.pdf. Note that this specific implementation takes the square root of the Hamming distance for discrete variables when calculating nearest neighbor distances in order to have them scaled roughly the same as continuous variables.
