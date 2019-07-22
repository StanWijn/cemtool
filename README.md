# cemtool: The interactive cost-effectiveness model tool to build a simple Markov model in R

There are multiple software systems that can be used to build Markov models for cost-effectiveness analyses like TreeAge, Excel or R. Although there are numerous advantages to use R over the others, the biggest downside is the steep learning curve from R. The cemtool package aims to close this gap by introducing a step-by-step tool to guide users in building a default Markov models. The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. The manual can be viewed here: https://stanwijn.github.io/portfolio/cemtool_0.3.pdf 

--------

## To install 'cemtool' in R:
 
 First, you need to install the devtools package. You can do this from CRAN. Start R and then type 
 ```
 install.packages("devtools")
 ```
 Load the devtools package.
 ```
 library(devtools)
 ```
 Type:
 
 ```
 install_github("StanWijn/cemtool")
 ```
 You can ignore any warning messages.
 
  
 To load the package, type:
 ```
 library(cemtool)
 ```

To start the tool, type: 

```
cemtool()
```


<br>
<br>
<br>
<br>
<br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>


We thank our testers:
S. Patel Msc
