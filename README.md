# cemtool: The interactive cost-effectiveness model tool to build a simple Markov model in R

There are multiple software systems that can be used to build Markov models for cost-effectiveness analyses like TreeAge, Excel or R. Although there are numerous advantages to use R over the others, the biggest downside is the steep learning curve from R. The cemtool package aims to close this gap by introducing a step-by-step tool to guide users in building a default Markov models. The tool guides the user though the steps of the development of a Markov model and will present the final result using graphs and tables. The only prerequisite is that the user knows the structure of the model and the transition probabilities, no calculations or coding is required. 

The manual can be viewed here: https://stanwijn.github.io/portfolio/cemtool.pdf 

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

To save the results from the tool, type:
```
cemtool.env <- cemtool()
```

# Images:

Step 1: Define number of healthstates and names
![Step 1: Basic information](https://stanwijn.github.io/cemtool_image/step1.PNG)

Step 2: Define transition probabilities, costs and effects (QALY/ utility) for both strategies
![Step 2: Markov model input](https://stanwijn.github.io/cemtool_image/step2.PNG)

Step 3: OPTIONAL: Alter the transition probability matrix to enable backward probabilties
![Step 3: Transition probability matrix](https://stanwijn.github.io/cemtool_image/step3.PNG)

Step 4: Inspect the Markov trace and the final results. 
![Step 4: Results](https://stanwijn.github.io/cemtool_image/step4.PNG)

Flow of functions:

![Flow of functions](https://stanwijn.github.io/cemtool_image/flow.png)

--------
--------

<br>
<br>
<br>
<br>
<br><br>
<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>


We thank our testers:
S. Patel Msc
