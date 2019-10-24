# PMFA
Base functions for performing a Probabilistic MFA using R

This work is licensed under the terms CC BY-NC-SA
More information on the license: https://creativecommons.org/licenses/#licenses

The following functions for the calculation of flows are part of this package:
- rtriang.perc: samples N values from a triangular probability distribution defined using a central value and a spread. Upper and lower cutoff can be used to truncate the distribution.
- rtrapez.perc: samples N values from a trapezoidal probability distribution defined using two central values and their corresponding spreads. Upper and lower cutoff can be used to truncate the distribution.
- import.input: function that creates probability distributions based on the input data in the Feed.xlsx example dataset.
- import.TC: function that creates probability distributions based on the transfer coefficient data in the Feed.xlsx example dataset.
- calc.rest.TC: function that calculates the probability distributions for transfer coefficients defined as the remaining fraction.
- normalize: function that normalizes transfer coefficient distributions together, so that the sum of the outflowing transfer coefficients from a same compartment is always equal to 1.
- solve.MC: function that performs the Monte-Carlo simulation. The result is the calculated mass in each compartment

The following two functions can be used to analyze the data
- find.fc: function that calculates what fraction of the mass of one compartment finishes in the final sinks (or whichever compartments are chosen to stop at). Useful for calculating how much of a product in use is finally recycled or ending up in the environment.
- sci.not: function that displays a probability distribution as MEAN +/- STANDARD DEVIATION. The standard deviation is rounded to two digits and the mean is rounded to the precision of the standard deviation. Please note that significant zeros could not be displayed in this version, so pay attention to the number of significant digits of the standard deviation, and from there, of the mean, if you would like to display the "significant zeros".

For a first test, we recommend running Model.R and starting the modifications from there. This script uses the above mentioned functions along with the Feed.xlsx input table.
