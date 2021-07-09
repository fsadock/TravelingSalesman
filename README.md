# TravelingSalesman


My solution to the Traveling Salesman Problem using Variable Neighborhood Search and decision theory.

The files '.csv' are data matrices provided simulating 250 cities.

In each matrix, line _i_ and column _j_ represent the distance - in Km (or time - in Hours) necessary to reach city _j_ from city _i_.

_TC.py_ contains all the code used.

The algorithm with best result is the GVNS (General Variable Neighborhood Search), but it takes a long time to run with 250 cities. I should try adding different neighborhoods or tweaking some other stuff, maybe there's a bug that I didn't catch.