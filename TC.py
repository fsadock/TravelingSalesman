import random
import numpy as np
import multiprocessing as mp
import matplotlib.pyplot as plt
import time
import tqdm


# generates a random list of integers between (including) _a_ and _b_
def initialSol(a,b):
    x = list(range(a,b))
    random.shuffle(x)
    return x

# cost function
def fobj(citiesArray, matriz):
    cost = 0
    for i in range(len(citiesArray)-1):
        cost += matriz[citiesArray[i]][citiesArray[i+1]]
    cost+=matriz[citiesArray[len(citiesArray)-1]][citiesArray[0]] # adds time (or distance) between the first and last cities, since the loop above won't account for it
    return cost

# responsible for changing neighborhoods
def neighborhoodChange(x, x_lin, k, matriz):
    newCost = fobj(x_lin, matriz)
    oldCost = fobj(x, matriz)
    if  newCost < oldCost:
        x = x_lin           # if newCost is less than oldCost, restart the neighborhoods (k=1)
        k = 1
    else:                   # otherwise go to the next neighborhood (k+1)
        k = k+1
    return x,k

# 'shake' neighborhoods
def shake(x,k):
    i,j,u = random.sample(list(range(len(matriz))),3)
    y = x[:]

    if k == 1:                  # swap two random cities
        y[i], y[j] = y[j], y[i]

    elif k == 2:                # swap three random cities
        y[i], y[j], y[u] = y[j], y[u], y[i]

    elif k == 3:                # shift positions
        if i < j:
            y = y[i:j] + y[0:i] + y[j:len(x)]
        else:
            y = y[j:i] + y[0:j] + y[i:len(x)]
    elif k == 4:                # 2-opt
        if i < j:
            y = y[0:i] + y[i:j][::-1] + y[j:len(x)]
        else:
            y = y[0:j] + y[j:i][::-1] + y[i:len(x)]
    return y

# Returns the first improvement after permutating combinations of x - an array of route 
def firstImprovement(x, k, matriz):
    y = x[:]
    if k == 1:
        for i in range(len(y)):
            for j in range(i+1, len(y)):
                y[i], y[j] = y[j], y[i]
                if fobj(y, matriz) < fobj(x, matriz): # if y is better than x, search y ( FIRST IMPROVEMENT )
                    return firstImprovement(y, k=1, matriz=matriz)
                continue                    # if not better, continue searching the neighborhood until nothing is found                            
    elif k == 2:
        for i in range(len(y)):
            for j in range(i, len(y)):
                for u in range(j, len(y)):
                    if i != j and u!=j and i!=u:
                        y[i], y[j], y[u] = y[j], y[u], y[i]
                        if fobj(y, matriz) < fobj(x, matriz):
                            return firstImprovement(y, k=2, matriz=matriz)
                        continue
    return x                                  # if no other permutation is better return x

# Variable Neighborhood Descent
def VND(x , k_max, matriz):
    k = 1
    while k <= k_max:
        x_lin = firstImprovement(x, k, matriz)        
        x,k = neighborhoodChange(x, x_lin, k, matriz) # Compare x and x_lin
    return x

# Reduced Variable Neighborhood Search
def rvns(x, k_max, t_max, matriz):
    t = 0
    results = []
    pbar = tqdm.tqdm(total=t_max)
    while t<=t_max:
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            t+=1
            pbar.update(1)
            x, k = neighborhoodChange(x, x_lin, k, matriz)
        results.append(np.round(fobj(x, matriz)))
    pbar.close()
    return x, results

# Basic Variable Neighborhood Search
def bvns(x, k_max, t_max, matriz):
    t = 0
    results = []
    pbar = tqdm.tqdm(total=t_max)
    while t<=t_max:
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            x_dlin = firstImprovement(x_lin, k, matriz)
            x, k = neighborhoodChange(x, x_dlin, k, matriz)
        t+=1
        pbar.update(1)
        results.append(np.round(fobj(x, matriz)))
    pbar.close()
    return x, results

# General Variable Neighborhood Search
def gvns(x,l_max, k_max, t_max, matriz):
    t = 0
    results = []
    pbar = tqdm.tqdm(total=t_max)
    while t<=t_max:
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            x_dlin = VND(x_lin, l_max, matriz)
            x, k = neighborhoodChange(x, x_dlin, k, matriz)
        t+=1
        pbar.update(1)
        results.append(np.round(fobj(x, matriz)))
    pbar.close()
    return x, results

def main():

    arquivo = "time.csv"
    matriz0 = np.genfromtxt(arquivo, delimiter=',') # uses numpy to read the matrix from the csv file

    # a smaller distance matrix
    matriz1 = [
            [0, 2451, 713, 1018, 1631, 1374, 2408, 213, 2571, 875, 1420, 2145, 1972],
            [2451, 0, 1745, 1524, 831, 1240, 959, 2596, 403, 1589, 1374, 357, 579],
            [713, 1745, 0, 355, 920, 803, 1737, 851, 1858, 262, 940, 1453, 1260],
            [1018, 1524, 355, 0, 700, 862, 1395, 1123, 1584, 466, 1056, 1280, 987],
            [1631, 831, 920, 700, 0, 663, 1021, 1769, 949, 796, 879, 586, 371],
            [1374, 1240, 803, 862, 663, 0, 1681, 1551, 1765, 547, 225, 887, 999],
            [2408, 959, 1737, 1395, 1021, 1681, 0, 2493, 678, 1724, 1891, 1114, 701],
            [213, 2596, 851, 1123, 1769, 1551, 2493, 0, 2699, 1038, 1605, 2300, 2099],
            [2571, 403, 1858, 1584, 949, 1765, 678, 2699, 0, 1744, 1645, 653, 600],
            [875, 1589, 262, 466, 796, 547, 1724, 1038, 1744, 0, 679, 1272, 1162],
            [1420, 1374, 940, 1056, 879, 225, 1891, 1605, 1645, 679, 0, 1017, 1200],
            [2145, 357, 1453, 1280, 586, 887, 1114, 2300, 653, 1272, 1017, 0, 504],
            [1972, 579, 1260, 987, 371, 999, 701, 2099, 600, 1162, 1200, 504, 0],
        ]

    # another test matrix
    np.random.seed(42)
    matriz2 = np.random.randint(0, 30, (30,30))

    # sets the matrix you want to solve
    matriz = matriz0

    print("\nChosen Matrix:")
    print(matriz)
    print('\n')
    start_time = time.time()
    x = initialSol(0,len(matriz))    
    # print(x)

#### ---------- Failed attempt to parallelize the problem ---------- ####
    # pool = mp.Pool(mp.cpu_count())
    # resultsParallel = [pool.apply(gvns, args=(x,1,3,100,matriz))]
    # pool.close()
    
#### --------------------------------------------------------- ####

    # Reduced VNS takes a lot less computational time but does not give the best results
    # sol, results = bvns(x,k_max=4,t_max=1000, matriz=matriz)
    print(fobj(x, matriz))
    print()
    sol, results = gvns(x,l_max=2,k_max=3,t_max=100, matriz=matriz)
    print(sol)
    print()
    print(results[-1])
    
    print(f"--- {time.time() - start_time} seconds ---")
    
    plt.plot(results)
    # plt.plot(resultsParallel[0][1])
    # plt.show()

if __name__ == '__main__':
    main()
    
   


