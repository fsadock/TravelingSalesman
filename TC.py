import random
import numpy as np
import multiprocessing as mp
import matplotlib.pyplot as plt
import time


def setOfX(x):
    if len(set(x)) != len(x):
        print("##### ATENTION!!!!!!!!!!!!!!! #####")
        return True
    return False

# gera uma lista aleatoria de inteiros entre inclusivo _a_ e exclusivo _b_
def initialSol(a,b):
    x = list(range(a,b))
    random.shuffle(x)
    return x

def fobj(x, matriz):
    custo = 0
    for i in range(len(x)-1):
        custo += matriz[x[i]][x[i+1]]
    custo+=matriz[x[len(x)-1]][x[0]]
    return custo

def neighborhoodChange(x, x_lin, k, matriz):
    # print("### ---------------Iniciando o neighborhoodChange!--------------- ###")
    custoNovo = fobj(x_lin, matriz)
    custoVelho = fobj(x, matriz)
    if  custoNovo < custoVelho:
        x = x_lin           # se for menor, começa no k=1 para x_lin
        k = 1
    else:                   # caso contrario passa para o proximo k
        k = k+1
    return x,k

def shake(x,k):
    # print("### ---------------Iniciando o shake!--------------- ###")
    i,j,u = random.sample(list(range(len(matriz))),3)
    y = x[:]

    if k == 1:                  # troca duas cidades aleatorias de lugar
        y[i], y[j] = y[j], y[i]

    elif k == 2:                # troca tres cidades aleatorias de lugar
        y[i], y[j], y[u] = y[j], y[u], y[i]

    elif k == 3:                # shift positions
        if i < j:
            y = y[i:j] + y[0:i] + y[j:len(x)]
        else:
            y = y[j:i] + y[0:j] + y[i:len(x)]
    elif k == 4:
        if i < j:
            y = y[0:i] + y[i:j][::-1] + y[j:len(x)]
        else:
            y = y[0:j] + y[j:i][::-1] + y[i:len(x)]
    return y

def firstImprovement(x, k, matriz):
    # print("### ---------------Iniciando o FirstImprovement!--------------- ###")
    y = x[:]
    if k == 1:
        for i in range(len(y)):
            for j in range(i, len(y)):
                if i != j:
                    y[i], y[j] = y[j], y[i]
                    if fobj(y, matriz) < fobj(x, matriz): # if y is better than x, return y ( FIRST IMPROVEMENT )
                    # print("### --------------- Houve melhora :) --------------- ###")
                        return y
                    y = x[:]                              # else: sets y = x to proceed to the next permutation
    elif k == 2:
        for i in range(len(y)):
            for j in range(i, len(y)):
                for u in range(j, len(y)):
                    if i != j and u!=j and i!=u:
                        y[i], y[j], y[u] = y[j], y[u], y[i]
                        if fobj(y, matriz) < fobj(x, matriz):
                            return y
                        y = x[:]
        # print("### ---------------Não houve melhora :( --------------- ###")
    return x                                  # if no other permutation is better than the original, returns x

def VND(x , k_max, matriz):
    k = 1
    while k <= k_max:
        x_lin = firstImprovement(x, k, matriz)
        x,k = neighborhoodChange(x, x_lin, k, matriz) # Compara x com x_lin
    return x

def rvns(x, k_max, t_max, matriz):
    t = 0
    results = []
    while t<=t_max:
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            t+=1
            x, k = neighborhoodChange(x, x_lin, k, matriz)
        results.append(np.round(fobj(x, matriz)))
    return x, results

def bvns(x, k_max, t_max, matriz):
    t = 0
    results = []
    while t<=t_max:
        print(f"### --------------- {t} --------------- ###")
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            x_dlin = firstImprovement(x_lin, k, matriz)
            x, k = neighborhoodChange(x, x_dlin, k, matriz)
        t+=1
        results.append(np.round(fobj(x, matriz)))
    return x, results

def gvns(x,l_max, k_max, t_max, matriz):

    t = 0
    results = []
    while t<=t_max:
        print(f"\n### --------------- {t} --------------- ###")
        k = 1
        while k <= k_max:
            x_lin = shake(x,k)
            x_dlin = VND(x_lin, l_max, matriz)
            x, k = neighborhoodChange(x, x_dlin, k, matriz)
        t+=1
        results.append(np.round(fobj(x, matriz)))
    return x, results


# global lenMatrix
arquivo = "e:\\Google Drive\\UFMG\\2021.1\\Teoria da Decisao\\TC\\distancia.csv"
matriz0 = np.genfromtxt(arquivo, delimiter=',')
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

np.random.seed(42)
matriz2 = np.random.randint(30, 2000, (30,30))

matriz = matriz0
print(matriz)

if __name__ == '__main__':

    
    start_time = time.time()
    
    # lenMatrix = len(matriz)
    x = initialSol(0,len(matriz))    
    # print(x)

#### ---------- Tentativa de paralelizar o problema ---------- ####
    # pool = mp.Pool(mp.cpu_count())
    # resultsParallel = [pool.apply(gvns, args=(x,1,3,100,matriz))]
    # pool.close()
    
#### --------------------------------------------------------- ####

    # sol, results = rvns(x,k_max=3,t_max=5000, matriz=matriz)
    sol, results = gvns(x,l_max=1,k_max=4,t_max=100, matriz=matriz)
    print(sol)
    print(results)
    
    print("--- %s seconds ---" % (time.time() - start_time))
    
    plt.plot(results)
    # plt.plot(resultsParallel[0][1])
    plt.show()
   


