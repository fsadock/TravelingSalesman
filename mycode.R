
# Função objetivo ####
ff = function(x,matriz){
  aux = c(x[2:length(x)],1)
  sm = 0
  
  for(i in 1:length(x)){
    sm = sm+matriz[[x[i],aux[i]]]
  }
  return(sm)
}



# Gera da estrutura de vizinhança 1 ####
shake_1=function(x,k){
  N=length(x)
  auxx = x
  p = sample(2:(N-k+1),size = 1)
  auxx[p:(p+(k-1))] = sample(x[p:(p+(k-1))],size = k, replace = FALSE)
  return(auxx)
}

# Gera da estrutura de vizinhança 2 ####
shake_2= function(x,k){
  N=length(x)
  auxx = x
  p = sample(2:N,size = k, replace = F)
  auxx[p] = sample(x[p],size = k, replace = FALSE)
  return(auxx)
}

# Gera da estrutura de vizinhança 3 ####

shake_3= function(x,k){
  N=length(x)
  auxx = x
  p = sample(2:(N-(k-1)),size = 1, replace = F)
  auxxx = auxx[(p:(p+k-1))]
  auxx = auxx[-(p:(p+k-1))]
  r = sample(2:(N-k),size = 1)
  return(c(auxx[1:(r-1)],auxxx,auxx[(r):(N-k)]))
}

# Gera da estrutura de vizinhança 4 ####
shake_4 = function(x,k){
  N=length(x)
  auxx = x
  
  p = sample(2:N,size = k, replace = F)
  
  auxxx = auxx[-p]
  
  r = sample(2:(N-k),size = 1)
  return(c(auxxx[1:(r-1)],auxx[p],auxxx[(r):(N-k)]))
  
}

# shake geral ####
shake = function(x_shake,k_shake,tipo){
  if(tipo == 1){
    return(shake_1(x=x_shake,k=k_shake))
  }else if(tipo==2){
    return(shake_2(x=x_shake,k=k_shake))
  }else if(tipo==3){
    return(shake_3(x=x_shake,k=k_shake))
  }else if(tipo==4){
    return(shake_4(x=x_shake,k=k_shake))
  }else{return(NULL)}
}

# Função de mudança de vizinhança ####
neighb_chang = function(x,x_lim,k,matt){
  if(ff(x_lim,matriz=matt)<ff(x,matriz = matt)){
    x=x_lim
    k=2
  }else{
    k=k+1
  }
  return(list(x,k))
}

# RVNS ####
rvns=function(x_rvns,kmax_rvns,tmax_rvns,matrr_rvns,vizz_rvns){
  
  evol_rvns = ff(x=x_rvns,matriz=matrr_rvns)
  t_rvns=0
  while(t_rvns<tmax_rvns){
    k_rvns = 2
    while(k_rvns<kmax_rvns){
      xl_rvns = shake(x_shake=x_rvns,k_shake=k_rvns,tipo=vizz_rvns)
      aux = neighb_chang(x=x_rvns,x_lim = xl_rvns,k=k_rvns,matt = matrr_rvns)
      x_rvns= aux[[1]]
      k_rvns=aux[[2]]
      evol_rvns=append(evol_rvns,ff(x=x_rvns,matriz = matrr_rvns))
    }
    t_rvns=t_rvns+1
    
  }
  return(list(x_rvns,evol_rvns))
}

tempo <- as.matrix(read.csv('tempo.csv', header = F))

# Para usar essa função:
# x_rvns é a solução inicial do RVNS, que será otimizada
# kmax_rvns é o tamanho da vizinhança
# tmax_rvns é o número máximo de interações permitido, quanto maior ser, será melhor
# será a solução
# matrr_rvns é a matriz, pode ser o tempo ou a distacia.
# vizz_rvns é o tipo de estrutura de vizinhança

ss=rvns(x_rvns =1:(nrow(tempo)),kmax_rvns = 200,tmax_rvns=500,matrr_rvns =tempo,vizz_rvns = 4)


###


# First Improvement ####
first_improv = function(a,kk,matrrr,card_max,vizz_fi){
  cond = TRUE
  while(cond){
    a_lim = a
    i=0
    cond2=TRUE
    while(cond2){
      ai=shake(x_shake=a_lim,k_shake=kk,tipo = vizz_fi)
      i= i+1
      if(ff(x=ai,matriz = matrrr)<ff(x=a,matriz = matrrr)){
        a = ai
      }
      
      if((ff(x=a,matriz = matrrr)<ff(x=a_lim,matriz = matrrr))|i==card_max){
        cond2=FALSE
        }
    }
    if(ff(x=a,matriz = matrrr)>=ff(x=a_lim,matriz = matrrr)){cond=FALSE}
  }
  return(a_lim)
}



# BVNS ####

# Para usar essa função:
# x_bvns é a solução inicial do BVNS, que será otimizada
# kmax_bvns é o tamanho da vizinhança
# tmax_bvns é o número máximo de interações permitido, quanto maior ser melhor
# será a solução
# matrr_bvns é a matriz, pode ser o tempo ou a distacia.
# cc_bvns é o número máximo de interações do firt improvemnet
# vizz_rvns é o tipo de estrutura de vizinhança

bvns=function(x_bvns,kmax_bvns,tmax_bvns,matrr_bvns,cc_bvns,vizz_bvns){
  
  evol_bvns = ff(x=x_bvns,matriz=matrr_bvns)
  t_bvns=0
  
  while(t_bvns<tmax_bvns){
    k_bvns = 2
    while(k_bvns<kmax_bvns){
      xl_bvns = shake(x_shake=x_bvns,k_shake=k_bvns,tipo = vizz_bvns)
      xll_bvns = first_improv(a=xl_bvns,kk=k_bvns,matrrr = matrr_bvns,card_max=cc_bvns,vizz_fi = vizz_bvns)
      aux = neighb_chang(x=x_bvns,x_lim = xll_bvns,k=k_bvns,matt = matrr_bvns)
      x_bvns= aux[[1]]
      k_bvns=aux[[2]]
      evol_bvns=append(evol_bvns,ff(x=x_bvns,matriz = matrr_bvns))
      
    }
    t_bvns=t_bvns+1
    
  }
  return(list(x_bvns,evol_bvns))
}

ss=rvns(x_bvns =1:(nrow(tempo)),kmax_bvns = 200,tmax_bvns=500,cc_bvns=1000,matrr_bvns =tempo,vizz_bvns = 4)

# GVNS ####

vnd = function(xx_vnd, kkk_vnd, mtr_vnd, cardmax_vnd,vizz_vnd){
  kk_vnd=2
  while(kk_vnd<kkk_vnd){
    xxl_vnd = first_improv(a=xx_vnd,kk=kk_vnd,matrrr = mtr_vnd,card_max = cardmax_vnd,vizz_fi = vizz_vnd)
    aux = neighb_chang(x=xx_vnd,x_lim = xxl_vnd,k=kk_vnd,matt = mtr_vnd)
    xx_vnd = aux[[1]]
    kk_vnd=aux[[2]]
    
  }
  return(xx_vnd)
}

testa = c(1,sample(2:250,size=249,replace=F))
testa
#tt=vnd(xx=testa,kkk_vnd =150,mtr_vnd = tempo,cardmax_vnd=1500)

gvns=function(xinit_gvns,kmax_gvns,kmax_vnd,tmax_gvns,mtr_gvns,cardmaxx_gvns,vizz_gvns){
  
  evol_gvns = ff(x=xinit_gvns,matriz=mtr_gvns)
  t_gvns = 0
  
  while(t_gvns<tmax_gvns){
    k_gvns = 3
    while(k_gvns<kmax_gvns){
      xl_gvns = shake(x = xinit_gvns,k=k_gvns, tipo=vizz_gvns)
      xll_gvns = vnd(xx=xl_gvns,kkk=kmax_vnd,mtr=mtr_gvns,cardmax_vnd = cardmaxx_gvns,vizz_vnd=vizz_gvns)
      aux = neighb_chang(x=xinit_gvns,x_lim = xll_gvns,k=k_gvns,matt = mtr_gvns)
      xinit_gvns = aux[[1]]
      k_gvns=aux[[2]]
      evol_gvns=append(evol_gvns,ff(x=xinit_gvns,matriz = mtr_gvns))
    }
    t_gvns=t_gvns+1
  }
  
  return(list(xinit_gvns,evol_gvns))
}


#ttt = gvns(xinit_gvns = testa,kmax_gvns = 15,kmax_vnd = 10,tmax_gvns = 11,mtr_gvns = tempo,cardmaxx_gvns = 1000, vizz_gvns  = 3)



tempo <- as.matrix(read.csv("E:\\Google Drive\\UFMG\\2021.1\\Teoria da Decisao\\TC\\tempo.csv", header = F))
distancia<-as.matrix(read.csv("E:\\Google Drive\\UFMG\\2021.1\\Teoria da Decisao\\TC\\distancia.csv", header = F))
funcs =list(tempo,distancia)

ttt = gvns(xinit_gvns = testa,kmax_gvns = 5,kmax_vnd = 5,tmax_gvns = 5,mtr_gvns = tempo,cardmaxx_gvns = 1000, vizz_gvns  = 1)
ttt

evol_gvns = list()
otimss_gvns = list()

for(k in 1:2){
  mag_res = list()
  gyros = list()
  for(i in 1:4){
    l1 = list()
    v1 = list()
    for(j in 1:5){
      initium = c(1,sample(2:250,size = 249, replace=F))
      exitus = gvns(xinit_gvns =initium,kmax_gvns = 5,kmax_vnd = 5,tmax_gvns=5,cardmaxx_gvns=1000,mtr_gvns =funcs[[k]],vizz_gvns = i)
      v1[j] = exitus[1]
      l1[j] = exitus[2]
    }
    mag_res[i]=list(v1)
    gyros[i]=list(l1)
  }
  evol_gvns[k]= list(gyros)
  otimss_gvns[k]=list(mag_res)
}