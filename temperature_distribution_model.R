Lnew <- matrix(nrow=10,ncol=10,0)
L <- matrix(nrow=10,ncol=10,0)


L[,1] <- rep(0,10)
L[,10] <- rep(0,10)
L[1,] <- rep(0,10)
L[10,] <- rep(1,10)


Lnew <- L

L

  
L_mean<-mean(L)
s<-0



saveGIF({
  k <- 1
  while(TRUE)
  {

#Dodanie obrazu dla stanu zerowego
    if(k == 1){
      Limg<- apply(L, 2, rev)
      image(t(Limg))
      text(0,1,0)
    }
    
    
    for (i in 2:9){
      for (j in 2:9){
        Lnew[i,j]<-0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])
      }
    }
  
    

    L <- Lnew

    
#Wyswietlenie wyniku co 10 iteracje
    if(k %% 10 == 0){
      Limg<- apply(L, 2, rev)
      image(t(Limg))
      text(0,1,k)
    }
    
    
    Ln_mean<-mean(L)
    s<-s+1
    
#Wyswietlenie macierzy dla kazdej iteracji
    print(paste0("iteracja numer:", k))
    print(L)
    
#Warunek stopu
    if((Ln_mean-L_mean)/L_mean < 0.001){
      break;
    }
    L_mean<-Ln_mean
    
    k <- k+1
  } 
  

})


