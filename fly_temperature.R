library(animation)
N<-100
#zwiekszenie liczby klatek na sekund?
ani.options(interval = 0.1)

#utworzenie macierzy NxN
Lnew <- matrix(nrow=N,ncol=N,0)
L <- matrix(nrow=N,ncol=N,0)

#warunki brzegowe
L[,1] <- rep(0,N)
L[,N] <- rep(0,N)
L[1,] <- rep(0,N)
L[N,] <- rep(0,N)

Lnew <- L

#poczatkowa pozycja punktu
pos<-round(runif(2)*(N-2)-1)
L[pos[1],pos[2]]<-1

saveGIF({
#obraz stanu zerowego
Limg<- apply(L, 2, rev)
image(t(Limg))
text(0.1,0.9,0)

for(k in 1:1000){
  #schemat roznicowy
  for (i in 2:(N-1)){
    for (j in 2:(N-1)){
      Lnew[i,j]<-0.25*(L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])
    }
  }
  L <- Lnew
  
  #nowa pozycja punktu z gwarancj? bycia w obszarze
  pom<-pos+round(runif(2)*2-1)
  while (pom[1]<=0 || pom[1]>=N || pom[2]<=0 || pom[2]>=N )
  {
    pom<-pos+round(runif(2)*2-1)
  }
  pos<-pom
  
  #przypisanie punktu
  L[pos[1],pos[2]]<-1
  
  #Wyswietlenie wyniku co 10 iteracje
  if(k %% 10 == 0){
    Limg<- apply(L, 2, rev)
    image(t(Limg))
    text(0.1,0.9,k)
  }
}
})