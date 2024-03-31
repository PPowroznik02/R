library(animation)
library(plotrix)

#pomiar czasu wykonania zadania
start<-Sys.time()

#wymiar
N=200

#krok
h<-5

#promien okregow
r<-20

#do sprawdzenia stasbilnosci
stan = 0

#inicjalizacja macierzy dla kroku nowego i przeszłego
Lnew<-matrix(nrow=N,ncol=N,0)
L<-matrix(nrow=N,ncol=N,0)


#funkcja do rysowania kol
draw_Circle<-function(a, x_center, y_center, r){
  
  for(x in (x_center-r):(x_center+r))
  {
    
    for(y in (y_center-r):(y_center+r))
    {
      
      if(x>=0 && x<=N && y>=0 && y<=N){
        if(((x - x_center) * (x - x_center) + (y - y_center) * (y - y_center)) <= r * r){
          a[x,y]<-0.006
          
        }
      }
    }
  }
  return(a)
}


#utworzenie modelu alfa 3 ciała  
a<-matrix(nrow=N,ncol=N,0.002)

Center_positions<-c()

for(i in 1:3){
  pos<-round(runif(2)*(N-2*r)+r)
  Center_positions<-c(Center_positions, pos[1], pos[2])
  
  a<-draw_Circle(a, pos[1], pos[2], r)
  
}

image(t(apply(a, 2, rev)))


#max a do stabilności
a_max<-max(a)

#krok czasowy
dt<-h^2/(4*a_max)

#czas symulacji
t<-0

#warunki brzegowe
L[,1]<-rep(0,N)
L[,N]<-rep(0,N)
L[1,]<-rep(0,N)
L[N,]<-rep(50,N)

#chcemy mieć warunki brzegowe też w nowym kroku
Lnew<-L

#wypisanie stanu pola w kroku 0
L

#ile będzie iteracji wstępnie DUŻO ZA MAŁO
niter<-20000

#inicjalizacja paska postępu
prog_bar<-txtProgressBar( min=0,max=niter,style=3)

#blok do zapisu w animacji GIF o domyślnych: interwale 1s i nazwie animation.gif



saveGIF({
  #pasek postępu nie umie w iterator pętli for :(
  stepi<-(-1)
  
  #pętla po iteracjach (k)
  for (k in 1:niter)
  {
    t<-t+dt
    
    #pasek postępu i jego osobista zmienna(zmieniana wewnątrz pętli for)
    stepi<-stepi+1
    setTxtProgressBar( prog_bar, stepi)
    
    #pętla po wierszach (i) i kolumnach (j)
    for (i in 2:(N-1))
      for (j in 2:(N-1)){
        Lnew[i,j]<-(1-(4*dt*a[i,j])/(h^2))*L[i,j] + dt*a[i,j]*
          ((L[i-1,j]+L[i+1,j]+L[i,j-1]+L[i,j+1])/(h^2))
      }
    
    #naiwny gradient 0 ale działa :)
    Lnew[,1]<-L[,2]
    Lnew[,N]<-L[,N-1]
    
    #przejście o krok do przodu w iteracji
    #auxL to zachowana macierz do przetestowania czy pole jest stabilne (Wam się przyda)
    auxL<-L
    L<-Lnew
    
    
    #sprawdzenie stabilnosci pola
    if(stan != 1){
      if(abs(1-(mean(Lnew)/mean(auxL))) < 0.0001){
        print(paste0("Osiagnieto stabilnosc dla iteracji numer: ", k ))
        print(paste0("Odpowiada to dniu: ", round((t/(3600*24)), 0) ))
        stan = 1
      }
    }
    
    
    
    if (k%%100==0)
    {
      #image po rotacji. apply zadaje funkcję (tu rev) do kolejnych
      #kolumn (2) lub wierszy (1) dla zadanej macierzy (L)
      #rev odwraca kolejność
      Limg <- apply(L, 2, rev)
      image(t(Limg))
      
      #dodanie w lewym górnym rogu czasu w dniach!
      text(0.2,0.9,round(t/(3600*24)))
      
      #dodanie okregow
      for (i in seq(1, 7, 2)) {
        draw.ellipse(1-Center_positions[i]/200, (Center_positions[i+1]/200), a = 1/(N/r), b = 1/(N/r), nv = 100, lwd = 1, border = "black")
      }
      box()
      
      
    } #if
  } #po k
},interval=0.1) #SaveGIF

#koniec pomiaru czasu
stop<-Sys.time()

#wypisanie czasu
stop-start

#wypisanie wyniku
L

#image standardowo


image (L)