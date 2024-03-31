library(animation)
licz<-0
#parametry modelu
nz<-500
nx<-500
fpeak<-30.0
dt<-0.002
dt2<-0.02
et<-0.5
ds<-1.0
xs<-nx/2.0
zs<-25
#kroki czasowe
nt<-et/dt+1;
st<-seq(0,et,length.out=nt)
ss<-matrix(nrow=nt,ncol=nx,)

#model
V<-matrix(nrow=nz,ncol=nx,2000)
for (i in 1:nz/2)
{
  for (j in 1:nx)
  {
    V[i,j]<-1000
  }
}


p<-matrix(nrow=nz,ncol=nx,0)
pm<-matrix(nrow=nz,ncol=nx,0)
pp<-matrix(nrow=nz,ncol=nx,0)
#vmax do warunku stabilnosci
vmax<-2000

#dtr realny krok probkowania
dtr <- ds/(2.0*vmax)
w2<-0
while(1)
{
  w2<-w2+1
  w1<-dt/w2
  if (w1<=dtr)
  {
    dtr<-w1
    break
  }
}

#inicjalizacja paska postepu
niter=et/dtr+1
prog_bar<-txtProgressBar(min=0,max=niter,style=3)
kk<-1 # ilosc dtr na jedna dt
kkk<-0 # ilosc dt
k<-1 # ilosc dtr
saveGIF({
while (1)
{
  k<-k+1
  kk<-kk+1
  t<-k*dtr
  #pasek postepu
  setTxtProgressBar(prog_bar, k)
  #glowna petla do modelowania
  for (i in 2:(nz-1))
  {
    for (j in 2:(nx-1))
    {
      pp[i,j] = 2.0*p[i,j]-pm[i,j] + ((dtr*dtr)/(ds*ds))*V[i,j]*V[i,j]*
        ( p[i+1,j]+p[i-1,j]+p[i,j+1]+p[i,j-1]-4.0*p[i,j]) }
  }

  pp[zs,xs]<-pp[zs,xs]+exp(-(((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak))))))*(1.0-2.0*((pi*fpeak*(t-(1.0/fpeak)))*(pi*fpeak*(t-(1.0/fpeak)))))
  #trasparent lewa - warunek brzegowy dla lewej krawedzi modelu
  for (i in 1:nz)
  {
    pp[i,1]= p[i,1] + p[i,2] - pm[i,2] +
      V[i,1]*(dtr/ds)*(p[i,2]-p[i,1]-(pm[i,3]-pm[i,2]))
  }
  #trasparent prawa
  for (i in 1:nz)
  {
    pp[i,nx]= p[i,nx] + p[i,nx-1] - pm[i,nx-1] +
      V[i,nx]*(dtr/ds)*(p[i,nx-1]-p[i,nx]-(pm[i,nx-2]-pm[i,nx-1]))
  }
  #trasparent gora
  for (i in 1:nx)
  {
    pp[1,i]= p[1,i] + p[2,i] - pm[2,i] +
      V[1,i]*(dtr/ds)*(p[2,i]-p[1,i]-(pm[3,i]-pm[2,i]))
  }
  #trasparent dol
  for (i in 1:nx)
  {
    pp[nz,i]= p[nz,i] + p[nz-1,i] - pm[nz-1,i] +
      V[nz,i]*(dtr/ds)*(p[nz-1,i]-p[nz,i]-(pm[nz-2,i]-pm[nz-1,i]))
  }
  #przejecie o krok do przodu z macierzami
  pm<-p
  p<-pp
  #warunek do zapisania probki sejsmogramu

  if ( kk*dtr + dtr/10.0 >= dt )
  {
    if(licz%%10==0){
      pimg <- apply(p, 2, rev)
      image(t(pimg))
    }
    licz<-licz+1
    kk<-0
    kkk<-kkk+1
    #dodanie probek do sejsmogramu
    ss[kkk,]<- pp[1,]
    if (kkk*dt > et) break
  } 
}
},interval=0.2)

#wyrysowanie pola po obliczeniach
image(t(apply(p, 2, rev)))
text(0.2,0.9,kkk*dt)
image(t(apply(ss, 2, rev)))

par(mfrow=c(1,25), mar = c(0,2,1,0))
for (i in seq(1,nx,nx/25)) {
  plot(ss[,i],st, type='l', bty="n", axes=FALSE, ylim = rev(range(st)))
  if(i==1) axis(2L)
  #axis(1L)
  #box(bty="n")
  title(main=i)
}
mtext("A1", side = 1, outer = TRUE, line = 2.2)
mtext("B", side = 2, outer = TRUE, line = 2.2)
