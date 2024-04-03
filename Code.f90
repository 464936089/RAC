program rimozioneSpike

!########################
!
!
!########################

implicit none
REAL(kind=4) ,ALLOCATABLE, DIMENSION(:) :: xR,yR,zR
REAL(kind=4) ,ALLOCATABLE, DIMENSION(:) :: RxR,RyR,RzR
REAL(kind=4) ,ALLOCATABLE, DIMENSION(:) :: GxR,GyR,GzR

integer(kind=4) :: nlines,nt,i,j,mask(3,3),ds,ii,dum
integer(kind=4) :: ncol,nrow,p,s,irec,steps,istp,ma,soglia
integer(kind=8) :: righe

real(kind=8) :: minX,minY
real(kind=4) :: NminX,NminY,minZ,NmaxX,NmaxY,maxZ
real(kind=4) :: t,array(3,3),th
real(kind=4) ,ALLOCATABLE, DIMENSION(:,:) ::  cell,cell3,cell2

character(len=20) file_bin,file_par,sds,ssoglia
character(len=50) Gout_bin,Rout_bin,file_stat

! ####################################  input
! Legge dallo std input il nome del file BIN su cui operare

  write(*,*) ' enter the input file name bin'
   read(*,'(a)') file_bin
 write(*,*) file_bin
   
  write(*,*) ' enter the input file name Parameters'
!  read(*,'(a)') file_par
  write(*,*) ' Remember to provide par.dat file with dimension & Threshold values'
!  read(*,*) ds, soglia

!file_bin='test.bin'
!file_bin='Tr17_026.bin'

file_par='par.dat'
file_stat='Stat_'//trim(file_bin)//".txt"


!Mauro
!###########################################
! Apertura del file binario e lettura dati
INQUIRE(FILE=file_bin, SIZE=righe)  ! return -1 if cannot determine file size
nt=righe/12

if((nt-5).gt.2E+6) then
write(*,*) "ATTENZIONE NUMERO DI DATI TROPPO ELEVATO"
write(*,*) "Numero di ELEMENTI:   ",  nt-5
!STOP
endif

open(10,file=file_bin,status='old',access='direct',recl=12)

!LEGGE HEADER file binario
  read(10,rec=1) minX,dum	
  read(10,rec=2) minY,dum	

!Lettura numero elementi
  read(10,rec=3) nlines,dum,dum	
!Lettura minimi
  read(10,rec=4) NminX,NminY,minZ	
!Lettura massimi
  read(10,rec=5) NmaxX,NmaxY,maxZ	

  ALLOCATE(xR(nt-5),yR(nt-5),zR(nt-5))

	
	PRINT*,'totale nrow*ncol', nrow*ncol
	PRINT*,'totale righe', nt-5

!##################################################################	
	
	
 open(8,file=file_stat,status='unknown')
 open(9,file=file_par,status='unknown')
  read(9,'(i1)') steps
  !Mauro Scrive nel file di statistiche il numero di step di pulizia (coppie dimensione/soglia)
  write(8,'(i1)') steps
  
  do istp=2,steps+1
  read(9,*) ds,soglia
  
!Mauro
! write(*,*) 'Conteggio righe e colonne...'
 ncol=floor((NmaxX-NminX)/ds)+1
 nrow=floor((NmaxY-NminY)/ds)+1
!Mauro
    write(*,*) 'ncol =', ncol
    write(*,*) 'nrow =', nrow
 ALLOCATE(cell(nrow,ncol),cell2(nrow,ncol),cell3(nrow,ncol))

!Mauro
! write(*,*) file_bin, file_par, ds, soglia,istp, steps
	cell=0.0
	cell2=0.0
	cell3=0.0
	
do irec=1,nt-5 !lettura 

!if(istp.eq.2)then
! read(10,rec=irec+5) xR(irec),yR(irec),zR(irec)      ! sono diventati degli Array
!Mauro  write(100,*) istp,xR(irec),irec,file_bin
!endif

if(istp.eq.2)  read(10,rec=irec+5) xR(irec),yR(irec),zR(irec)      ! sono diventati degli Array

!Mauro  write(100,*) istp,xR(irec),irec,file_bin
	p=floor((xR(irec)-NminX)/ds)+1
	s=floor((yR(irec)-NminY)/ds)+1
	cell2(s,p)=cell2(s,p)+1
	cell(s,p)=(zR(irec)+(cell(s,p)*(cell2(s,p)-1)))/cell2(s,p)
enddo 


DEALLOCATE (cell2)
t=0.0
array=0.0
mask=-1

write(*,*) 'Calcolo dei coefficienti'
 do i=2,nrow-1
  do j=2, ncol-1
		
   array(1:3,1:3)=(cell(i-1:i+1,j-1:j+1))
	where (array(1:3,1:3).eq.0)
	mask=0
	elsewhere
	mask=1
	end where
	ma=sum(mask)
	if (ma.eq.0) then
	ma=1
	endif
	t=sum(array)
	cell3(i,j)=t/ma

  enddo
 enddo
	
	
ALLOCATE(RxR(nt-5),RyR(nt-5),RzR(nt-5))
ALLOCATE(GxR(nt-5),GyR(nt-5),GzR(nt-5))
RxR=0
RyR=0
RzR=0
GxR=0
GyR=0
GzR=0


i=0
j=0
do irec=1,nt-5 ! lecture  
 p=floor((xR(irec)-NminX)/ds)+1
 s=floor((yR(irec)-NminY)/ds)+1
 th=abs(zR(irec)-cell3(s,p))

	if (th.gt.soglia.and.(cell3(s,p).ne.0)) then
	 i=i+1
	 RxR(i)=xR(irec)
	 RyR(i)=yR(irec)
	 RzR(i)=zR(irec)
	else
	 j=j+1
	 GxR(j)=xR(irec)
	 GyR(j)=yR(irec)
	 GzR(j)=zR(irec)	
	endif
if(irec-(i+j).ne.0) write(*,*) "ATTENZIONEEEEEEEEEEEEEEEE",irec,i,j
enddo 	
DEALLOCATE(xR,yR,zR)

!MAuro write(*,*) 'Tutto bene', i,j,irec

!########################################################
!Mauro Definizione nome file senza estensione
ii=len(trim(file_bin))

! Write the integer into a string & Output file name definition:
write(sds, '(i0)') ds
write(ssoglia, '(i0)') soglia
Gout_bin= file_bin(:ii-4)//"_ds_"//trim(sds)//"_soglia_"//trim(ssoglia)//".bin"
Rout_bin= "Reject_"//file_bin(:ii-4)//"_ds_"//trim(sds)//"_soglia_"//trim(ssoglia)//".bin"


!Mauro Verifiche
!!write(*,*) file_bin(:ii-4),ii
! WRITE(*,*) file_bin(:ii-4),"_ds_",trim(sds),"_soglia_",trim(ssoglia),".BIN"
write(*,*)  "VERIFICA:"
write(*,*)  "File good:  ", trim(Gout_bin)
write(*,*)  "File Reject:  ", trim(Rout_bin)

dum=0
!########################################################
! Scrittura del file con i punti ACCETTATI
open(19,file=Gout_bin,access='direct',recl=12)
write(*,*) ' Scrittura header binario Validati' 
!Scrittura dei valori di riferimento X e Y in doppia precisione
  write(19,rec=1) minX,dum	
  write(19,rec=2) minY,dum	
!Scrittura numero elementi, dim-cell, soglia
!Mauro  write(19,rec=3) j,ds,soglia	
  write(19,rec=3) j,dum,dum
!Scrittura valori minimi
  write(19,rec=4) minval(GxR),minval(GyR),minval(GzR)     
!Scrittura massimi
  write(19,rec=5) maxval(GxR),maxval(GyR),maxval(GzR) 
write(*,*) ' Scrittura binario Validati' 
do irec=1,j
  write(19,rec=irec+5) GxR(irec),GyR(irec),GzR(irec)	  
enddo
close(19)
!########################################################

! Scrittura del file con i punti SCARTATI
open(20,file=Rout_bin,access='direct',recl=12)
write(*,*) ' Scrittura header binario Reject' 
!Scrittura dei valori di riferimento X e Y in doppia precisione
  write(20,rec=1) minX,dum	
  write(20,rec=2) minY,dum	
!Scrittura numero elementi, dim-cell, soglia
!  write(20,rec=3) i,ds,soglia	nlines,dum,dum
  write(20,rec=3) nlines,dum,dum
!Scrittura valori minimi
  write(20,rec=4) minval(RxR),minval(RyR),minval(RzR)     
!Scrittura massimi
  write(20,rec=5) maxval(RxR),maxval(RyR),maxval(RzR) 
write(*,*) ' Scrittura binario Reject' 
do irec=1,i
  write(20,rec=irec+5) RxR(irec),RyR(irec),RzR(irec)	  
enddo
close(20)
!########################################################

	write(*,*)'Records Validati=',j
	write(*,*)'Records Reject=',i
	write(*,*)'finito!!!'
  write(8,*) ds,soglia, nt-5, j, i

DEALLOCATE (cell,cell3)


!Mauro Preparazione dati per secondo ciclo

NminX=minval(GxR)
NminY=minval(GyR)
minZ=minval(GzR)	
NmaxX=maxval(GxR)
NmaxY=maxval(GyR)
maxZ=maxval(GzR)	
nt=j+5
ALLOCATE(xR(nt-5),yR(nt-5),zR(nt-5))
xR=GxR
yR=GyR
zR=GzR


DEALLOCATE(RxR,RyR,RzR)
DEALLOCATE(GxR,GyR,GzR)

!Mauro Chiusura ciclo DO su lettura ds & soglia
  enddo 
  close(8)
  close(10)

	stop
	end program rimozioneSpike