!###############################################################################|
! Program a2Bin      ASCII >>> Bin 
!
!       input file (ASCII)
!       Tr17_025.xyz
!           Long,Lat,Z coord chilometriche
!
!       output file (binary)
!       Bin        (Coord) 3*4 = 12 bytes for each record            |
! BINARY FORMAT:
!          HEADER:
!  Xref(kind8);dum        valore minimo e di riferimento di X per passaggio Coord. Km a relative
!  Yref(kind8);dum        valore minimo e di riferimento di Y per passaggio Coord. Km a relative
!  nlines;dim;soglia      numero di punti, dimensione della cella, soglia valori Z (in questa conversione dim=soglia=0.0)
!  X(min),Y(min),Z(min)   Valori in Coord Km relative (kind 4)                               |
!  X(max),Y(max),Z(max)                                    |
!     
!        CORPO FILE: record 5+irec
!  X,Y,Z                  Valori in Coord Km relative (kind 4)                              |
!                                                                               |
!###############################################################################|
program a2Bin
 implicit none

integer(kind=4) :: nlines,irec,flog,dum
real   (kind=8) :: xR,yR
real   (kind=8) :: minX,minY,maxX,maxY
real   (kind=4) :: NminX,NminY,NmaxX,NmaxY,minZ,maxZ
real   (kind=4) :: zR4,xR4,yR4,zR
character(len=60) file_xyz,file_bin,flog_file     ! filename for input and output

dum=0

  ! ####################################  input
  write(*,*) ' enter the input file name xyz'
  read(*,'(a)') file_xyz
  write(*,*) ' enter the output file name bin'
  read(*,'(a)') file_bin
  
  write(*,*) file_xyz, file_bin

write(flog_file,'(3A)')  "flog."// trim(file_xyz) //".conversion.dat"

flog=51
open(flog,file=flog_file)
open(11,file=file_xyz,status='old')
open(10,file=file_bin,access='direct',recl=12)

write(*,*) ' file conversion from file_xyz (kind8) into file_bin (kin4 coord.Relative & header) '

minX=9999999.99
minY=9999999.99
minZ=9999999.99

maxX=-9999999.99
maxY=-9999999.99
maxZ=-9999999.99

nlines = 1 
rewind(11)
do
 read(11,*,END=10) xR,yR,zR
  if(xR.lt.minX)minX=xR    
  if(yR.lt.minY)minY=yR 
  if(zR.lt.minZ)minZ=zR    
     
  if(xR.gt.maxX)maxX=xR    
  if(yR.gt.maxY)maxY=yR 
  if(zR.gt.maxZ)maxZ=zR 	
  nlines = nlines + 1 
enddo
10 nlines=nlines-1

! definizione valori min/max delle coordinate relative
NminX=real(minX-minX)    ! sempre uguale a 0.0
NminY=real(minY-minY)    ! sempre uguale a 0.0
NmaxX=real(maxX-minX)
NmaxY=real(maxY-minY)

write(*,*) ' Statistiche: min/max, x,y,z,points  ' 
write(*,'(4F16.4,2F11.4,I10)') minX,maxX,minY,maxY,minZ,maxZ,nlines
write(flog,'(2A15)') file_xyz,file_bin
write(flog,*) ' Statistiche: min/max, x,y,z,points  ' 
write(flog,'(4F16.4,2F11.4,I10)') minX, maxX, minY, maxY, minZ, maxZ, nlines
write(flog,*) ' Statistiche: min/max, nuovo riferimento  ' 
write(flog,'(4F16.4)') NminX,NmaxX,NminY,NmaxY

write(*,*) ' Statistiche: NUOVI min/max, x,y,z,points  ' 
write(*,'(4F16.4,1x,2F10.4,I10)') NminX,NmaxX,NminY,NmaxY,minZ,maxZ,nlines

write(*,*) ' X e Y di riferimento:  ' 
write(*,'(4F16.4,2F10.4,I10)') minX,minY

write(*,*) ' Scrittura header binario ' 
!Scrittura dei valori di riferimento X e Y in doppia precisione
  write(10,rec=1) minX,dum	
  write(10,rec=2) minY,dum	
  
!Scrittura numero elementi,Dim,Soglia  !In questo caso i valori di Dim e Soglia sono 0.0
  write(10,rec=3) nlines,dum,dum	
!Scrittura minimi
  write(10,rec=4) NminX,NminY,minZ	
!Scrittura massimi
  write(10,rec=5) NmaxX,NmaxY,maxZ	

write(*,*) ' Scrittura binario ' 
rewind(11)
do irec=1,nlines
  read(11,*) xR,yR,zR
xR4=real(xR-minX)
yR4=real(yR-minY)
zR4=real(zR)
  write(10,rec=irec+5) xR4,yR4,zR4	
enddo 

close(10)
close(11)
close(flog)

stop
end program a2Bin
