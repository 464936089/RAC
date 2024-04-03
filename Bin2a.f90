!###############################################################################|
! Program fobs2a      BIN >>> ASCII
!
!       input file (binary)
!                                                                               |
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
!  X,Y,Z                  Valori in Coord Km relative (kind 4)  
!
!       output file (ASCII)
!       
!
!###############################################################################|
program Bin2a
implicit none

integer(kind=4) :: nlines,irec,nt,dum,dim,soglia
integer(kind=8) :: righe

real(kind=8) :: minX,minY
real(kind=4) :: NminX,NminY,minZ,NmaxX,NmaxY,maxZ
real(kind=4) :: xR,yR,gR
character(len=60) file_xyz,file_bin     ! filename for input and output

write(*,*) ' file conversion from BIN into ASCII '

  write(*,*) ' enter the output file name bin'
  read(*,'(a)') file_bin
  write(*,*) ' enter the input file name xyz'
  read(*,'(a)') file_xyz  

!file_bin='test.bin'
!file_xyz='Tr17026.bin'

  write(*,*) file_xyz, file_bin
 
    
INQUIRE(FILE=file_bin, SIZE=righe)  ! return -1 if cannot determine file size
nt=righe/12

write(*,*) ' Numero di righe  ',  nt
!read(*,*) nt

open(10,file=file_bin,access='direct',recl=12)
open(11,file=file_xyz,status='unknown')

!LEGGE HEADER file binario
  read(10,rec=1) minX,dum	
  read(10,rec=2) minY,dum

!Lettura numero elementi
  read(10,rec=3) nlines,dim,soglia
!Lettura minimi
  read(10,rec=4) NminX,NminY,minZ	
!Lettura massimi
  read(10,rec=5) NmaxX,NmaxY,maxZ	

write(11,*) minX,dum
write(11,*) minY,dum
write(11,*) nlines,dim,soglia
write(11,*) NminX,NminY,minZ
write(11,*) NmaxX,NmaxY,maxZ

do irec=1,nt-5 ! lecture 
  read(10,rec=irec+5) xR,yR,gR
  write(11,'(2F25.2,F25.2)') xR+minX,yR+minY,gR
enddo 

close(10)
close(11)

write(*,*) ' Finito!!!!!  '

stop
end program Bin2a
