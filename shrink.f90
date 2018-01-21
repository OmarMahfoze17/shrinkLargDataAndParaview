!############################################################################
!
program verif
!
!############################################################################
  implicit none
  
  integer xitime,xitime_temp,itime,ifirst,nx, ny, nz
  real(8),allocatable,dimension(:, :, :) :: fileData,umean_temp

  integer :: i,j,k,count,nfil,ifileNumb
  real(8) ::pi,u_to,Re,Re_t_up,Re_t_low,u_to_temp,Re_t_up_temp,Re_t_low_temp
  real(8),allocatable,dimension(:) :: yp,qstat2,qstat2_temp
  logical :: exist
!IF THE DATA ARE STORED WITH 3 DIGITS, IE UX001,UX002,ETC.
  character(3) :: chits
    
  integer,dimension(8) :: values 
  
  nx=3071
  ny=321
  nz=256
  xlx=300
  yly=40
  zlz=20
  nclx=2
  ncly=2
  nclz=0
  nfiles=2
  file1=1
  filen=2
  istret=3.
  ifileNumb=1

!! ***************** Allocating the variables ****************************
allocate(fileData(nx,ny,nz))
allocate(umean_temp(nx,ny,nz))
allocate(qstat2(ny))
allocate(qstat2_temp(ny))
!----------------------------------------

!! U Perturbation statistics !!



     995 format('ux',I3.3)
      write(filename, 995) ifileNumb


 OPEN(11,FILE=filename,FORM='UNFORMATTED',&
       ACCESS='DIRECT', RECL=8)
  COUNT = 1
  DO K=1,nz
     DO J=1,ny
        DO I=1,nx
           READ(11,REC=COUNT) fileData(I,J,K)
           COUNT = COUNT + 1
        ENDDO
     ENDDO
  ENDDO
 close(11)

  end program verif
!
