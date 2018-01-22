module variables
  type string
    character(len=:), allocatable :: str
  end type string
end module variables
!############################################################################
!
program verif
!
!############################################################################
  use variables
  implicit none
  integer, parameter :: mytype = KIND(0.0D0)
  integer nx, ny, nz,shrinkStep
  integer :: file1,filen,nfiles,nclx,ncly,nclz,istret,ivarName
  integer :: nxShr,nyShr,nzShr,jj,jjj
  integer(4) :: icrfile, ifile, dig1, dig2, dig3, dig4
  real(4) :: xlx,yly,zlz,dt,dx,dy,dz,ydumy
  real(mytype),allocatable,dimension(:, :, :) :: fileData
  real(4), allocatable :: yp(:),y1(:),y3(:)
  integer :: i,j,k,count,nfil,ifileNumb,io
  logical :: exist
  character(3) :: chits !IF THE DATA ARE STORED WITH 3 DIGITS, IE UX001,UX002,ETC.
  character(len=20) :: filename  
  type(string), allocatable:: varName(:)
  
  nx=3071
  ny=321
  nz=256
  xlx=1050
  yly=40
  zlz=40
  nclx=2
  ncly=2
  nclz=0
  file1=3
  filen=7
  nfiles=filen-file1+1
  istret=3.
  shrinkStep=2
 ! nxShr=int(nx/shrinkStep)
  !nyShr=int(ny/shrinkStep)
  !nzShr=int(nz/shrinkStep)
allocate(varName(5))
  varName=(/string('ux'),string('uy'),string('uz'),string('vort'),string('lam') /)
  !varName(1)%str='ux'
  !varName(2)%str='uy'
  !varName(3)%str='uz'
  !varName(4)%str='vort'
  !varName(5)%str='lam'
!! ***************** Allocating the variables ****************************
allocate(fileData(nx,ny,nz))
!----------------------------------------

!! U Perturbation statistics !!

do ifileNumb=file1,filen
do ivarName=1,size(varName)

 10 format(A,I3.3)
 write(filename, 10) varName(ivarName)%str,ifileNumb
print *,
 print *, 'Reading file  <<  ' , filename 

 OPEN(20,FILE=filename,FORM='UNFORMATTED',ACCESS='DIRECT', RECL=8)
 fileData(:,:,:)=0._mytype
  COUNT = 1
  DO K=1,nz
     DO J=1,ny
        DO I=1,nx
           READ(20,REC=COUNT,IOstat=io) fileData(I,J,K)
           if (io .ne. 0) then 
           PRINT *, '======================================================='
           print *, 'Error occurred in reading file' , filename
           PRINT *, 'The error at i, j, k ',i,j,k
           PRINT *, '======================================================='
           goto 100
           endif
           COUNT = COUNT + 1
        ENDDO
     ENDDO
  ENDDO
 close(20)

 30 format(A,A,I3.3)
 write(filename, 30) varName(ivarName)%str,'_shrinked_', ifileNumb
print *,
print *, 'Writing file  >>  ', filename
 open(40, file=filename, FORM='UNFORMATTED', action="write",access='stream')
nzShr=0
  DO K=1,nz,shrinkStep
     nyShr=0
     DO J=1,ny,shrinkStep
        nxShr=0
        DO I=1,nx,shrinkStep
           nxShr=nxShr+1
           !write (40) fileData(I,J,K)
        ENDDO
        nyShr=nyShr+1
     ENDDO
     nzShr=nzShr+1
  ENDDO
  close(40)
100 enddo
enddo 
print *,
print *, 'Shrinked domain size ',nxShr,nyShr,nzShr
!!##########################################################################
!!########################   PARAVIEW   ################################
!!##########################################################################
    

  if (nclx==0) dx=xlx/nxShr
  if (nclx==1 .or. nclx==2) dx=xlx/(nxShr-1.)
  if (ncly==0) dy=yly/nyShr
  if (ncly==1.or.ncly==2) dy   =yly/(nyShr-1.)
  if (nclz==0) dz=zlz/nzShr
  if (nclz==1.or.nclz==2) dz=zlz/(nzShr-1.)
  dt=1.

  allocate(y1(nxShr))
  allocate(yp(nyShr))
  allocate(y3(nzShr))
  do i=1,nxShr
     y1(i)=(i-1)*dx
  enddo

if (istret .ne. 0) then     
print *,'We need to read the yp.dat file'
     open(12,file='yp.dat',form='formatted',status='unknown')
jj=0
     do j=1,ny,shrinkStep
        jj=jj+1
        read(12,*) yp(jj)

        if (j .le. ny-shrinkStep+1) then
            do jjj=1,shrinkStep-1             
                read(12,*) ydumy
            enddo
         endif
     enddo
     close(12)
  else
     do j=1,ny,shrinkStep
        jj=jj+1
        yp(jj)=(jj-1)*dy
     enddo
  endif
  do k=1,nzShr
     y3(k)=(k-1)*dz
  enddo


  nfil=41
  open(nfil,file='visu_Shrink.xdmf')

  write(nfil,'(A22)')'<?xml version="1.0" ?>'
  write(nfil,*)'<!DOCTYPE Xdmf SYSTEM "Xdmf.dtd" []>'
  write(nfil,*)'<Xdmf xmlns:xi="http://www.w3.org/2001/XInclude" Version="2.0">'
  write(nfil,*)'<Domain>'
  write(nfil,*)'    <Topology name="topo" TopologyType="3DRectMesh"'
  write(nfil,*)'        Dimensions="',nzShr,nyShr,nxShr,'">'
  write(nfil,*)'    </Topology>'
  write(nfil,*)'    <Geometry name="geo" Type="VXVYVZ">'
  write(nfil,*)'    <DataItem Dimensions="',nxShr,'" NumberType="Float" Precision="4" Format="XML">'
  write(nfil,*)'    ',y1(:) 
  write(nfil,*)'    </DataItem>'
  write(nfil,*)'    <DataItem Dimensions="',nyShr,'" NumberType="Float" Precision="4" Format="XML">'
  write(nfil,*)'    ',yp(:) 
  write(nfil,*)'    </DataItem>'
  write(nfil,*)'    <DataItem Dimensions="',nzShr,'" NumberType="Float" Precision="4" Format="XML">'
  write(nfil,*)'    ',y3(:) 
  write(nfil,*)'    </DataItem>'
  write(nfil,*)'    </Geometry>'
  write(nfil,'(/)')
  write(nfil,*)'    <Grid Name="TimeSeries" GridType="Collection" CollectionType="Temporal">'
  write(nfil,*)'        <Time TimeType="HyperSlab">'
  write(nfil,*)'            <DataItem Format="XML" NumberType="Float" Dimensions="3">'
  write(nfil,*)'           <!--Start, Stride, Count-->'
  write(nfil,*)'            0.0',dt
  write(nfil,*)'            </DataItem>'
  write(nfil,*)'        </Time>'

  do ifile = file1, filen

!IF THE DATA ARE STORED WITH 4 DIGITS, IE UX0001,UX0002,ETC.
  !   dig1 =   ifile/1000 + 48
  !   dig2 = ( ifile - 1000*( ifile/1000 ) )/100 + 48
  !   dig3 = ( ifile - 100*( ifile/100 ) )/10 + 48
  !   dig4 = ( ifile - 10*( ifile/10 ) )/1 + 48
  !   chits(1:4) = char(dig1)//char(dig2)//char(dig3)//char(dig4)    

!IF THE DATA ARE STORED WITH 3 DIGITS, IE UX001,UX002,ETC.
    dig1 =   ifile/100 + 48
    dig2 = ( ifile - 100*( ifile/100 ) )/10 + 48
    dig3 = ( ifile - 10*( ifile/10 ) )/1 + 48
    chits(1:3) = char(dig1)//char(dig2)//char(dig3)

     write(*,*) ifile, 'file'//chits

     write(nfil,'(/)')
     write(nfil,*)'        <Grid Name="'//chits//'" GridType="Uniform">'
     write(nfil,*)'            <Topology Reference="/Xdmf/Domain/Topology[1]"/>'
     write(nfil,*)'            <Geometry Reference="/Xdmf/Domain/Geometry[1]"/>'
!SINGLE PRECISION-->Precision=4
!DOUBLE PRECISION-->Precision=8
     write(nfil,*)'            <Attribute Name="ux_shrinked_" Center="Node">'
     write(nfil,*)'               <DataItem Format="Binary" '
     write(nfil,*)'                DataType="Float" Precision="8" Endian="little"'
     write(nfil,*)'                Dimensions="',nzShr,nyShr,nxShr,'">'
     write(nfil,*)'                  ux_shrinked_'//chits
     write(nfil,*)'               </DataItem>'
     write(nfil,*)'            </Attribute>'

!it is possible to add as much field as you want for example uy

    write(nfil,*)'            <Attribute Name="uy_shrinked_" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little"'
    write(nfil,*)'                Dimensions="',nzShr,nyShr,nxShr,'">'
    write(nfil,*)'                  uy_shrinked_'//chits
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'            <Attribute Name="uz_shrinked_" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little"'
    write(nfil,*)'                Dimensions="',nzShr,nyShr,nxShr,'">'
    write(nfil,*)'                  uz_shrinked_'//chits
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'            <Attribute Name="lam_shrinked_" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little"'
    write(nfil,*)'                Dimensions="',nzShr,nyShr,nxShr,'">'
    write(nfil,*)'                  lam_shrinked_'//chits
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'

    write(nfil,*)'            <Attribute Name="vort_shrinked_" Center="Node">'
    write(nfil,*)'               <DataItem Format="Binary" '
    write(nfil,*)'                DataType="Float" Precision="8" Endian="little"'
    write(nfil,*)'                Dimensions="',nzShr,nyShr,nxShr,'">'
    write(nfil,*)'                  vort_shrinked_'//chits
    write(nfil,*)'               </DataItem>'
    write(nfil,*)'            </Attribute>'


     write(nfil,*)'        </Grid>'

  enddo
  write(nfil,'(/)')
  write(nfil,*)'    </Grid>'
  write(nfil,*)'</Domain>'
  write(nfil,'(A7)')'</Xdmf>'
  close(nfil)
end program verif






!!!#############################################################

!
