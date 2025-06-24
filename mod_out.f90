module mod_out
    implicit none
    private 

    public :: write_csv, write_script

    contains

    subroutine write_csv(csv_file,totprec_data,totprec_mask_data,points_inside,mapa)

        character(len=*), intent(in) :: csv_file
        real, intent(in) :: totprec_data(:,:,:)
        real, intent(in) :: points_inside(:,:)
        character(len=*), intent(in) :: mapa
        !
        real, intent(out) :: totprec_mask_data(:,:,:)

        real :: sum_tot,sum_prec
        integer :: t,i,j
        
        sum_tot = 0.0
        open(unit=22,file=trim(csv_file),status="replace",action="write")
        write(22,fmt='(A4,",",A15,",",A30)') "Time","Tot_area","Tot_Accum_Area"
        do t = 1, size(totprec_data,3)
            sum_prec = 0.0
            do i = 1, size(totprec_data,1)
                do j = 1, size(totprec_data,2)
                    if (points_inside(i,j) .ne. 0.0) then 
                        sum_prec = sum_prec + totprec_data(i,j,t)
                        !print *,"inside: ",points_inside(i,j),i,j,lon_data(i),lat_data(j)
                        if (trim(mapa)=="1") then
                            totprec_mask_data(i,j,t) = points_inside(i,j)
                        else
                            totprec_mask_data(i,j,t) = totprec_data(i,j,t)
                        end if
                    else
                        if (trim(mapa)=="1") then
                            totprec_mask_data(i,j,t) = 0.0
                        else
                            totprec_mask_data(i,j,t) = 0.0
                        end if                    
                    end if
                end do
            end do
            sum_tot = sum_tot + sum_prec
            write(22,fmt='(I4.4,",",F15.4,",",F30.4)') t,sum_prec,sum_tot  
        end do
        close(unit=22)
        write(*,*) 'Arquivo CSV criado: ', csv_file

    end subroutine write_csv

    subroutine write_script(csv_file, png_file)

        character(len=*), intent(in) :: csv_file
        character(len=*), intent(in) :: png_file
        character(len=256) :: filename
        character(len=256) :: mensagem
        integer :: status

        filename = trim(csv_file)//".gnu"

        open(unit=22,file=trim(filename),status="replace",action="write")
        write(22,*) "set terminal pngcairo enhanced font 'Arial,12' size 800,600"
        write(22,*) "set output '"//trim(png_file)//"'"
        write(22,*) "set y2tics"
        write(22,*) "set ytics nomirror"
        write(22,*) "set xtics 24"
        write(22,*) "set ylabel 'mm'"
        write(22,*) "set xlabel 'Horas desde "//
        write(22,*) "set y2label 'mm Acum'"
        write(22,*) "set xrange[1:360]"
        write(22,*) "set title 'Precipitacao - Area de Contribuicao - Estado do RS'"
        write(22,*) "set grid"
        write(22,*) "plot '"//trim(csv_file)//"' using 1:2 axis x1y1 with boxes title 'Prec mm/h' , '"//trim(csv_file)//"' using 1:3 axis x1y2 with lines title 'Prec. acum. mm'"

        close(unit=22)
        write(*,*) 'Script gnuplot criado: ', trim(filename)

        call execute_command_line('gnuplot "'//trim(csv_file)//'.gnu"', exitstat=status, cmdmsg=mensagem)
        if (status == 0) then
            write(*,*) 'Arquivo de imagem criado: ', trim(png_file)
        else
            write(*,*) "***** Erro ao executar o gnuplot. Código: ", status
            write(*,*) "***** Verifique o erro ou se ele está presente!"
        end if

    end subroutine write_script


end module mod_out