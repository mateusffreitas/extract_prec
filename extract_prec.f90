program extract_prec_with_coords
    !Author: Rodrigues, L.F [LFR]
    !Data: 24Jun2025
    !Função:
    !Este programa extrai dos arquivos FRNaaaammddhh-FRNaaaammddhh.nc os dados de 
    !precipitação em uma área delimitada pelo arquivo de contorno e gera um CSV
    !ALém de um gráfico/figura
    !
    !Para compilar, faça:
    !
    !  gfortran -O3 -o extract_totprec mod_out.f90 mod_netcdf.f90  mod_polygon_point_chek.f90 \
    !  extract_prec.f90 -I/home/lufla/apps/include -L/home/lufla/apps/lib -lnetcdff -lnetcdf |
    !  -ffree-form -ffree-line-length-none
    !
    ! OBS: Coloque o diretório com as bibliotecas NetCDF no LD_LIBRARY_PATH caso não estejam
    !
    ! Para rodar:
    !
    ! ~/home/dir_com_o_programa/extract_prec 2025061500 2025063000 totprec corumba.txt 0 0
    !
    ! Obs:
    !
    ! O arquivo de entrada em NetCDF pode ser gerado à partir de todas as saídas NetCDF do modelo BRAMS
    ! que podem ser concatenadas por um comando CDO (exemplo):
    !
    ! cdo mergetime $(ls -v <dir_com_as_saidas_netCDF/FRN*.nc) FRN2025061500-FRN2025063000.nc
    !
    use netcdf
    use mod_polygon_point_check
    use mod_netcdf
    use mod_out

    implicit none
    
    ! Variáveis NetCDF
    integer :: ncid_in, ncid_out
    integer :: varid_in, varid_out, latid, lonid
    integer :: dimids_in(NF90_MAX_VAR_DIMS)
    integer :: ndims_in, dimids_out(NF90_MAX_VAR_DIMS)
    character(len=NF90_MAX_NAME) :: dim_name
    integer :: dim_size, dimid
    integer :: i, j, t, status
    
    ! Informações das variáveis
    integer :: xtype_in, natts_in
    character(len=NF90_MAX_NAME) :: var_name_in
    
    type(point), allocatable :: polygon(:)
    integer :: ierr, icount
    logical :: is_inside
    real :: sum_prec, sum_tot

    character(len=256) :: arg1

    character(len=256) :: input_file
    character(len=256) :: output_file
    character(len=256) :: var_name
    character(len=256) :: filename
    character(len=256) :: mapa
    character(len=256) :: write_all
    character(len=256) :: csv_file 
    character(len=256) :: png_file 
    character(10) :: dataini,datafin

    integer :: contagem_inicio, contagem_fim, taxa_contagem, contagem_max
    
    ! Obtém a taxa do relógio do sistema
    call system_clock(count_rate=taxa_contagem)
    call system_clock(count_max=contagem_max)
    
    ! Inicia a contagem
    call system_clock(contagem_inicio)

    ! Verifica se há argumentos suficientes
    if (command_argument_count() /= 6) then
        print *, "Uso: ./extract_prec data_inicial data_final variável arquivo_de_contorno cria_mapa plota_tudo"
        print *, "Ex:  ./extract_prec 2025061500 2025063000 totprec corumba.txt 0 0"
        stop
    end if

    ! Lê os argumentos da linha de comando
    do i = 1, 6
        call get_command_argument(i, arg1)  ! Só usamos arg1 temporariamente
        select case(i)
            case(1); read(arg1, *) dataini
            case(2); read(arg1, *) datafin
            case(3); read(arg1, *) var_name
            case(4); read(arg1, *) filename
            case(5); read(arg1, *) mapa
            case(6); read(arg1, *) write_all
        end select
    end do
    !FRN2025061500-FRN2025063000.nc
    input_file = "FRN"//dataini//"-FRN"//datafin//".nc"
    output_file = var_name//"-"//trim(input_file)
    csv_file = trim(filename(1:len(trim(filename))-4)//"-"//dataini//"-"//datafin//".csv")
    png_file = trim(filename(1:len(trim(filename))-4)//"-"//dataini//"-"//datafin//".png")

    ! Lê o polígono do arquivo
    write(*,*) 'Lendo polígono de entrada...'
    call read_polygon_from_file(trim(filename), polygon, ierr)
    if (ierr /= 0) then
        write(*,*) 'Erro ao ler o arquivo do polígono:'
        if (ierr == -1) write(*,*) 'O polígono deve ter pelo menos 3 pontos'
        stop
    end if

    ! Inicialização NetCDF
    write(*,*) 'Iniciando extração da variável totprec e leitura de coordenadas...'
    call process_netCDF_in(input_file,var_name)

    write(*,*) 'Checando pontos dentro do polígono...'
    call check_inside(lon_data,lat_data, polygon, size(totprec_data,1), size(totprec_data,2))

    write(*,*) 'Escrevendo CSV...'
    call write_csv(csv_file,totprec_data,totprec_mask_data,points_inside,mapa)
    
    write(*,*) 'Escrevendo saída NetCDF...'
    call process_netCDF_out(output_file,var_name, write_all)

    write(*,*) 'Escrevendo script gnuplot...'
    call write_script(csv_file, png_file)

    call system_clock(contagem_fim)

    write(*,*) ''
    write(*,*) 'Processamento concluído com sucesso!'
     ! Calcula e exibe o tempo decorrido
    if (contagem_fim < contagem_inicio) then
        write(*,*) "Tempo decorrido: ", (contagem_max - contagem_inicio + contagem_fim) / real(taxa_contagem), " segundos"
    else
        write(*,*) "Tempo decorrido: ", (contagem_fim - contagem_inicio) / real(taxa_contagem), " segundos"
    end if
    
end program extract_prec_with_coords

