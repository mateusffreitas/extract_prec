module mod_netcdf
    use netcdf

    implicit none

    real, allocatable :: lat_data(:), lon_data(:)
    real, allocatable :: totprec_data(:,:,:), totprec_mask_data(:,:,:)
    private

    public :: process_netCDF_in, lat_data, lon_data, totprec_data, totprec_mask_data, process_netCDF_out
    
    integer, allocatable :: dim_sizes(:)
    integer :: ncid_in, ncid_out
    integer :: varid_in, varid_out, latid, lonid
    integer :: dimids_in(NF90_MAX_VAR_DIMS)
    integer :: ndims_in, dimids_out(NF90_MAX_VAR_DIMS)
    character(len=NF90_MAX_NAME) :: dim_name
    integer :: dim_size, dimid
    integer :: i, status

    ! Informações das variáveis
    integer :: xtype_in, natts_in
    character(len=NF90_MAX_NAME) :: var_name_in

contains

    subroutine process_netCDF_in(input_file,var_name)

        character(len=*), intent(in) :: input_file
        character(len=*), intent(in) :: var_name

        ! Abrir o arquivo NetCDF de entrada
        status = nf90_open(trim(input_file), NF90_NOWRITE, ncid_in)
        if (status /= nf90_noerr) call handle_err(status)

        ! Ler valores de latitude
        status = nf90_inq_varid(ncid_in, "lat", latid)
        if (status /= nf90_noerr) then
            status = nf90_inq_varid(ncid_in, "latitude", latid)  ! Tenta nome alternativo
            if (status /= nf90_noerr) call handle_err(status)
        end if

        ! Obter tamanho da dimensão lat
        status = nf90_inquire_variable(ncid_in, latid, dimids=dimids_in)
        status = nf90_inquire_dimension(ncid_in, dimids_in(1), len=dim_size)

        allocate(lat_data(dim_size))
        status = nf90_get_var(ncid_in, latid, lat_data)
        if (status /= nf90_noerr) call handle_err(status)

        ! Ler valores de longitude
        status = nf90_inq_varid(ncid_in, "lon", lonid)
        if (status /= nf90_noerr) then
            status = nf90_inq_varid(ncid_in, "longitude", lonid)  ! Tenta nome alternativo
            if (status /= nf90_noerr) call handle_err(status)
        end if

        ! Obter tamanho da dimensão lon
        status = nf90_inquire_variable(ncid_in, lonid, dimids=dimids_in)
        status = nf90_inquire_dimension(ncid_in, dimids_in(1), len=dim_size)

        allocate(lon_data(dim_size))
        status = nf90_get_var(ncid_in, lonid, lon_data)
        if (status /= nf90_noerr) call handle_err(status)

        ! Extrair variável totprec (como no programa original)
        status = nf90_inq_varid(ncid_in, trim(var_name), varid_in)
        if (status /= nf90_noerr) call handle_err(status)

        status = nf90_inquire_variable(ncid_in, varid_in, var_name_in, xtype_in, ndims_in, dimids_in, natts_in)
        if (status /= nf90_noerr) call handle_err(status)

        allocate(dim_sizes(ndims_in))
        do i = 1, ndims_in
            status = nf90_inquire_dimension(ncid_in, dimids_in(i), dim_name, dim_size)
            if (status /= nf90_noerr) call handle_err(status)
            dim_sizes(i) = dim_size
        end do

        if (ndims_in == 3) then
            allocate(totprec_data(dim_sizes(1), dim_sizes(2), dim_sizes(3)))
            allocate(totprec_mask_data(dim_sizes(1), dim_sizes(2), dim_sizes(3)))
        else
            write(*,*) 'Dimensão inesperada para a variável prec:', ndims_in
            stop
        end if

        status = nf90_get_var(ncid_in, varid_in, totprec_data)
        if (status /= nf90_noerr) call handle_err(status)

    end subroutine process_netCDF_in


    subroutine process_netCDF_out(output_file,var_name, write_all)

        character(len=*), intent(in) :: output_file
        character(len=*), intent(in) :: var_name
        character(len=*), intent(in) :: write_all

        integer :: i, status

        ! Criar novo arquivo NetCDF para saída
        status = nf90_create(trim(output_file), NF90_NETCDF4, ncid_out)
        if (status /= nf90_noerr) call handle_err(status)

        ! 7. Copiar dimensões e coordenadas
        do i = 1, ndims_in
            status = nf90_inquire_dimension(ncid_in, dimids_in(i), dim_name, dim_size)
            if (status /= nf90_noerr) call handle_err(status)

            status = nf90_def_dim(ncid_out, dim_name, dim_size, dimid)
            if (status /= nf90_noerr) call handle_err(status)

            dimids_out(i) = dimid

            ! Copiar variáveis de coordenadas se for lat ou lon
            if (trim(dim_name) == 'lat' .or. trim(dim_name) == 'latitude') then
                status = nf90_def_var(ncid_out, dim_name, NF90_FLOAT, dimid, latid)
                status = nf90_copy_att(ncid_in, latid, "units", ncid_out, latid)
                status = nf90_copy_att(ncid_in, latid, "long_name", ncid_out, latid)
            elseif (trim(dim_name) == 'lon' .or. trim(dim_name) == 'longitude') then
                status = nf90_def_var(ncid_out, dim_name, NF90_FLOAT, dimid, lonid)
                status = nf90_copy_att(ncid_in, lonid, "units", ncid_out, lonid)
                status = nf90_copy_att(ncid_in, lonid, "long_name", ncid_out, lonid)
            end if
        end do

        ! 8. Definir variável totprec no novo arquivo
        status = nf90_def_var(ncid_out, trim(var_name), xtype_in, dimids_out(1:ndims_in), varid_out)
        if (status /= nf90_noerr) call handle_err(status)

        ! 9. Copiar atributos da variável
        do i = 1, natts_in
            status = nf90_inq_attname(ncid_in, varid_in, i, dim_name)
            if (status /= nf90_noerr) call handle_err(status)

            status = nf90_copy_att(ncid_in, varid_in, dim_name, ncid_out, varid_out)
            if (status /= nf90_noerr) call handle_err(status)
        end do

        ! 10. Finalizar definições do arquivo
        status = nf90_enddef(ncid_out)
        if (status /= nf90_noerr) call handle_err(status)

        ! 11. Escrever dados no novo arquivo
        ! Escrever coordenadas primeiro
        status = nf90_put_var(ncid_out, latid, lat_data)
        status = nf90_put_var(ncid_out, lonid, lon_data)

        !Totalizando a chuva em cada tempo e no total

        ! Escrever dados totprec
        if (trim(write_all)=="1") then
            status = nf90_put_var(ncid_out, varid_out, totprec_data)
        else
            status = nf90_put_var(ncid_out, varid_out, totprec_mask_data)
        end if
        if (status /= nf90_noerr) call handle_err(status)

        ! 12. Fechar arquivos
        status = nf90_close(ncid_in)
        status = nf90_close(ncid_out)

        write(*,*) 'Arquivo NetCDF criado: ', output_file

    end subroutine process_netCDF_out

    subroutine handle_err(status)
        integer, intent(in) :: status
        
        if (status /= nf90_noerr) then
            write(*,*) 'Erro NetCDF: ', trim(nf90_strerror(status))
            stop
        end if
    end subroutine handle_err

end module mod_netcdf