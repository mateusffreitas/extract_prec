module mod_polygon_point_check
    use, intrinsic :: iso_fortran_env, only: dp=>real64
    implicit none
    
    type :: point
        real(dp) :: lat, lon
    end type point
    real, allocatable :: points_inside(:,:)
    type(point) :: test_point
    
    private
    public :: point, point_in_polygon, read_polygon_from_file, points_inside, check_inside
    
contains
    ! Função principal que verifica se um ponto está dentro do polígono
    function point_in_polygon(polygon, test_point) result(is_inside)
        type(point), intent(in) :: polygon(:)
        type(point), intent(in) :: test_point
        logical :: is_inside
        
        integer :: i, j, n
        real(dp) :: xi, yi, xj, yj, intersect
        real(dp) :: x, y
        
        n = size(polygon)
        is_inside = .false.
        x = test_point%lon
        y = test_point%lat
        
        ! Algoritmo Ray Casting (lançamento de raios)
        do i = 1, n
            j = merge(1, i+1, i == n)  ! Conecta o último ponto ao primeiro
            
            xi = polygon(i)%lon
            yi = polygon(i)%lat
            xj = polygon(j)%lon
            yj = polygon(j)%lat
            
            ! Verifica se o ponto está entre os y-coordinates do segmento
            if (((yi > y) .neqv. (yj > y))) then
                ! Calcula a interseção
                intersect = (xj - xi) * (y - yi) / (yj - yi) + xi
                
                ! Verifica se o ponto está à esquerda da interseção
                if (x < intersect) then
                    is_inside = .not. is_inside
                end if
            end if
        end do
    end function point_in_polygon
    
    ! Subrotina para ler polígono de arquivo texto
    subroutine read_polygon_from_file(filename, polygon, ierr)
        character(len=*), intent(in) :: filename
        type(point), allocatable, intent(out) :: polygon(:)
        integer, intent(out) :: ierr
        
        integer :: io_unit, n_lines, i
        real(dp) :: lon, lat
        
        ierr = 0
        open(newunit=io_unit, file=filename, status='old', action='read', iostat=ierr)
        if (ierr /= 0) return
        !print *, ierr   
        ! Conta o número de linhas no arquivo
        n_lines = 0
        do
            read(io_unit, *, iostat=ierr) lon, lat
            !print *,lon,lat
            if (ierr /= 0) exit
            n_lines = n_lines + 1
        end do
        ierr = 0
        if (n_lines < 3) then
            ierr = -1  ! Polígono precisa de pelo menos 3 pontos
            close(io_unit)
            return
        end if
        
        ! Aloca e lê os pontos
        allocate(polygon(n_lines))
        rewind(io_unit)
        
        do i = 1, n_lines
            read(io_unit, *) polygon(i)%lon, polygon(i)%lat
        end do
        
        close(io_unit)
    end subroutine read_polygon_from_file

    subroutine check_inside(lon_data,lat_data,polygon,xsiz,jsiz)

        real, intent(in) :: lon_data(:),lat_data(:)
        integer, intent(in) :: xsiz,jsiz
        integer :: icount, i,j
        logical :: is_inside
        type(point) :: polygon(:)
    
        allocate(points_inside(xsiz,jsiz))

        icount = 0
        points_inside = 0.0
        do i=1 , size(lon_data)
            test_point%lon = lon_data(i)
            do j=1 , size(lat_data)
                test_point%lat = lat_data(j)
                is_inside = point_in_polygon(polygon, test_point)
                if (is_inside) then
                    icount = icount+1
                    points_inside(i,j) = 100.0
                end if
            end do
        end do

    end subroutine check_inside

    
end module mod_polygon_point_check