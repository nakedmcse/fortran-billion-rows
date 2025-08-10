program billionrows
    use omp_lib
    implicit none
    integer :: num_args, idx, ios
    logical :: do_parse = .false.
    logical :: do_generate = .false.
    character(len=256), dimension(:), allocatable :: args
    character(len=256) :: target_file

    num_args = command_argument_count()
    allocate(args(num_args))
    do idx = 1, num_args
        call get_command_argument(idx, args(idx))
        select case (args(idx))
            case ('--parse')
                do_parse = .true.
            case ('--generate')
                do_generate = .true.
            case ('--help')
                print *,'Command Format:'
                print *,'billionrows <filename> [--parse] [--generate] [--help]'
            case default
                target_file = trim(args(idx))
        end select
    end do

    if (do_parse) then
        call parse(target_file)
    else if (do_generate) then
        call generate(target_file)
    end if

    contains

    subroutine generate(filename)
        character(len=256) :: filename, line
        character(len=20), dimension(50000) :: weather_stations
        integer :: p, num_stations, i
        integer :: output_lines = 1000000000
        real :: rnd_station, rnd_temp

        ! Read weather stations
        print *, 'Reading Weather Station Data'
        num_stations = 0;
        open(unit=10, file='weather_stations.csv', status='old')
        do
            read(10, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (line(1:1) /= '#') then
                num_stations = num_stations + 1
                p = index(line, ';')
                if (p > 0) then
                    weather_stations(num_stations) = line(1:p-1)
                end if
            end if
        end do
        close(10)

        ! Generate data
        print *, 'Generating Data'
        open(unit=20, file=filename, status='unknown', position='append', iostat=ios)
        !$OMP PARALLEL SHARED(weather_stations) PRIVATE(i, rnd_station, rnd_temp)
        !$OMP DO
        do i = 1, output_lines
            call random_number(rnd_station)
            call random_number(rnd_temp)
            !$OMP CRITICAL
            write(20,'(A, A, F6.2)') trim(weather_stations(int(rnd_station * num_stations) + 1)), ';', (rnd_temp * 200.0) - 100.0
            !$OMP END CRITICAL
        end do
        !$OMP END DO
        !$OMP END PARALLEL
        close(20)
    end subroutine generate

    subroutine parse(filename)
        character(len=256) :: filename
        stop
    end subroutine parse
end program billionrows