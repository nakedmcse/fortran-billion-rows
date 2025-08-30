program billionrows
    use omp_lib
    implicit none
    integer :: num_args, idx, ios
    logical :: do_parse = .false.
    logical :: do_generate = .false.
    character(len=256), dimension(:), allocatable :: args
    character(len=256) :: target_file

    type :: output_chunk
        character(len=20), dimension(:), allocatable :: lines
        integer :: linecount
    end type output_chunk

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

    subroutine append_line(chunk, line)
        type(output_chunk) :: chunk
        character(len=40), dimension(:), allocatable :: temp
        character(len=40) :: line

        if(size(chunk%lines) - chunk%linecount == 0) then
            allocate(temp(chunk%linecount * 2))
            temp(1:chunk%linecount) = chunk%lines
            deallocate(chunk%lines)
            allocate(chunk%lines(chunk%linecount * 2))
            chunk%lines = temp
            deallocate(temp)
        end if

        chunk%linecount = chunk%linecount + 1
        chunk%lines(chunk%linecount) = line
    end subroutine append_line

    subroutine generate(filename)
        character(len=256) :: filename, line
        character(len=20), dimension(50000) :: weather_stations
        type(output_chunk) :: current_chunk
        character(len=40) :: current_line
        integer :: p, num_stations, i, k, n, num_processors
        integer :: output_lines = 1000000000
        integer :: chunk_size = 100000
        real, dimension(2,100000) :: rnd_stat_temp

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
        num_processors = omp_get_num_procs()
        call omp_set_num_threads(num_processors)
        !num_processors = 1
        print *, 'Generating Data using ', num_processors, ' processors'
        open(unit=20, file=filename, status='new', action='write', iostat=ios)
        !$OMP PARALLEL DO SHARED(weather_stations) PRIVATE(i, k, n, rnd_stat_temp, current_chunk, current_line)
        do i = 1, output_lines, chunk_size
            ! init chunk
            current_chunk%linecount = 0
            if (allocated(current_chunk%lines)) deallocate(current_chunk%lines)
            allocate(current_chunk%lines(1000))

            ! fill chunk
            call random_number(rnd_stat_temp)
            do k = 1, min(chunk_size, output_lines - i + 1)
                write(current_line,'(A, A, F6.2)') &
                        trim(weather_stations(int(rnd_stat_temp(1,k) * num_stations) + 1)), ';', &
                        ((rnd_stat_temp(2,k) * 200.0) - 100.0)
                call append_line(current_chunk, current_line)
            end do

            !$OMP CRITICAL(io_write)
            write(20,'(A)') (trim(current_chunk%lines(n)), n=1 ,current_chunk%linecount)
            !$OMP END CRITICAL(io_write)
        end do
        !$OMP END PARALLEL DO
        close(20)
    end subroutine generate

    subroutine parse(filename)
        character(len=256) :: filename
        stop
    end subroutine parse
end program billionrows