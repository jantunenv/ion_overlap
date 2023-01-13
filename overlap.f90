program overlap
	use mt19937_64
	integer(8) :: seed = 5167171
	integer :: total_ions, i, j, k, i2, olap_timesteps, olaps_total, n_rounds
	real(8), allocatable :: grid(:,:)
	real(8) :: t, flux, dx, dt, r1, r2, olap_radius, olap_dt, r
	character(len=80) :: argu



	t = 10000.0 !Ps
	fluence = 7.9e-4 !ions/nm

	if(command_argument_count() == 4) then
		call get_command_argument(1, argu)
		read(argu,*), olap_radius
		call get_command_argument(2, argu)
		read(argu,*),  olap_dt
		call get_command_argument(3, argu)
		read(argu,*),  dx
		call get_command_argument(4, argu)
		read(argu,*),  n_rounds
	else
		olap_radius = 20.0 !nm
		olap_dt = 100.0 !ps
		dx = 10000.0 !nm
		n_rounds = 100

	end if

	total_ions = dx*dx*fluence

	allocate(grid(total_ions,2))

	dt = t/total_ions
	olap_timesteps = olap_dt / dt

	call init_genrand64(seed)

	print *, total_ions, olap_radius, olap_dt

	olaps_total = 0

	do i2 = 1, n_rounds
		do k=1, total_ions
			r1 = genrand64_real2()
			r2 = genrand64_real2()
			grid(k,1) = dx*r1
			grid(k,2) = dx*r2
		end do

		if(olap_timesteps >= total_ions) then
			olap_timesteps = total_ions - 1
		end if

		do i=1, total_ions - 1
			do j=i+1, min(i + olap_timesteps, total_ions)
					r = norm2(grid(i,:) - grid(j,:))
					if(r <= olap_radius) then
						!print *, "overlap: r[nm], dt[ps]"
						olaps_total = olaps_total + 1
					end if
			end do
		end do

	end do

	print *, "average overlaps / pulse:", 1.0*olaps_total/n_rounds



end program overlap
