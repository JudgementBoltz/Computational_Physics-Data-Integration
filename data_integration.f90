program data_integration
!this program reads in data and integrates using first a trapezoid rule
!then a simpson method and then compares the 2 results

implicit none
integer, parameter:: ndata=21
real*8 :: stuff, trapezoid, trapezoid_sum, parabola, parabola_sum
real*8, dimension(ndata):: pointsx, pointsy
integer:: i, j


open(10, file = 'results.dat', status = 'old')

do i = 1, ndata
	read(10,*) pointsx(i), pointsy(i)
	!write(6,*) pointsx(i), pointsy(i)
end do

trapezoid_sum = 0.0d0
trapezoid_loop: do i = 1, ndata - 1
	trapezoid = (pointsx(i+1)-pointsx(i))*(pointsy(i + 1) + pointsy(i))*(.5)
	trapezoid_sum = trapezoid_sum + trapezoid
end do trapezoid_loop

parabola_sum = 0.0d0
simpson_method: do i = 1, ((ndata - 1)/2)
	parabola = (pointsx(2*(i) + 1) - pointsx(2*(i-1) + 1))*(pointsy(2*(i-1) + 1) + 4*pointsy(2*i) + pointsy(2*(i) + 1))
	parabola = parabola/6.0d0
	parabola_sum = 	parabola_sum + parabola
	write(6,*) i
end do simpson_method

write(6,*) 'Simpson Method: ', parabola_sum
write(6,*) 'Trapezoid Method: ', trapezoid_sum
close(10)
end program data_integration

