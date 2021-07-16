module ZeroMQInterface
    use, intrinsic :: iso_c_binding, only: C_CHAR, C_DOUBLE, C_NULL_CHAR
    implicit none

CONTAINS
    SUBROUTINE UpdateZeroMQ()
        character(256) :: zmq_address
        real(C_DOUBLE), dimension(0:2) :: measurements
        real(C_DOUBLE), dimension(0:1) :: setpoints

        ! C interface with ZeroMQ client
        interface
            subroutine zmq_client(zmq_address, measurements, setpoints) bind(C, name="zmq_client")
                import :: C_CHAR, C_DOUBLE
                implicit none
                character(C_CHAR), intent(out) :: zmq_address(*)
                real(C_DOUBLE) :: measurements(3), setpoints(2)
            end subroutine zmq_client
        end interface

        ! Define a ZeroMQ IP address and port to communicate over
        zmq_address = C_CHAR_"tcp://localhost:5555"//C_NULL_CHAR
      
        ! Call and communicate with zeromq server 1000 times
        write (*,*) "Doing large number of func. calls to zmq to test speed... "

        measurements = (/0.5, 265.1001, 3.05/)  ! Placeholder
        call zmq_client(zmq_address,measurements,setpoints)

        write (*,*) "Finished."
        write (*,*) "main.f90: yaw_setpoint from ssc: ", setpoints(0)
        write (*,*) "main.f90: pitch_setpoint from ssc: ", setpoints(1)

    END SUBROUTINE UpdateZeroMQ
end module ZeroMQInterface