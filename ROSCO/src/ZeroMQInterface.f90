module ZeroMQInterface
    USE, INTRINSIC :: ISO_C_BINDING, only: C_CHAR, C_DOUBLE, C_NULL_CHAR
    IMPLICIT NONE
    ! 

CONTAINS
    SUBROUTINE UpdateZeroMQ(turbine_measurements, zmq_address)
        IMPLICIT NONE
        character(256) :: zmq_address
        real(C_DOUBLE), dimension(0:4) :: setpoints
        real(C_DOUBLE), dimension(0:14) :: turbine_measurements

        ! C interface with ZeroMQ client
        interface
            subroutine zmq_client(zmq_address, measurements, setpoints) bind(C, name="zmq_client")
                import :: C_CHAR, C_DOUBLE
                implicit none
                character(C_CHAR), intent(out) :: zmq_address(*)
                real(C_DOUBLE) :: measurements(3), setpoints(3)
            end subroutine zmq_client
        end interface

        ! zmq_address = C_CHAR_"tcp://localhost:5555"//C_NULL_CHAR
        call zmq_client(zmq_address,turbine_measurements,setpoints)

        write (*,*) "ZeroMQInterface: torque setpoint from ssc: ", setpoints(0)
        write (*,*) "ZeroMQInterface: yaw setpoint from ssc: ", setpoints(1)
        write (*,*) "ZeroMQInterface: pitch 1 setpoint from ssc: ", setpoints(2)
        write (*,*) "ZeroMQInterface: pitch 2 setpoint from ssc: ", setpoints(3)
        write (*,*) "ZeroMQInterface: pitch 3 setpoint from ssc: ", setpoints(4)

    END SUBROUTINE UpdateZeroMQ
end module ZeroMQInterface