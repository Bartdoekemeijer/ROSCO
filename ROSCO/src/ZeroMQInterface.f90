module ZeroMQInterface
    USE, INTRINSIC :: ISO_C_BINDING, only: C_CHAR, C_DOUBLE, C_NULL_CHAR
    IMPLICIT NONE
    ! 

CONTAINS
    SUBROUTINE UpdateZeroMQ(LocalVar, zmq_address, setpoints)
        USE ROSCO_Types, ONLY : LocalVariables
        IMPLICIT NONE
        TYPE(LocalVariables), INTENT(IN) :: LocalVar
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

        ! Collect measurements to be sent to ZeroMQ server
        turbine_measurements(0) = LocalVar%iStatus
        turbine_measurements(1) = LocalVar%Time
        turbine_measurements(2) = LocalVar%VS_MechGenPwr
        turbine_measurements(3) = LocalVar%VS_GenPwr
        turbine_measurements(4) = LocalVar%GenSpeed
        turbine_measurements(5) = LocalVar%RotSpeed
        turbine_measurements(6) = LocalVar%GenTqMeas
        turbine_measurements(7) = LocalVar%Y_M
        turbine_measurements(8) = LocalVar%HorWindV
        turbine_measurements(9) = LocalVar%rootMOOP(1)
        turbine_measurements(10) = LocalVar%rootMOOP(2)
        turbine_measurements(11) = LocalVar%rootMOOP(3)
        turbine_measurements(12) = LocalVar%FA_Acc
        turbine_measurements(13) = LocalVar%NacIMU_FA_Acc
        turbine_measurements(14) = LocalVar%Azimuth
        ! ... add nacelle position...?

        ! zmq_address = C_CHAR_"tcp://localhost:5555"//C_NULL_CHAR
        call zmq_client(zmq_address, turbine_measurements, setpoints)

        ! write (*,*) "ZeroMQInterface: torque setpoint from ssc: ", setpoints(0)
        ! write (*,*) "ZeroMQInterface: yaw setpoint from ssc: ", setpoints(1)
        ! write (*,*) "ZeroMQInterface: pitch 1 setpoint from ssc: ", setpoints(2)
        ! write (*,*) "ZeroMQInterface: pitch 2 setpoint from ssc: ", setpoints(3)
        ! write (*,*) "ZeroMQInterface: pitch 3 setpoint from ssc: ", setpoints(4)

    END SUBROUTINE UpdateZeroMQ
end module ZeroMQInterface