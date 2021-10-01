module ZeroMQInterface
    USE, INTRINSIC :: ISO_C_BINDING, only: C_CHAR, C_DOUBLE, C_NULL_CHAR
    IMPLICIT NONE
    ! 

CONTAINS
    SUBROUTINE UpdateZeroMQ(LocalVar, zmqVar)
        USE ROSCO_Types, ONLY : LocalVariables, ZMQ_Variables
        IMPLICIT NONE
        TYPE(LocalVariables), INTENT(INOUT) :: LocalVar
        TYPE(ZMQ_Variables), INTENT(INOUT)  :: zmqVar

        character(256) :: zmq_address
        real(C_DOUBLE) :: setpoints(5)
        real(C_DOUBLE) :: turbine_measurements(16)

        ! C interface with ZeroMQ client
        interface
            subroutine zmq_client(zmq_address, measurements, setpoints) bind(C, name="zmq_client")
                import :: C_CHAR, C_DOUBLE
                implicit none
                character(C_CHAR), intent(out) :: zmq_address(*)
                real(C_DOUBLE) :: measurements(16)
                real(C_DOUBLE) :: setpoints(5)
            end subroutine zmq_client
        end interface

		! Increment call counter by 1
		zmqVar%ZMQ_UpdateCounter = zmqVar%ZMQ_UpdateCounter + 1
		
		! Communicate if threshold has been reached
		IF (zmqVar%ZMQ_UpdateCounter .GE. zmqVar%ZMQ_UpdateFreq) THEN
			! Collect measurements to be sent to ZeroMQ server
			turbine_measurements(1) = LocalVar%iStatus
			turbine_measurements(2) = LocalVar%Time
			turbine_measurements(3) = LocalVar%VS_MechGenPwr
			turbine_measurements(4) = LocalVar%VS_GenPwr
			turbine_measurements(5) = LocalVar%GenSpeed
			turbine_measurements(6) = LocalVar%RotSpeed
			turbine_measurements(7) = LocalVar%GenTqMeas
			turbine_measurements(8) = LocalVar%NacHeading
			turbine_measurements(9) = LocalVar%NacVane
			turbine_measurements(10) = LocalVar%HorWindV
			turbine_measurements(11) = LocalVar%rootMOOP(1)
			turbine_measurements(12) = LocalVar%rootMOOP(2)
			turbine_measurements(13) = LocalVar%rootMOOP(3)
			turbine_measurements(14) = LocalVar%FA_Acc
			turbine_measurements(15) = LocalVar%NacIMU_FA_Acc
			turbine_measurements(16) = LocalVar%Azimuth

			write (zmq_address, "(A,A)") TRIM(zmqVar%ZMQ_CommAddress), C_NULL_CHAR
			call zmq_client(zmq_address, turbine_measurements, setpoints)

			! write (*,*) "ZeroMQInterface: torque setpoint from ssc: ", setpoints(1)
			! write (*,*) "ZeroMQInterface: yaw setpoint from ssc: ", setpoints(2)
			! write (*,*) "ZeroMQInterface: pitch 1 setpoint from ssc: ", setpoints(3)
			! write (*,*) "ZeroMQInterface: pitch 2 setpoint from ssc: ", setpoints(4)
			! write (*,*) "ZeroMQInterface: pitch 3 setpoint from ssc: ", setpoints(5)
			zmqVar%Yaw_Offset = setpoints(2)
			
			zmqVar%ZMQ_UpdateCounter = 1
		ENDIF


    END SUBROUTINE UpdateZeroMQ
end module ZeroMQInterface