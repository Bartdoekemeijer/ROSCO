#
#   Hello World server in Python
#   Binds REP socket to tcp://*:5555
#   Expects b"Hello" from client, replies with b"World"
#


import numpy as np
import time
import zmq


context = zmq.Context()
socket = context.socket(zmq.REP)
socket.bind("tcp://*:5555")
print("Starting ZMQ Server on Python side")

verbose = True

t_last_change = 0.0
t_change_frequency = 100000.0
setpoints = [20.0]

setpoint_id = 0
i = 0
while True:
    #  Wait for next request from client
    message_in = socket.recv()
    if verbose:
        print("zmq_server.py: Received request [%d]." % i)

    # Convert to individual strings and then to floats
    measurements = bytes.decode(message_in).split(',')
    measurements = [float(m) for m in measurements]

    # Convert to a measurement dict
    measurements = dict({
        'iStatus': measurements[0],
        'Time': measurements[1],
        'VS_MechGenPwr':  measurements[2],
        'VS_GenPwr': measurements[3],
        'GenSpeed': measurements[4],
        'RotSpeed': measurements[5],
        'GenTqMeas': measurements[6],
        'NacelleHeadingDeg': measurements[7],
        'NacelleVaneRad': measurements[8],
        'HorWindV': measurements[9],
        'rootMOOP1': measurements[10],
        'rootMOOP2': measurements[11],
        'rootMOOP3': measurements[12],
        'FA_Acc': measurements[13],
        'NacIMU_FA_Acc': measurements[14],
        'Azimuth': measurements[15],
    })
    if verbose:
        print('i = %d' % i, measurements)

    # # Do some estimation stuff
    # wd_consensus_array = consensus_filtering(...)
    # yaw_angles_opt = floris.optimize(...)

    current_time = measurements['Time']
    
    #if  (current_time - t_last_change) >= t_change_frequency:
    #    print('t=%d s, Updating yaw setpoint...' % current_time)
    #    t_last_change = current_time
    #    setpoint_id += 1
    #    if setpoint_id == len(setpoints):
    #        setpoint_id = 0
    if current_time <= 3.0:
        stpnt_yaw = 0.0
    else:
        stpnt_yaw = 20.0
    #stpnt_yaw = setpoints[setpoint_id]
    stpnt_torque = 0.0
    stpnt_pitch1 = 0.0
    stpnt_pitch2 = 0.0
    stpnt_pitch3 = 0.0
    #stpnt_pitch1 = 3. + 5.*np.sin(measurements['Time'])
    #stpnt_pitch2 = 0.0
    #stpnt_pitch3 = -4.1

    # # Create a message with setpoints to send to ROSCO
    message_out = b"%016.5f, %016.5f, %016.5f, %016.5f, %016.5f" % (
        stpnt_torque, stpnt_yaw, stpnt_pitch1, stpnt_pitch2, stpnt_pitch3
    )

    #  Send reply back to client
    if verbose:
        print("Sending string to ROSCO: %s." % message_out)
    socket.send(message_out)

    i += 1
