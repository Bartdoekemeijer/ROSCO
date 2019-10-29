# Copyright 2019 NREL

# Licensed under the Apache License, Version 2.0 (the "License"); you may not use
# this file except in compliance with the License. You may obtain a copy of the
# License at http://www.apache.org/licenses/LICENSE-2.0

# Unless required by applicable law or agreed to in writing, software distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied. See the License for the
# specific language governing permissions and limitations under the License.

import numpy as np
import datetime
from ccblade import CCAirfoil, CCBlade
from scipy import interpolate, gradient
from WTC_toolbox import turbine as wtc_turbine

# Some useful constants
now = datetime.datetime.now()
pi = np.pi
rad2deg = np.rad2deg(1)
deg2rad = np.deg2rad(1)
rpm2RadSec = 2.0*(np.pi)/60.0
RadSec2rpm = 60/(2.0 * np.pi)

class Controller():
    """
    Class Controller used to calculate controller tunings parameters


    Methods:
    -------
    tune_controller

    Parameters:
    -----------
    controller_params: dict
                       Dictionary containing controller paramaters that need to be defined
    """

    def __init__(self, controller_params):
        ''' 
        Load controller tuning parameters from input dictionary
        '''

        print('---------------------------------------------------------------------------')
        print('Tuning a reference wind turbine controller using the ROSCO toolbox')
        print('Developed by Nikhar J. Abbas, 2019')
        print('---------------------------------------------------------------------------')

        # Controller Flags
        self.LoggingLevel = controller_params['LoggingLevel']
        self.F_LPFType = controller_params['F_LPFType']
        self.F_NotchType = controller_params['F_NotchType']
        self.IPC_ControlMode = controller_params['IPC_ControlMode']
        self.VS_ControlMode = controller_params['VS_ControlMode']
        self.PC_ControlMode = controller_params['PC_ControlMode']
        self.Y_ControlMode = controller_params['Y_ControlMode']
        self.SS_Mode = controller_params['SS_Mode']
        self.WE_Mode = controller_params['WE_Mode']
        self.PS_Mode = controller_params['PS_Mode']

        # Necessary parameters
        self.zeta_pc = controller_params['zeta_pc']
        self.omega_pc = controller_params['omega_pc']
        self.zeta_vs = controller_params['zeta_vs']
        self.omega_vs = controller_params['omega_vs']

        # Optional parameters, default to standard if not defined
        if controller_params['min_pitch']:
            self.min_pitch = controller_params['min_pitch']
        else:
            self.min_pitch = 0.      # Default to zero degrees min pitch
        
        if controller_params['max_pitch']:
            self.max_pitch = controller_params['max_pitch']
        else:
            self.max_pitch = 90*deg2rad      # Default to 90 degrees max pitch
        
        if controller_params['ss_vsgain']:
            self.ss_vsgain = controller_params['ss_vsgain']
        else:
            self.ss_vsgain = 1.      # Default to 100% setpoint shift
        
        if controller_params['ss_pcgain']:
            self.ss_pcgain = controller_params['ss_pcgain']
        else:
            self.ss_pcgain = 0.001      # Default to 0.1% setpoint shift
        
        if controller_params['ss_cornerfreq']:
            self.ss_cornerfreq = controller_params['ss_cornerfreq']
        else:
            self.ss_cornerfreq = .62831850001     # Default to 10 second time constant 
        
        if controller_params['ps_percent']:
            self.ps_percent = controller_params['ps_percent']
        else:
            self.ps_percent = 0.75      # Default to 75% peak shaving

    def tune_controller(self, turbine):
        """
        Given a turbine model, tune a controller based on the NREL generic controller tuning process

        Parameters:
        -----------
        turbine : class
                  Turbine class containing necessary turbine information to accurately tune the controller. 
        """
        # -------------Load Parameters ------------- #
        # Re-define Turbine Parameters for shorthand
        J = turbine.J                           # Total rotor inertial (kg-m^2) 
        rho = turbine.rho                       # Air density (kg/m^3)
        R = turbine.rotor_radius                    # Rotor radius (m)
        Ar = np.pi*R**2                         # Rotor area (m^2)
        Ng = turbine.Ng                         # Gearbox ratio (-)
        rated_rotor_speed = turbine.rated_rotor_speed               # Rated rotor speed (rad/s)


        # -------------Define Operation Points ------------- #
        TSR_rated = rated_rotor_speed*R/turbine.v_rated  # TSR at rated

        # separate wind speeds by operation regions
        v_below_rated = np.arange(turbine.v_min,turbine.v_rated,0.5)             # below rated
        v_above_rated = np.arange(turbine.v_rated+0.5,turbine.v_max,0.5)             # above rated
        v = np.concatenate((v_below_rated, v_above_rated))

        # separate TSRs by operations regions
        TSR_below_rated = np.ones(len(v_below_rated))*turbine.Cp.TSR_opt # below rated     
        TSR_above_rated = rated_rotor_speed*R/v_above_rated                     # above rated
        TSR_op = np.concatenate((TSR_below_rated, TSR_above_rated))   # operational TSRs

        # Find expected operational Cp values
        Cp_above_rated = turbine.Cp.interp_surface(0,TSR_above_rated[0])             # Cp during rated operation (not optimal). Assumes cut-in bld pitch to be 0
        Cp_op_br = np.ones(len(v_below_rated)) * turbine.Cp.max              # below rated
        Cp_op_ar = Cp_above_rated * (TSR_above_rated/TSR_rated)**3           # above rated
        Cp_op = np.concatenate((Cp_op_br, Cp_op_ar))                # operational CPs to linearize around
        pitch_initial_rad = turbine.pitch_initial_rad
        TSR_initial = turbine.TSR_initial

        # initialize variables
        pitch_op = np.empty(len(TSR_op))
        dCp_beta = np.empty(len(TSR_op))
        dCp_TSR = np.empty(len(TSR_op))

        # ------------- Find Linearized State "Matrices" ------------- #
        for i in range(len(TSR_op)):
            # Find pitch angle as a function of expected operating CP for each TSR
            Cp_TSR = np.ndarray.flatten(turbine.Cp.interp_surface(turbine.pitch_initial_rad, TSR_op[i]))     # all Cp values for a given tsr
            Cp_op[i] = np.clip(Cp_op[i], np.min(Cp_TSR), np.max(Cp_TSR))        # saturate Cp values to be on Cp surface
            f_cp_pitch = interpolate.interp1d(Cp_TSR,pitch_initial_rad)         # interpolate function for Cp(tsr) values
            pitch_op[i] = f_cp_pitch(Cp_op[i])                                  # expected operation blade pitch values
            dCp_beta[i], dCp_TSR[i] = turbine.Cp.interp_gradient(pitch_op[i],TSR_op[i])       # gradients of Cp surface in Beta and TSR directions
        
        # Full Cp surface gradients
        dCp_dbeta = dCp_beta/np.diff(pitch_initial_rad)[0]
        dCp_dTSR = dCp_TSR/np.diff(TSR_initial)[0]
        
        # Linearized system derivatives
        dtau_dbeta = Ng/2*rho*Ar*R*(1/TSR_op)*dCp_dbeta*v**2
        dtau_dlambda = Ng/2*rho*Ar*R*v**2*(1/(TSR_op**2))*(dCp_dTSR*TSR_op - Cp_op)
        dlambda_domega = R/v/Ng
        dtau_domega = dtau_dlambda*dlambda_domega

        # Second order system coefficients
        A = dtau_domega/J             # Plant pole
        B_tau = -Ng**2/J              # Torque input  
        B_beta = dtau_dbeta/J         # Blade pitch input 

        # Wind Disturbance Input
        dlambda_dv = -(TSR_op/v)
        # dtau_dv = dtau_dlambda*dlambda_dv
        # B_v = dtau_dv/J # wind speed input - currently unused 


        # separate and define below and above rated parameters
        A_vs = A[0:len(v_below_rated)]          # below rated
        A_pc = A[len(v_below_rated):len(v)]     # above rated
        B_tau = B_tau * np.ones(len(v_below_rated))
        B_beta = B_beta[len(v_below_rated):len(v)]

        # Find gain schedule
        self.pc_gain_schedule = ControllerTypes()
        self.pc_gain_schedule.second_order_PI(self.zeta_pc, self.omega_pc,A_pc,B_beta,linearize=True,v=v_above_rated)
        self.vs_gain_schedule = ControllerTypes()
        self.vs_gain_schedule.second_order_PI(self.zeta_vs, self.omega_vs,A_vs,B_tau,linearize=False,v=v_below_rated)

        # Find K for Komega_g^2
        self.vs_rgn2K = 0.5*rho*Ar*R**5 * turbine.Cp.max / (turbine.Cp.TSR_opt**3 * Ng)
        self.vs_refspd = min(turbine.Cp.TSR_opt * turbine.v_rated/R, turbine.rated_rotor_speed) * Ng

        # Define some setpoints
        self.vs_minspd = (turbine.Cp.TSR_opt * turbine.v_min / turbine.rotor_radius) * Ng

        # Store some variables
        self.v = v                                  # Wind speed (m/s)
        self.pitch_op = pitch_op
        self.pitch_op_pc = pitch_op[len(v_below_rated):len(v)]
        self.TSR_op = TSR_op
        self.A = A 
        self.B_beta = B_beta
        self.B_tau = B_tau
        # --- Might want these to debug
        # self.Cp_op = Cp_op
        # self.v_below_rated = v_below_rated

        # Peak Shaving
        self.ps = ControllerBlocks()
        self.ps.peak_shaving(self, turbine)

class ControllerBlocks():
    '''
    Class ControllerBlocks defines tuning parameters for additional controller features or "blocks"

    Methods:
    --------
    peak_shaving

    '''
    def __init__(self):
        pass
    
    def peak_shaving(self,controller, turbine):
        ''' 
        Define minimum blade pitch angle for peak shaving routine based on a maximum allowable thrust 

        Parameters:
        -----------
        controller: class
                    Controller class containing controller operational information
        turbine: class
                 Turbine class containing necessary wind turbine information for controller tuning
        '''

        # Re-define Turbine Parameters for shorthand
        J = turbine.J                           # Total rotor inertial (kg-m^2) 
        rho = turbine.rho                       # Air density (kg/m^3)
        R = turbine.rotor_radius                    # Rotor radius (m)
        A = np.pi*R**2                         # Rotor area (m^2)
        Ng = turbine.Ng                         # Gearbox ratio (-)
        rated_rotor_speed = turbine.rated_rotor_speed               # Rated rotor speed (rad/s)

        # Initialize some arrays
        Ct_op = np.empty(len(controller.TSR_op),dtype='float64')
        Ct_max = np.empty(len(controller.TSR_op),dtype='float64')
        beta_min = np.empty(len(controller.TSR_op),dtype='float64')
        # Find unshaved rotor thurst coefficients and associated rotor thrusts
        # for i in len(controller.TSR_op):
        for i in range(len(controller.TSR_op)):
            Ct_op[i] = turbine.Ct.interp_surface(controller.pitch_op[i],controller.TSR_op[i])
            T = 0.5 * rho * A * controller.v**2 * Ct_op

        # Define minimum max thrust and initialize pitch_min
        Tmax = controller.ps_percent * np.max(T)
        pitch_min = np.ones(len(controller.pitch_op)) * controller.min_pitch

        # Modify pitch_min if max thrust exceeds limits
        for i in range(len(controller.TSR_op)):
            # Find Ct values for operational TSR
            # Ct_tsr = turbine.Ct.interp_surface(turbine.pitch_initial_rad, controller.TSR_op[i])
            Ct_tsr = turbine.Ct.interp_surface(turbine.pitch_initial_rad,controller.TSR_op[i])
            # Define max Ct values
            Ct_max[i] = Tmax/(0.5 * rho * A * controller.v[i]**2)
            if T[i] > Tmax:
                Ct_op[i] = Ct_max[i]
            else:
                Ct_max[i] = np.minimum( np.max(Ct_tsr), Ct_max[i])
            # Define minimum pitch angle
            f_pitch_min = interpolate.interp1d(Ct_tsr, turbine.pitch_initial_rad, bounds_error=False, fill_value=(turbine.pitch_initial_rad[0],turbine.pitch_initial_rad[-1]))
            pitch_min[i] = f_pitch_min(Ct_max[i])

        # save some outputs for analysis or future work
        self.Tshaved = 0.5 * rho * A * controller.v**2 * Ct_op
        self.pitch_min = pitch_min
        self.v = controller.v
        self.Ct_max = Ct_max
        self.Ct_op = Ct_op
        self.T = T

class ControllerTypes():
    '''
    Class ControllerTypes used to define any types of controllers that can be tuned. 
        Generally, calculates gains based on some pre-defined tuning parameters. 

    Methods:
    --------
    second_order_PI
    '''
    def __init__(self):
        pass

    def second_order_PI(self,zeta,om_n,A,B,linearize=False,v=None):
        '''
        Define proportional integral gain schedule for a closed
            loop system with a standard second-order form.

        Parameters:
        -----------
        zeta : int (-)
               Desired damping ratio 
        om_n : int (rad/s)
               Desired natural frequency 
        A : array_like (1/s)
            Plant poles (state transition matrix)
        B : array_like (varies)
            Plant numerators (input matrix)
        linearize : bool, optional
                    If 'True', find a gain scheduled based on a linearized plant.
        v : array_like (m/s)
            Wind speeds for linearized plant model, if desired. 
        '''
        # Linearize system coefficients w.r.t. wind speed if desired
        if linearize:
            pA = np.polyfit(v,A,1)
            pB = np.polyfit(v,B,1)
            A = pA[0]*v + pA[1]
            B = pB[0]*v + pB[1]

        # Calculate gain schedule
        self.Kp = 1/B * (2*zeta*om_n + A)
        self.Ki = om_n**2/B           

class FileProcessing():
    """
    Class FileProcessing used to write out controller 
        parameter files need to run ROSCO

    Methods:
    -----------
    write_param_file
    write_rotor_performance
    """

    def __init__(self):
        pass
    def write_param_file(self, turbine, controller, param_file='DISCON.IN'):
        """
        Print the controller parameters to the DISCON.IN input file for the generic controller

        Parameters:
        -----------
        turbine: class
                 Turbine class containing turbine operation information (ref speeds, etc...)
        controller: class
                    Controller class containing controller operation information (gains, etc...)
        param_file: str, optional
            filename to for parameter input file, should be DISCON.IN
        """
        print('Writing new controller parameter file parameter file %s.' % param_file)
        # Should be obvious what's going on here...
        file = open(param_file,'w')
        file.write('! Controller parameter input file for the %s wind turbine\n' % turbine.TurbineName)
        file.write('!    - File written using NREL Reference Controller tuning logic on %s\n' % now.strftime('%m/%d/%y'))
        file.write('\n')
        file.write('!------- DEBUG ------------------------------------------------------------\n')
        file.write('{0:<12d}        ! LoggingLevel		- {{0: write no debug files, 1: write standard output .dbg-file, 2: write standard output .dbg-file and complete avrSWAP-array .dbg2-file}}\n'.format(controller.LoggingLevel))
        file.write('\n')
        file.write('!------- CONTROLLER FLAGS -------------------------------------------------\n')
        file.write('{0:<12d}        ! F_LPFType			- {{1: first-order low-pass filter, 2: second-order low-pass filter}}, [rad/s] (currently filters generator speed and pitch control signals\n'.format(controller.F_LPFType))
        file.write('{0:<12d}        ! F_NotchType		- Notch on the measured generator speed {{0: disable, 1: enable}}\n'.format(controller.F_NotchType))
        file.write('{0:<12d}        ! IPC_ControlMode	- Turn Individual Pitch Control (IPC) for fatigue load reductions (pitch contribution) {{0: off, 1: 1P reductions, 2: 1P+2P reductions}}\n'.format(controller.IPC_ControlMode))
        file.write('{0:<12d}        ! VS_ControlMode	- Generator torque control mode in above rated conditions {{0: constant torque, 1: constant power, 2: TSR tracking PI control}}\n'.format(controller.VS_ControlMode))
        file.write('{0:<12d}        ! PC_ControlMode    - Blade pitch control mode {{0: No pitch, fix to fine pitch, 1: active PI blade pitch control}}\n'.format(controller.PC_ControlMode))
        file.write('{0:<12d}        ! Y_ControlMode		- Yaw control mode {{0: no yaw control, 1: yaw rate control, 2: yaw-by-IPC}}\n'.format(controller.Y_ControlMode))
        file.write('{0:<12d}        ! SS_Mode           - Setpoint Smoother mode {{0: no setpoint smoothing, 1: introduce setpoint smoothing}}\n'.format(controller.SS_Mode))
        file.write('{0:<12d}        ! WE_Mode           - Wind speed estimator mode {{0: One-second low pass filtered hub height wind speed, 1: Immersion and Invariance Estimator (Ortega et al.)}}\n'.format(controller.WE_Mode))
        file.write('{0:<12d}        ! PS_Mode           - Peak shaving mode {{0: no peak shaving, 1: implement peak shaving}}\n'.format(controller.PS_Mode))
        file.write('\n')
        file.write('!------- FILTERS ----------------------------------------------------------\n') 
        file.write('{:<13.2f}       ! F_LPFCornerFreq	- Corner frequency (-3dB point) in the low-pass filters, [rad/s]\n'.format(turbine.bld_edgewise_freq * 1/4)) 
        file.write('{:<13.2f}       ! F_LPFDamping		- Damping coefficient [used only when F_FilterType = 2]\n'.format(0.0))
        file.write('{:<13.2f}       ! F_NotchCornerFreq	- Natural frequency of the notch filter, [rad/s]\n'.format(0.0))
        file.write('{:<6.1f}{:<13.1f} ! F_NotchBetaNumDen	- Two notch damping values (numerator and denominator, resp) - determines the width and depth of the notch, [-]\n'.format(0.0,0.0))
        file.write('{:<014.6f}      ! F_SSCornerFreq    - Corner frequency (-3dB point) in the first order low pass filter for the setpoint smoother, [rad/s].\n'.format(controller.ss_cornerfreq))
        file.write('\n')
        file.write('!------- BLADE PITCH CONTROL ----------------------------------------------\n')
        file.write('{:<11d}         ! PC_GS_n			- Amount of gain-scheduling table entries\n'.format(len(controller.pitch_op_pc)))
        file.write('{}              ! PC_GS_angles	    - Gain-schedule table: pitch angles\n'.format(''.join('{:<4.6f}  '.format(controller.pitch_op_pc[i]) for i in range(len(controller.pitch_op_pc)))))            
        file.write('{}              ! PC_GS_KP		- Gain-schedule table: pitch controller kp gains\n'.format(''.join('{:<4.6f}  '.format(controller.pc_gain_schedule.Kp[i]) for i in range(len(controller.pc_gain_schedule.Kp)))))
        file.write('{}              ! PC_GS_KI		- Gain-schedule table: pitch controller ki gains\n'.format(''.join('{:<4.6f}  '.format(controller.pc_gain_schedule.Ki[i]) for i in range(len(controller.pc_gain_schedule.Ki)))))
        file.write('{}              ! PC_GS_KD			- Gain-schedule table: pitch controller kd gains\n'.format(''.join('{:<1.1f}  '.format(0.0) for i in range(len(controller.pc_gain_schedule.Ki)))))
        file.write('{}              ! PC_GS_TF			- Gain-schedule table: pitch controller tf gains (derivative filter)\n'.format(''.join('{:<1.1f}  '.format(0.0) for i in range(len(controller.pc_gain_schedule.Ki)))))
        file.write('{:<014.5f}      ! PC_MaxPit			- Maximum physical pitch limit, [rad].\n'.format(controller.max_pitch))
        file.write('{:<014.5f}      ! PC_MinPit			- Minimum physical pitch limit, [rad].\n'.format(controller.min_pitch))
        file.write('{:<014.5f}      ! PC_MaxRat			- Maximum pitch rate (in absolute value) in pitch controller, [rad/s].\n'.format(turbine.max_pitch_rate))
        file.write('{:<014.5f}      ! PC_MinRat			- Minimum pitch rate (in absolute value) in pitch controller, [rad/s].\n'.format(turbine.min_pitch_rate))
        file.write('{:<014.5f}      ! PC_RefSpd			- Desired (reference) HSS speed for pitch controller, [rad/s].\n'.format(turbine.rated_rotor_speed*turbine.Ng))
        file.write('{:<014.5f}      ! PC_FinePit		- Record 5: Below-rated pitch angle set-point, [rad]\n'.format(controller.min_pitch))
        file.write('{:<014.5f}      ! PC_Switch			- Angle above lowest minimum pitch angle for switch, [rad]\n'.format(1 * deg2rad))
        file.write('{:<11d}         ! Z_EnableSine		- Enable/disable sine pitch excitation, used to validate for dynamic induction control, will be removed later, [-]\n'.format(0))
        file.write('{:<014.5f}      ! Z_PitchAmplitude	- Amplitude of sine pitch excitation, [rad]\n'.format(0.0))
        file.write('{:<014.5f}      ! Z_PitchFrequency	- Frequency of sine pitch excitation, [rad/s]\n'.format(0.0))
        file.write('\n')
        file.write('!------- INDIVIDUAL PITCH CONTROL -----------------------------------------\n')
        file.write('{:<13.1f}       ! IPC_IntSat		- Integrator saturation (maximum signal amplitude contribution to pitch from IPC), [rad]\n'.format(0.0))
        file.write('{:<6.1f}{:<13.1f} ! IPC_KI			- Integral gain for the individual pitch controller: first parameter for 1P reductions, second for 2P reductions, [-]\n'.format(0.0,0.0))
        file.write('{:<6.1f}{:<13.1f} ! IPC_aziOffset		- Phase offset added to the azimuth angle for the individual pitch controller, [rad]. \n'.format(0.0,0.0))
        file.write('{:<13.1f}       ! IPC_CornerFreqAct - Corner frequency of the first-order actuators model, to induce a phase lag in the IPC signal {{0: Disable}}, [rad/s]\n'.format(0.0))
        file.write('\n')
        file.write('!------- VS TORQUE CONTROL ------------------------------------------------\n')
        file.write('{:<014.5f}      ! VS_GenEff			- Generator efficiency mechanical power -> electrical power, [should match the efficiency defined in the generator properties!], [-]\n'.format(turbine.GenEff))
        file.write('{:<014.5f}      ! VS_ArSatTq		- Above rated generator torque PI control saturation, [Nm]\n'.format(turbine.rated_torque))
        file.write('{:<014.5f}      ! VS_MaxRat			- Maximum torque rate (in absolute value) in torque controller, [Nm/s].\n'.format(turbine.max_torque_rate))
        file.write('{:<014.5f}      ! VS_MaxTq			- Maximum generator torque in Region 3 (HSS side), [Nm].\n'.format(turbine.rated_torque*1.1))
        file.write('{:<014.5f}      ! VS_MinTq			- Minimum generator (HSS side), [Nm].\n'.format(0.0))
        file.write('{:<014.5f}      ! VS_MinOMSpd		- Optimal mode minimum speed, cut-in speed towards optimal mode gain path, [rad/s]\n'.format(controller.vs_minspd))
        file.write('{:<014.5f}      ! VS_Rgn2K			- Generator torque constant in Region 2 (HSS side), [N-m/(rad/s)^2]\n'.format(controller.vs_rgn2K))
        file.write('{:<014.5f}      ! VS_RtPwr			- Wind turbine rated power [W]\n'.format(turbine.rated_power))
        file.write('{:<014.5f}      ! VS_RtTq			- Rated torque, [Nm].\n'.format(turbine.rated_torque))
        file.write('{:<014.5f}      ! VS_RefSpd			- Rated generator speed [rad/s]\n'.format(controller.vs_refspd))
        file.write('{:<11d}         ! VS_n				- Number of generator PI torque controller gains\n'.format(1))
        file.write('{:<014.5f}      ! VS_KP				- Proportional gain for generator PI torque controller [1/(rad/s) Nm]. (Only used in the transitional 2.5 region if VS_ControlMode =/ 2)\n'.format(controller.vs_gain_schedule.Kp[-1]))
        file.write('{:<014.5f}      ! VS_KI				- Integral gain for generator PI torque controller [1/rad Nm]. (Only used in the transitional 2.5 region if VS_ControlMode =/ 2)\n'.format(controller.vs_gain_schedule.Ki[-1]))
        file.write('{:<13.2f}       ! VS_TSRopt			- Power-maximizing region 2 tip-speed-ratio [rad].\n'.format(turbine.Cp.TSR_opt))
        file.write('\n')
        file.write('!------- SETPOINT SMOOTHER ---------------------------------------------\n')
        file.write('{:<13.5f}       ! SS_VSGain         - Variable speed torque controller setpoint smoother gain, [-].\n'.format(controller.ss_vsgain))
        file.write('{:<13.5f}       ! SS_PCGain         - Collective pitch controller setpoint smoother gain, [-].\n'.format(controller.ss_pcgain))
        file.write('\n')
        file.write('!------- WIND SPEED ESTIMATOR ---------------------------------------------\n')
        file.write('{:<13.3f}       ! WE_BladeRadius	- Blade length [m]\n'.format(turbine.rotor_radius))
        file.write('{:<11d}         ! WE_CP_n			- Amount of parameters in the Cp array\n'.format(1))
        file.write(          '{}    ! WE_CP - Parameters that define the parameterized CP(lambda) function\n'.format(''.join('{:<2.1f} '.format(0.0) for i in range(4))))
        file.write('{:<13.1f}		! WE_Gamma			- Adaption gain of the wind speed estimator algorithm [m/rad]\n'.format(0.0))
        file.write('{:<13.1f}       ! WE_GearboxRatio	- Gearbox ratio [>=1],  [-]\n'.format(turbine.Ng))
        file.write('{:<014.5f}      ! WE_Jtot			- Total drivetrain inertia, including blades, hub and casted generator inertia to LSS, [kg m^2]\n'.format(turbine.J))
        file.write('{:<13.3f}       ! WE_RhoAir			- Air density, [kg m^-3]\n'.format(turbine.rho))
        file.write(     '{:<16s}    ! PerfFileName      - File containing rotor performance tables (Cp,Ct,Cq)\n'.format('"Cp_Ct_Cq.txt"'))
        file.write('{:<7d} {:<10d}  ! PerfTableSize     - Size of rotor performance tables, first number refers to number of blade pitch angles, second number referse to number of tip-speed ratios\n'.format(len(turbine.Cp.pitch_initial_rad),len(turbine.Cp.TSR_initial)))
        file.write('{:<11d}         ! WE_FOPoles_N      - Number of first-order system poles used in EKF\n'.format(len(controller.A)))
        file.write('{}              ! WE_FOPoles_v      - Wind speeds corresponding to first-order system poles [m/s]\n'.format(''.join('{:<4.2f} '.format(controller.v[i]) for i in range(len(controller.v)))))
        file.write('{}              ! WE_FOPoles        - First order system poles\n'.format(''.join('{:<10.8f} '.format(controller.A[i]) for i in range(len(controller.A)))))
        file.write('\n')
        file.write('!------- YAW CONTROL ------------------------------------------------------\n')
        file.write('{:<13.1f}       ! Y_ErrThresh		- Yaw error threshold. Turbine begins to yaw when it passes this. [rad^2 s]\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_IPC_IntSat		- Integrator saturation (maximum signal amplitude contribution to pitch from yaw-by-IPC), [rad]\n'.format(0.0))
        file.write('{:<11d}         ! Y_IPC_n			- Number of controller gains (yaw-by-IPC)\n'.format(1))
        file.write('{:<13.1f}       ! Y_IPC_KP			- Yaw-by-IPC proportional controller gain Kp\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_IPC_KI			- Yaw-by-IPC integral controller gain Ki\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_IPC_omegaLP		- Low-pass filter corner frequency for the Yaw-by-IPC controller to filtering the yaw alignment error, [rad/s].\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_IPC_zetaLP		- Low-pass filter damping factor for the Yaw-by-IPC controller to filtering the yaw alignment error, [-].\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_MErrSet			- Yaw alignment error, set point [rad]\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_omegaLPFast		- Corner frequency fast low pass filter, 1.0 [Hz]\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_omegaLPSlow		- Corner frequency slow low pass filter, 1/60 [Hz]\n'.format(0.0))
        file.write('{:<13.1f}       ! Y_Rate			- Yaw rate [rad/s]\n'.format(0.0))
        file.write('\n')
        file.write('!------- TOWER FORE-AFT DAMPING -------------------------------------------\n')
        file.write('{:<11d}         ! FA_KI				- Integral gain for the fore-aft tower damper controller, -1 = off / >0 = on [rad s/m] - !NJA - Make this a flag\n'.format(-1))
        file.write('{:<13.1f}       ! FA_HPF_CornerFreq	- Corner frequency (-3dB point) in the high-pass filter on the fore-aft acceleration signal [rad/s]\n'.format(0.0))
        file.write('{:<13.1f}       ! FA_IntSat			- Integrator saturation (maximum signal amplitude contribution to pitch from FA damper), [rad]\n'.format(0.0))
        file.write('\n')
        file.write('!------- PEAK SHAVING -------------------------------------------\n')
        file.write('{:<11d}         ! PS_BldPitchMin_N  - Number of values in minimum blade pitch lookup table (should equal number of values in PS_WindSpeeds and PS_BldPitchMin)\n'.format(len(controller.ps.pitch_min)))
        file.write('{}              ! PS_WindSpeeds       - Wind speeds corresponding to minimum blade pitch angles [m/s]\n'.format(''.join('{:<4.2f} '.format(controller.ps.v[i]) for i in range(len(controller.ps.v)))))
        file.write('{}              ! PS_BldPitchMin          - Minimum blade pitch angles [rad]'.format(''.join('{:<10.8f} '.format(controller.ps.pitch_min[i]) for i in range(len(controller.ps.pitch_min)))))
        file.close()

    def write_rotor_performance(self,turbine,txt_filename='Cp_Ct_Cq.txt'):
        '''
        Write text file containing rotor performance data

        Parameters:
        ------------
            txt_filename: str, optional
                          Desired output filename to print rotor performance data. Default is Cp_Ct_Cq.txt
        '''
        
        file = open(txt_filename,'w')
        # Headerlines
        file.write('# ----- Rotor performance tables for the {} wind turbine ----- \n'.format(turbine.TurbineName))
        file.write('# ------------ Written on {} using the ROSCO toolbox ------------ \n\n'.format(now.strftime('%b-%d-%y')))

        # Pitch angles, TSR, and wind speed
        file.write('# Pitch angle vector - x axis (matrix columns) (deg)\n')
        for i in range(len(turbine.Cp.pitch_initial_rad)):
            file.write('{:0.4}   '.format(turbine.Cp.pitch_initial_rad[i] * rad2deg))
        file.write('\n# TSR vector - y axis (matrix rows) (-)\n')
        for i in range(len(turbine.TSR_initial)):
            file.write('{:0.4}    '.format(turbine.Cp.TSR_initial[i]))
        file.write('\n# Wind speed vector - z axis (m/s)\n')
        file.write('{:0.4}    '.format(turbine.v_rated))
        file.write('\n')
        
        # Cp
        file.write('\n# Power coefficient\n\n')
        for i in range(len(turbine.Cp.TSR_initial)):
            for j in range(len(turbine.Cp.pitch_initial_rad)):
                file.write('{0:.6f}   '.format(turbine.Cp_table[i,j]))
            file.write('\n')
        file.write('\n')
        
        # Ct
        file.write('\n#  Thrust coefficient\n\n')
        for i in range(len(turbine.Ct.TSR_initial)):
            for j in range(len(turbine.Ct.pitch_initial_rad)):
                file.write('{0:.6f}   '.format(turbine.Ct_table[i,j]))
            file.write('\n')
        file.write('\n')
        
        # Cq
        file.write('\n# Torque coefficient\n\n')
        for i in range(len(turbine.Cq.TSR_initial)):
            for j in range(len(turbine.Cq.pitch_initial_rad)):
                file.write('{0:.6f}   '.format(turbine.Cq_table[i,j]))
            file.write('\n')
        file.write('\n')
        file.close()