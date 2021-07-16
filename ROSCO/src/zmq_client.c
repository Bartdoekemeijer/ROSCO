#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <zmq.h>


void delete_blank_spaces_in_string(char *s)
{
	int  i,k=0;
	 for(i=0;s[i];i++)
    {
     	s[i]=s[i+k];
     	if(s[i]==' '|| s[i]=='\t')
     	{
		  k++;
		  i--;
	    }
    }
}


int zmq_client (
    char *zmq_address,
    double measurements[15],
    double setpoints[5]
)
{
    int verbose = 1; // Variable to define verbose
    int i = 0, ns = 3, nm = 15, ml = 16;  // Number of setpoints and measurements, respectively, and float precision (character length)
    int slm = ml * nm + (nm - 1);  // Measurement string length
    int sls = ml * ns + (ns - 1);  // Setpoint string length
    if (verbose == 1) {
        printf ("Connecting to ZeroMQ server at %s...\n", zmq_address);
    }

    // Open connection with ZeroMQ server
    void *context = zmq_ctx_new ();
    void *requester = zmq_socket (context, ZMQ_REQ);
    zmq_connect (requester, zmq_address);  // string_to_zmq is something like "tcp://localhost:5555"

    // Create a string with measurements to be sent to ZeroMQ server (e.g., Python)
    char a[slm], string_to_ssc[slm], b[ml];
    // char b[slm];
    sprintf(a, "%016.5f,", measurements[0]);
    // printf ("zmq_client.c: a[ml]: measurements[0]: %s\n", a);
    i = 1;
    while (i <= nm) {
        sprintf(b, "%016.5f,", measurements[i]);
        strcat(a, b);  // Concatenate b to a
        // printf ("zmq_client.c: b[ml]: measurements[i]: %s\n", b);
        // printf (" --> zmq_client.c: a[ml]: measurements[i]: %s\n", a);
        i = i + 1;
    }
    strncpy(string_to_ssc, a, slm);

    // Print the string
    if (verbose == 1) {
        printf ("zmq_client.c: string_to_ssc: %s…\n", string_to_ssc);
    }

    // Core ZeroMQ communication: receive data and send back signals
    char string_from_ssc[sls];  // Buffer to receive message in
    zmq_send (requester, string_to_ssc, slm, 0);
    zmq_recv (requester, string_from_ssc, sls, 0);

    if (verbose == 1) {
        printf ("zmq_client.c: Received a response: %s\n", string_from_ssc);
    }

    // Convert string_from_ssc string to separate floats
    delete_blank_spaces_in_string(string_from_ssc);
    char *pt;
    pt = strtok (string_from_ssc,",");
    i = 0;
    while (pt != NULL) {
        double dtmp = atof(pt);
        if (verbose == 1) {
            printf("pt subloop: %s (var), %f (double) \n", pt, dtmp);
            printf("zmq_client.c: setpoint[%d]: %f \n", i, dtmp);
        }
        pt = strtok (NULL, ",");
        setpoints[i] = dtmp;  // Save values to setpoints
        i = i + 1;
    }

    // Close connection
    zmq_close (requester);
    zmq_ctx_destroy (context);
    return 0;
}