/* Broadcast function
 * Steven Raaijmakers & Marcus van Bergen
 * 10804242 & 10871993
 *
 * This program makes does a broadcast function where the root send all the
 * messages to all nodes but the root
 * Sources: http://bit.ly/2fATNtE http://bit.ly/2g0l1Za
 *
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <mpi.h>
#include "br.h"

int MYMPI_Bcast(void *buffer, int count, MPI_Datatype datatype, int root, MPI_Comm communicator){
    //Here the init vars from the main are sent to the function.
    //Define my_rank, numtaks ect for the operations.
    int my_rank, num_tasks;
    MPI_Status status;
    //Set the rank and num_tasks
    MPI_Comm_rank(communicator, &my_rank);
    MPI_Comm_size(communicator, &num_tasks);

    //If not root, we wait for the root to send us a message and the print it
    if(my_rank != root){
        MPI_Recv(buffer, count, datatype, root, 0, communicator,&status);
        printf("%s %d %s %s\n", "Proc ", my_rank, " says it's msg is: ", buffer);
        return 1;
    }

    //If we are root, we start at 0 and send to all processes but itself
    else {
        for(int i = 0; i < num_tasks; i++){
            if(i != root){
                MPI_Send(buffer, count, datatype, i, 0, communicator);
            }
        }
        //Finish up
        return 1;
    }
}

int main(int argc, char * argv []) {
    //Message is test
    char message[] = "Test";
    //Init the word
    MPI_Init(&argc, &argv);

    /* ROOT process is always 3 in this case
     * So run this code with mpi -n > 3
     */
    int STANDARD_ROOT = 3;
    MYMPI_Bcast(&message, strlen(message), MPI_INT, STANDARD_ROOT, MPI_COMM_WORLD);
    //shutdown the world and finish
    MPI_Finalize();
}
