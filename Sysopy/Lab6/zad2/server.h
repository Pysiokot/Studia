void recieveMessage();
void exitProgram(int status, char *exit_message);
mqd_t createQueue();
void sendMessageQID(mqd_t client_qid, message *msg);
mqd_t getClientQueue(pid_t client_pid);
void sendMessagePID(pid_t client_pid, message *msg);
void loginHandler(message msg);
void logoutHandler(message msg);
void handleECHO(message msg);
void handleUPPER(message msg);
void handleTIME(message msg);
long emptyQueue();
void killServer();
void exitHandler();
void recieveMessage();
void sigint_handler(int signum);

struct client {
    pid_t pid;
    mqd_t qid;
};