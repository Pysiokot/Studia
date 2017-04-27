void handle_message(struct message msg);
void exit_program(int status, char *exit_message);
void exit_handler();
void handle_login(struct message msg);
int get_client_qid(pid_t pid);
void handle_logout(struct message msg);
void handle_default(struct message msg);
void handle_echo(struct message msg);
void handle_upper(struct message msg);
void handle_time(struct message msg);
void handle_terminate(struct message trigger_msg);
void sigint_handler(int signum);

typedef struct client {
    int qid;
    pid_t pid;
} client;