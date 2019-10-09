#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define VERSION "1.0"

#define MAX_HOSTNAME_LEN 256

void printHelp() {
  printf("Usage: rstart [OPTION]... EXECUTABLE-FILE NODE-OPTION...\n\
\n\
  -H HOSTFILE list of host names\n\
  -h          this usage message\n\
  -n N        fork N node processes\n\
  -v          print version information\n\
\n\
\n\
Forks N copies (one copy if -n not given) of\n\
EXECUTABLE-FILE.  The NODE-OPTIONs are passed as arguments to the node\n\
processes.  The hosts on which node processes are started are given in\n\
HOSTFILE, which defaults to `hosts'.  If the file does not exist,\n\
`localhost' is used.\n");
}

void printVersion() {
  printf("Version %s\n", VERSION);
}

/*
 * Processes arguments from commend line
 * Returns number of arguments once all options are removed
 * 0 if there is an error when processing arguments or if the program
 * is not to continue
 */
int processArgs(int argc, char **argv, char **hostFile, int *nNodes, char ***args) {
  int c;
  while ((c = getopt(argc, argv, "H:hn:v")) != -1) {
    switch (c) {
      case 'H':
        if (optarg) {
          *hostFile = optarg;
        } else {
          printHelp();
          return 0;
        }
        break;
      case 'h':
        printHelp();
        return 0;
      case 'n':
        if (optarg) {
          *nNodes = atoi(optarg);
          if (*nNodes == 0) {
            printf("Error: n must be > 0\n");
            return 0;
          }
        } else {
          printHelp();
          return 0;
        }
        break;
      case 'v':
        printVersion();
        return 0;
      default:
        printHelp();
        return 0;
    }
  }

  if (optind == argc) { // Need executable-file
    printHelp();
    return 0;
  }

  *args = &argv[optind];
  return argc - optind;
}

char **prepareSSHArgs(int numArgs, char **args) {
  // Prepare 2d char array of args
  char** sshArgs = calloc(numArgs + 3, sizeof(char *)); // Need ssh + hostname + NULL
  if (sshArgs == NULL) {
    fprintf(stderr, "Error: Out of memory");
    return NULL;
  }

  sshArgs[0] = "/usr/bin/ssh";
  memcpy(sshArgs + 2, args, numArgs * sizeof(char *));
  return sshArgs;
}

char *getNextHost(FILE **f, char *host) {
  if (*f == NULL) {
    // Use localhost
    return "localhost";
  } else {
    if (fgets(host, MAX_HOSTNAME_LEN + 1, *f)) {
      int len = strlen(host);
      if (host[len - 1] == '\n') host[len - 1] = '\0';
      return host;
    } else {
      // seek back to start
      fseek(*f, 0, SEEK_SET);
      if (fgets(host, MAX_HOSTNAME_LEN + 1, *f)) {
        int len = strlen(host);
        if (host[len - 1] == '\n') host[len - 1] = '\0';
        return host;
      } else {
        // Close f and use localhost
        fclose(*f);
        *f = NULL;
        return "localhost";
      }
    }
  }
}

int rstart(char **args) {
  // Fork and then call ssh
  switch((fork())) {
    case 0: // In child
      execv("/usr/bin/ssh", args);
      exit(1);
    case -1: // Failure
      perror("Fork failed");
      return -1;
    default: // In parent
      return 0;
  }
}

int main(int argc, char **argv) {
  char *hostFile = "hosts";
  int nNodes = 1;
  char **args;
  
  int numArgs = processArgs(argc, argv, &hostFile, &nNodes, &args);
  if (numArgs == 0) return 0;
  
  char **sshArgs = prepareSSHArgs(numArgs, args);
  if (sshArgs == NULL) return 0;

  FILE *f = fopen(hostFile, "r");
  char host[MAX_HOSTNAME_LEN + 1];

  for (int i = 0; i < nNodes; i++) {
    sshArgs[1] = getNextHost(&f, host);
    if (rstart(sshArgs)) {
      free(sshArgs);
      fprintf(stderr, "Error calling rstart\n");
      return 1;
    }
  }

  if (f != NULL) {
    fclose(f);
  }

  free(sshArgs);
  return 0;
}
