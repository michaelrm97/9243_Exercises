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

void processArgs(char **hostFile, int *nNodes) {

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

  int c;
  while ((c = getopt(argc, argv, "H:hn:v")) != -1) {
    switch (c) {
      case 'H':
        if (optarg) {
          hostFile = optarg;
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
          nNodes = atoi(optarg);
          if (nNodes == 0) {
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

  char **args = &argv[optind];
  // Prepare 2d array of args
  int numArgs = argc - optind + 2; // Executable + args + ssh + hostname
  char** sshArgs = calloc(numArgs + 1, sizeof(char *)); // Also need NULL at end
  if (sshArgs == NULL) {
    fprintf(stderr, "Error: Out of memory");
    return 1;
  }

  sshArgs[0] = "/usr/bin/ssh";

  for (int i = 0; i < argc - optind; i++) {
    sshArgs[i + 2] = args[i];
  }

  FILE *f = fopen(hostFile, "r");
  char *localhost = "localhost";;

  for (int i = 0; i < nNodes; i++) {
    char host[MAX_HOSTNAME_LEN + 1];
    if (f == NULL) {
      // Use localhost
      sshArgs[1] = localhost;
    } else {
      if (fgets(host, MAX_HOSTNAME_LEN + 1, f)) {
        int len = strlen(host);
        if (host[len - 1] == '\n') host[len - 1] = '\0';
        sshArgs[1] = host;
      } else {
        // seek back to start
        fseek(f, 0, SEEK_SET);
        if (fgets(host, MAX_HOSTNAME_LEN + 1, f)) {
          int len = strlen(host);
          if (host[len - 1] == '\n') host[len - 1] = '\0';
          sshArgs[1] = host;
        } else {
          // Close f and use localhost
          fclose(f);
          sshArgs[1] = localhost;
        }
      }
    }
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
