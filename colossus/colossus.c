#include "colossus.h"

#include <stdio.h>
#include <string.h>

// TODO: Research how this was actually done on Colossus tape.
#define START_MARKER "@@@"

typedef struct BaudotMapping {
    char *baudot;
    char ascii;
} BaudotMapping;

BaudotMapping baudot_mapping[] = {
        {"10100", 'A'},
        {"11000", 'B'},
        {"10110", 'C'},
};

char read_baudot_line(char* baudot_code) {
    if (strcmp(baudot_code, START_MARKER) == 0) {
        printf("Start marker detected!\n");
        return '\0';
    } else {
        for (int i = 0; i < sizeof(baudot_mapping) / sizeof(BaudotMapping); i++) {
            if (strcmp(baudot_code, baudot_mapping[i].baudot) == 0) {
                return baudot_mapping[i].ascii;
            }
        }
    }
    return '\0';
}

char calculate_delta(char* baudot_stream) {
    char *baudot_code = strtok(baudot_stream, " ");
    char *prev_baudot_code = NULL;

    while (baudot_code != NULL) {
        if(prev_baudot_code != NULL) {
            char delta_baudot[6];
            for (int i = 0; i < 5; i++) {
                delta_baudot[i] = ((prev_baudot_code[i] - '0') ^ (baudot_code[i] - '0')) + '0';
            }
            delta_baudot[5] = '\0';
            char delta_char = read_baudot_line(delta_baudot);
            return delta_char;
        }
        prev_baudot_code = baudot_code;
        baudot_code = strtok(NULL, " ");
    }

    return '\0';
}