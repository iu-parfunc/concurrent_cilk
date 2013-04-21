#ifndef AGREEMENT_H
#define AGREEMENT_H

#define PORT 5000

typedef enum {REQUEST, RESPONSE, FILE_DATA} frame_type;

#define DATASIZE 1024

typedef struct{
  frame_type type;
  //char data[DATASIZE];
  int data;
  
} frame;

#endif

