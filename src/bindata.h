#ifndef _BINDATA_H
#define _BINDATA_H

#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

typedef uint32_t word32;

///////// Packet data

typedef word32* PACKET;

void* newPacket(int length);
void dumpPacket(PACKET p, int length);
void freePacket(void* packet);


void setPacketByte(PACKET p, int b, int data);
void setPacketBits(PACKET p, int start, int end, int data);

void setPacketString(PACKET p, int start, char* string, int l, char t);

int getPacketByte(PACKET p, int b);
int getPacketBits(PACKET p, int start, int end);

#endif


