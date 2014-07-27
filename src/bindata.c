// Bindata -- binary data manipulation, C side.
// Most of this isn't mine, it's Edwin's.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#include "bindata.h"

void* newPacket(int length) {
    int words = length >> 5;
    if ((length & 31)!=0) words++; // Need one more if it's not exactly aligned
    void* pckt = malloc(words * sizeof(word32));
    memset(pckt, words*sizeof(word32), 0);
    return pckt;
}

void dump_binary(int x)
{
    unsigned int z, i;
    for (z = 1 << 31, i=1; z > 0; z >>= 1, i++)
    {
        putchar(((x & z) == z) ? '1' : '0');
        if (i%8==0) putchar(' ');
    }
}

void dumpPacket(PACKET p, int length) {
    int i;
    int words = length >> 5;
    if ((length & 31)!=0) words++; // Need one more if it's not exactly aligned

    for (i=0; i<words; ++i) {
        printf("%x      \t", p[i]);
        dump_binary(p[i]);
        printf("\n");
    }
    printf("\n");
}

word32 setnbits(word32 v, int startoff, int endoff, int data) {
    // Clear the bits in v from startoff to endoff.
    word32 two_m = (0xffffffff << (32-startoff)) ^ 0xffffffff;
    word32 two_n = (0xffffffff << (32-endoff)) ^ 0xffffffff;

    // Right, (two_m - two_n) is now ones in the position we want to clear.
    // So, flip it, to make it zeros we want to clear, ones elsewhere,
    // then AND v with it to pick out the bits we want to keep.

    v = v & ((two_m - two_n) ^ 0xffffffff);

    // Put the data in the right place (from startbit to endbit, all other
    // bits zero).
    word32 nv = data << (32-endoff);

//    printf("%x\n", nv);
//    printf("%x\n", v | nv);

    // Put nv in the right place in v.
    return (v | nv);
}



word32 getnbits(word32 v, int startbit, int endbit) {
    //printf("Getting %d, %d\n", startbit, endbit);
    return (v << startbit) >> (startbit + (32-endbit));
}

void setPacketByte(PACKET p, int b, int data) {
    p[b] = data;
}

void setPacketBits(PACKET p, int start, int end, int data) {
    int startb = start >> 5; // Word the start bits are in
    int endb = end >> 5;     // Word the end bits are in

    int startoff = start & 31; // Offset of start bits in that word
    int endoff = (end & 31)+1; // Offset of end bits in that word

    if (startb==endb) { // In the same word, easy...
        p[startb] = setnbits(p[startb], startoff, endoff, data);
    } else {
        // Set the least significant 32-[startoff] bits of p[startb] to
        // [data] >> [endoff].
        p[startb] = setnbits(p[startb], startoff, 32, data >> endoff);

        // Set the most significant [endoff] bits of p[endb] to the least
        // significant [endoff] bits of [data].
        p[endb] = setnbits(p[endb], 0, endoff, 
                           (data << (32 - endoff)) >> (32 - endoff));
    }
}

void setPacketString(PACKET p, int start, char* s, int l, char t) {
    while(*s!=t && (l!=0)) {
        setPacketBits(p, start, start+7, (int)(*s));
        start+=8;
        ++s;
        --l;
    }
}

int getPacketByte(PACKET p, int b) {
    return p[b];
}

int getPacketBits(PACKET p, int start, int end) {
    //printf("getPacketBits -- Start: %d, End: %d\n", start, end);
    int startb = start >> 5; // Word the start bits are in
    int endb = end >> 5;     // Word the end bits are in

    int startoff = start & 31; // Offset of start bits in that word
    int endoff = 1 + (end & 31); // Offset of end bits in that word
    int rv;

    if (startb==endb) {
        rv = getnbits(p[startb], startoff, endoff);
    } else {
        int startn = getnbits(p[startb], startoff, 32);
        int endn = getnbits(p[endb], 0, endoff);
        rv = (startn << endoff) + endn; 
    }
    return rv;
}


void freePacket(void* packet) {
    free(packet);
}
