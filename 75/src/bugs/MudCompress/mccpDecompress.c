/*

 * Client decompression module for the mud client compression protocol.

 * See http://homepages.ihug.co.nz/~icecube/compress/ for more details.

 *

 * mccpDecompress.c - module code. Link this with your client code.

 *

 * Oliver Jowett <icecube$ihug.co.nz>. Demangle address as needed.

 *

 * This code is placed in the public domain.

 *

 */



/* Modified: 981203 */



/* See mccpDecompress.h for API information */



#include <stdlib.h>

#include <stdio.h>

#include <string.h>



#include <zlib.h>



#include "mccpDecompress.h"



/* Telnet values we're interested in */



#define IAC             255

#define DONT            254

#define DO              253

#define WONT            252

#define WILL            251

#define SB              250

#define SE              240



#define TELOPT_COMPRESS 85



/* Server sends  IAC WILL COMPRESS

 * We reply with IAC DO COMPRESS

 *

 * Later the server sends IAC SB COMPRESS WILL SE, and immediately following

 * that, begins compressing

 *

 * Compression ends on a Z_STREAM_END, no other marker is used

 */



static char will_sig[] = { IAC, WILL, TELOPT_COMPRESS, 0 };

static char do_sig[] =   { IAC, DO, TELOPT_COMPRESS, 0 };

static char on_sig[] =   { IAC, SB, TELOPT_COMPRESS, WILL, SE, 0 };



/* this is used on every call to mudcompress_receive */

#define will_sig_len 3

#define on_sig_len 5



/* "Opaque" state object */



struct mc_state_s {

    z_stream *stream;      /* stream we're using */



    unsigned char *inbuf;  /* input buffer (data from mud) */

    unsigned int insize;   /* .. and how much is used */

    unsigned int inalloc;  /* .. and how much is allocated */

    

    unsigned char *outbuf; /* output buffer (data to user) */

    unsigned int outsize;  /* .. and how much is used */

    unsigned int outalloc; /* .. and how much is allocated */



    int error;

    int response;



    unsigned long comp;

    unsigned long uncomp;

};



/* Initialise a new state object */

mc_state *mudcompress_new(void)

{

    mc_state *state;



    state = malloc(sizeof(*state));

    state->stream   = NULL;   /* Not decompressing */

    state->inalloc  = 2048;

    state->outalloc = 2048;

    state->inbuf    = malloc(state->inalloc);

    state->outbuf   = malloc(state->outalloc);

    state->insize   = 0;

    state->outsize  = 0;

    state->error    = 0;

    state->comp     = 0;

    state->uncomp   = 0;

    state->response = 0;



    return state;

}



/* Clean up a state object */

void mudcompress_delete(mc_state *state)

{

    if (state->stream) {

        inflateEnd(state->stream);

        free(state->stream);

    }

    

    free(state->inbuf);

    free(state->outbuf);

    free(state);

}



/* zlib helpers */



static void *zlib_alloc(void *opaque, unsigned int items, unsigned int size)

{

    return calloc(items, size);

}



static void zlib_free(void *opaque, void *address)

{

    free(address);

}



static void grow_inbuf(mc_state *state, int needed)

{

    int old = state->inalloc;

    

    while (state->inalloc < state->insize + needed)

        state->inalloc *= 2;



    if (old != state->inalloc)

        state->inbuf = realloc(state->inbuf, state->inalloc);

}

        

static void grow_outbuf(mc_state *state, int needed)

{

    int old = state->outalloc;

    

    while (state->outalloc < state->outsize + needed)

        state->outalloc *= 2;



    if (old != state->outalloc)

        state->outbuf = realloc(state->outbuf, state->outalloc);

}



static void decompress_inbuf(mc_state *state)

{

    int status;

    

    /* We are now decompressing from inbuf to outbuf */



    if (!state->insize)

        return; /* nothing to decompress? */



    state->stream->next_in = state->inbuf;

    state->stream->next_out = state->outbuf + state->outsize;

    state->stream->avail_in = state->insize;

    state->stream->avail_out = state->outalloc - state->outsize;

    

    status = inflate(state->stream, Z_PARTIAL_FLUSH);



    if (status == Z_OK || status == Z_STREAM_END) {

        /* Successful decompression */



        /* Remove used data from inbuf */

        state->comp += state->insize - state->stream->avail_in;

        state->uncomp += state->stream->next_out - state->outbuf;

        

        memmove(state->inbuf, state->stream->next_in, state->stream->avail_in);

        state->insize = state->stream->avail_in;



        /* Update outbuf pointers */

        state->outsize = state->stream->next_out - state->outbuf;



        /* Done */



        if (status == Z_STREAM_END) {

            /* Turn off compression too */



            grow_outbuf(state, state->insize);



            memcpy(state->outbuf + state->outsize, state->inbuf, state->insize);

            state->outsize += state->insize;

            state->insize = 0;



            inflateEnd(state->stream);

            free(state->stream);

            state->stream = NULL;

        }

        

        return;

    }



    if (status == Z_BUF_ERROR) {

        /* Full buffers? Maybe we need more output space.. */



        if (state->outsize * 2 > state->outalloc) {

            grow_outbuf(state, state->outalloc);

            decompress_inbuf(state);

        }



        return;

    }



    /* Error */

    state->error = 1;

}



/* We received some data */

void mudcompress_receive(mc_state *state, const char *data, unsigned len)

{

    int i;

    

    if (state->error)

        return;



    if (!state->stream) {

        int residual = -1;

        int clen;

        

        /* Just copy to outbuf. Also copy any residual inbuf */



        grow_outbuf(state, len + state->insize);

        memcpy(state->outbuf + state->outsize, data, len);

        state->outsize += len;

        memcpy(state->outbuf + state->outsize, state->inbuf, state->insize);

        state->outsize += state->insize;

        state->insize = 0;



        /* Check for Magic Marker. ugh this is messy */

        for (i=0; i < state->outsize; i++) {

            if (state->outbuf[i] == IAC) {

                if (i + 1 < state->outsize && state->outbuf[i+1] == IAC) {

                    /* IAC IAC - ignore */

                    i++;

                    continue;

                }



                clen = (i + will_sig_len < state->outsize) ? will_sig_len : state->outsize - i;

                

                if (!memcmp(&state->outbuf[i], will_sig, clen)) {

                    if (clen != will_sig_len) {

                        /* Partial match. Save it. */

                        residual = i;

                        break;

                    }

                    

                    /* Say we'll do compression. remove sig from inbuf */



                    state->response = 1;



                    memmove(&state->outbuf[i],

                            &state->outbuf[i + will_sig_len],

                            state->outsize - will_sig_len);

                    state->outsize -= strlen(will_sig);

                    i--;

                    continue;

                }

                    

                clen = (i + on_sig_len < state->outsize) ? on_sig_len : state->outsize - i;

                

                if (!memcmp(&state->outbuf[i], on_sig, clen)) {

                    if (clen != on_sig_len) {

                        /* Partial match. Save it. */

                        residual = i;

                        break;

                    }

                    

                    /* Switch to compression */

                    /* copy any compressible bits to our inbuf */



                    grow_inbuf(state, state->outsize - i - strlen(on_sig));

                    

                    memcpy(state->inbuf,

                           state->outbuf + i + strlen(on_sig),

                           state->outsize - i - strlen(on_sig));



                    state->insize = state->outsize - i - strlen(on_sig);



                    /* clean up our output buffer */

                    state->outsize = i;



                    /* init stream */

                    state->stream = malloc(sizeof(z_stream));

                    state->stream->zalloc = zlib_alloc;

                    state->stream->zfree = zlib_free;

                    state->stream->opaque = NULL;



                    if (inflateInit(state->stream) != Z_OK) {

                        state->error = 1;

                        free(state->stream);

                        state->stream = NULL;

                        return;

                    }



                    /* Continue with decompression */

                    break;

                }

            }

        }



        if (!state->stream) { /* didn't start decompressing? */

            /* We might have some residual, copy to inbuf for later checking */



            if (residual != -1) {

                grow_inbuf(state, state->outsize - residual);

                memcpy(state->inbuf + state->insize, state->outbuf + residual, state->outsize - residual);

                state->outsize = residual;

            }

            

            return;

        }

    } else {

        /* New data to decompress. Copy to inbuf */

        grow_inbuf(state, len);

        memcpy(state->inbuf + state->insize, data, len);

        state->insize += len;

    }



    decompress_inbuf(state);

}



/* How much data is available? */

int mudcompress_pending(mc_state *state)

{

    return state->error ? 0 : state->outsize;

}



/* Was there an error? */

int mudcompress_error(mc_state *state)

{

    return state->error;

}



/* Get some data */

int mudcompress_get(mc_state *state, char *buf, int size)

{

    int copied;

    

    if (state->error || !state->outsize)

        return 0;



    if (size > state->outsize)

        copied = state->outsize;

    else

        copied = size;



    memcpy(buf, state->outbuf, copied);

    state->outsize -= copied;

    if (state->outsize)

        memmove(state->outbuf, state->outbuf + copied, state->outsize);



    /* Do some more decompression */

    decompress_inbuf(state);

    

    return copied;

}



void mudcompress_stats(mc_state *state, unsigned long *comp, unsigned long *uncomp)

{

    *comp = state->comp;

    *uncomp = state->uncomp;

}



const char *mudcompress_response(mc_state *state)

{

    if (state->response) {

        state->response = 0;

        return do_sig;

    }



    return NULL;

}



int mudcompress_compressing(mc_state *state)

{

    return (state->stream != NULL);

}

