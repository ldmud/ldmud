/*---------------------------------------------------------------------------
 * Iconv optimization.
 *
 *---------------------------------------------------------------------------
 * This is a drop-in replacement for iconv, which has shortcuts for
 * conversion from ASCII to UTF-8 and UTF-8 to UTF-8.
 *---------------------------------------------------------------------------
 */

#ifndef ICONV_OPT_H__
#define ICONV_OPT_H__ 1

#include <iconv.h>

#define USE_ICONV_OPT
#ifdef USE_ICONV_OPT
enum iconv_opt_type
{
    ICONV_OPT_NATIVE,   /* Use native iconv(). */
    ICONV_OPT_ASCII,    /* Conversion from ASCII to UTF-8. */
    ICONV_OPT_UTF8,     /* Conversion from UTF-8 to UTF-8. */
};

struct iconv_opt_s
{
    iconv_t cd;
    enum iconv_opt_type type;
};

typedef struct iconv_opt_s iconv_opt_t;

static INLINE iconv_opt_t iconv_opt_open(const char *tocode, const char *fromcode)
{
    if (strcasecmp(tocode, "UTF-8") == 0 || strcasecmp(tocode, "UTF8") == 0)
    {
        if (strcasecmp(fromcode, "ASCII") == 0)
            return (iconv_opt_t) {.cd = 0, .type = ICONV_OPT_ASCII };
        if (strcasecmp(fromcode, "UTF-8") == 0 || strcasecmp(fromcode, "UTF8") == 0)
            return (iconv_opt_t) {.cd = 0, .type = ICONV_OPT_UTF8 };
    }

    return (iconv_opt_t) {.cd = iconv_open(tocode, fromcode), .type = ICONV_OPT_NATIVE };
}

static INLINE int iconv_opt_close(iconv_opt_t cd)
{
    if (cd.type == ICONV_OPT_NATIVE)
        return iconv_close(cd.cd);
    return 0;
}

static INLINE size_t iconv_opt(iconv_opt_t cd,
                               char **inbuf, size_t *inbytesleft,
                               char **outbuf, size_t *outbytesleft)
{
    switch (cd.type)
    {
        case ICONV_OPT_NATIVE:
            return iconv(cd.cd, inbuf, inbytesleft, outbuf, outbytesleft);

        case ICONV_OPT_ASCII:
        case ICONV_OPT_UTF8:
        {
            size_t count, incount, outcount;
            size_t pos;

            if (!inbuf)
                return 0;

            count = incount = *inbytesleft;
            if (!count)
                return 0;

            outcount = *outbytesleft;
            if (count > outcount)
                count = outcount;

            if (!count)
            {
                errno = E2BIG;
                return (size_t)-1;
            }

            if (cd.type == ICONV_OPT_ASCII)
            {
                for (pos = 0; pos < count; pos++)
                    if ((*inbuf)[pos] < 0)
                        break;
            }
            else /* ICONV_OPT_UTF8 */
            {
                unsigned char* buf = (unsigned char*)*inbuf;
                for (pos = 0; pos < count; pos++)
                {
                    unsigned char c = *buf++;
                    int invalid = 0;

                    if (!(c & 0x80))
                        continue;

                    if ((c & 0xc0) != 0xc0 || c == 0xc0 || c == 0xc1 || c >= 0xf5)
                        break;

                    for (unsigned char bit = 0x20, bytes = 1; bit > 0x02; bytes++, bit >>= 1)
                    {
                        if (bytes + pos == incount)
                        {
                            invalid = EINVAL;
                            break;
                        }

                        if (bytes + pos == outcount)
                        {
                            invalid = E2BIG;
                            break;
                        }

                        if (((*buf++) & 0xc0) != 0x80)
                        {
                            invalid = EILSEQ;
                            break;
                        }

                        if (!(c & bit))
                        {
                            pos += bytes;
                            break;
                        }

                        if (bit == 0x04)
                        {
                            invalid = EILSEQ;
                            break;
                        }
                    }

                    if (invalid)
                    {
                        if (!pos)
                        {
                            errno = invalid;
                            return (size_t)-1;
                        }
                        break;
                    }
                }
            }

            if (!pos)
            {
                errno = EILSEQ;
                return (size_t)-1;
            }

            memcpy(*outbuf, *inbuf, pos);
            *outbuf += pos;
            *inbuf  += pos;
            *outbytesleft -= pos;
            *inbytesleft -= pos;

            return pos;
        }

        default:
            errno = EBADF;
            return (size_t)-1;
    }
}

static INLINE bool iconv_opt_valid(iconv_opt_t cd)
{
    return cd.cd != (iconv_t)-1;
}

static INLINE iconv_opt_t iconv_opt_init()
{
    return (iconv_opt_t) {.cd = (iconv_t)-1, .type = ICONV_OPT_NATIVE };
}

#define iconv_t     iconv_opt_t
#define iconv_open  iconv_opt_open
#define iconv_close iconv_opt_close
#define iconv       iconv_opt
#define iconv_valid iconv_opt_valid
#define iconv_init  iconv_opt_init

#else /* !USE_ICONV_OPT */

#define iconv_valid(cd) ((cd) != (iconv_t)-1)
#define iconv_init()    ((iconv_t) -1)

#endif /* USE_ICONV_OPT */
#endif /* ICONV_OPT_H__ */
