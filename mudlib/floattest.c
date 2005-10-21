/* -------------------------------------------------------------
 
The following program computes the floating point characteristica
of the driver:

ibeta  is the radix in which numbers are represented, almost always
       2, but occasionally 16, or even 10.
it     is the number of base-ibeta digits in the floating point
       mantissa.
machep is the exponent gives the smallest (most negative) power of
       ibeta that, added to 1.0, gives something different from 1.0
eps    is the floating point number ibeta^machep, loosely referred
       to as the "floating point precision".
negep  is the exponent of the smallest (most negative) power of
       ibeta that, subtracted to 1.0, gives something different
       from 1.0.
epsneg is ibeta^negep, another way of defining floating point
       precision.  Not infrequently epsneg is 0.5 times eps;
       occassionaly eps and epsneg are equal.
iexp   is the number of bits in the exponent (including its sign or
       bias).
minexp is the smallest (most negative) power of ibeta consistent
       with there no being leading zeros in the mantissa.
xmin   is the floating point number ibeta^minxep, generally the
       smallest (in magnitude) useable floating value.
maxexp is the smallest (positive) power of ibeta that causes
       overflow.
xmax   is (1-epsneg)*ibeta^maxexp, generally the largest (in
       magnitude) usable floating value.
irnd   returns a code in range 0..5, giving information on what
       kind of rounding is done in addition (2+5 compliant with
       IEEE standard; 1+4 rounding, but not IEEE standard; 0+3
       truncating, nor desirable), and on how underflow is handled
       (0+1+2 value becomes zero, 3+4+5 exponent is frozen, while
       mantissa is decreased, acquiring leading zeros (IEEE)).
ngrd   is the number of "guard digits" used when truncatiing the
       product of two mantissas to fit the representation.

 -- Croft, 5-Dec-2000
*/

#define CONV(i) ((float)(i))

int ibeta, it, irnd, ngrd, machep, negep, iexp, minexp, maxexp;
float eps, epsneg, xmin, xmax;

void main() {
    int i, itemp, iz, j, k, mx, nxres;
    float a,b,beta,betah,betain,one,t,temp,temp1,tempa,two,y,z,zero;

    one = CONV(1);
    two = one+one;
    zero = one-one;
    a = one;
    do {
        a += a;
        temp = a+one;
        temp1 = temp-a;
    } while (temp1-one == zero);
    b = one;
    do {
        b += b;
        temp = a+b;
        itemp = (int)(temp-a);
    } while (itemp == 0);
    ibeta = itemp;
    beta = CONV(ibeta);
    it = 0;
    b = one;
    do {
        ++it;
        b *= beta;
        temp = b+one;
        temp1 = temp-b;
    } while (temp1-one == zero);
    irnd = 0;
    betah = beta/two;
    temp = a+betah;
    if (temp-a != zero)
        irnd = 1;
    tempa = a+beta;
    temp = tempa+betah;
    if (irnd == 0 && temp-tempa != zero)
        irnd = 2;
    negep = it+3;
    betain = one/beta;
    a = one;
    for (i=1; i<=negep; i++)
        a *= betain;
    b = a;
    for (;;) {
        temp = one-a;
        if (temp-one != zero)
            break;
        a *= beta;
        --negep;
    }
    negep = -negep;
    epsneg = a;
    machep = -it-3;
    a = b;
    for (;;) {
        temp = one+a;
        if (temp-one != zero)
            break;
        a *= beta;
        ++machep;
    }
    eps = a;
    ngrd = 0;
    temp = one+eps;
    if (irnd == 0 && temp*one-one != zero)
        ngrd = 1;
    i = 0;
    k = 1;
    z = betain;
    t = one + eps;
    nxres = 0;
    for (;;) {
        y = z;
        z = y*y;
        a = z*one;
        temp = z*t;
        if (a+a == zero || abs(z) >= y)
            break;
        temp1 = temp*betain;
        if (temp1*beta == z)
            break;
        ++i;
        k += k;
    }
    if (ibeta != 10) {
        iexp = i+1;
        mx = k+k;
    } else {
        iexp = 2;
        iz = ibeta;
        while (k >= iz) {
            iz *= ibeta;
            ++iexp;
        }
        mx = iz+iz-1;
    }
    for (;;) {
        xmin = y;
        y *= betain;
        a = y*one;
        temp = y*t;
        if (a+a != zero && abs(y) < xmin) {
            ++k;
            temp1 = temp*betain;
            if (temp1*beta == y && temp != y) {
                nxres = 3;
                xmin = y;
                break;
            }
        } else
            break;
    }
    minexp = -k;
    if (mx <= k+k-3 && ibeta != 10) {
        mx += mx;
        ++iexp;
    }
    maxexp = mx+minexp;
    irnd += nxres;
    if (irnd >= 2)
        maxexp -= 2;
    i = maxexp+minexp;
    if (ibeta == 2 && !i)
        --maxexp;
    if (i > 20)
        --maxexp;
    if (a != y)
        maxexp -= 2;
    xmax = one-epsneg;
    if (xmax*one != xmax)
        xmax = one-beta*epsneg;
    xmax /= xmin*beta*beta*beta;
    i = maxexp+minexp+3;
    for (j=1; j<=i; j++) {
        if (ibeta == 2)
            xmax += xmax;
        else
            xmax *= beta;
    }
    printf("ibeta = %d\n", ibeta);
    printf("it = %d\n", it);
    printf("machep = %d\n", machep);
    printf("eps = %.16e\n", eps);
    printf("negep = %d\n", negep);
    printf("epsneg = %.16e\n", epsneg);
    printf("iexp = %d\n", iexp);
    printf("minexp = %d\n", minexp);
    printf("xmin = %.16e\n", xmin);
    printf("maxexp = %d\n", maxexp);
    printf("xmax = %.16e\n", xmax);
    printf("irnd = %d\n", irnd);
    printf("ngrd = %d\n", ngrd);
}
