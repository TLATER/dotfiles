#include <stdio.h>
#include <sensors/sensors.h>
#include "../include/read_sensors.h"

int main(void);

/* Print all current sensor readings to stdout */
int main()
{
    int testchip = 1;
    int testfeature = 0;
    int test = 0;

    init();

    printf("%s\t%f\n",
           Chips[testchip].features[testfeature].readings[test].subfeature_name,
           Chips[testchip].features[testfeature].readings[test].value);

    return 0;
}
