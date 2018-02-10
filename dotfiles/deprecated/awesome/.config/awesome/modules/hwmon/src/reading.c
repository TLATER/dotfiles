#include <string.h>
#include <sensors/sensors.h>
#include "../include/read_sensors.h"

/* Create a reading */
Reading create_reading(const sensors_chip_name *chip_input,
                       const sensors_feature *feature_input,
                       const sensors_subfeature *subfeature_input)
{
    Reading reading;

    reading.chip = chip_input;
    reading.feature = feature_input;
    reading.subfeature = subfeature_input;

    sensors_snprintf_chip_name(reading.chip_name,
                               STRING_SIZE,
                               chip_input);
    strcpy(reading.feature_name, feature_input->name);
    strcpy(reading.subfeature_name, subfeature_input->name);

    sensors_get_value(reading.chip,
                      reading.subfeature->number,
                      &reading.value);

    return reading;
}

/* Update a reading */
void update_reading(Reading *reading)
{
    sensors_get_value((*reading).chip,
                      (*reading).subfeature->number,
                      &(*reading).value);
}
