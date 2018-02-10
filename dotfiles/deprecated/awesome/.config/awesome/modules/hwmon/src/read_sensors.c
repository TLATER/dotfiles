#include <string.h>
#include <sensors/sensors.h>
#include "../include/read_sensors.h"

/* The array containing all chips */
Chip Chips[10] = { NULL };

/* Scan for all readings and store them in Chips[]
 *
 * I know this is fucking horrible, but I'm not the one who created 3 separate
 * objects and didn't interlink them and only supplied a way to iterate through
 * ALL of them if you don't know the internal names that can't be accessed from
 * outside...
 *
 * I guess I could rewrite this to be three functions calling each other, which
 * might improve clarity, but I don't feel like it right now.
 */
void init(void)
{
    const sensors_chip_name *chip_name;
    const sensors_feature *feature;
    const sensors_subfeature *subfeature;

    int chipno;
    int featureno;
    int subfeatureno;

    /* Since the above variables keep growing, we can't use them to build arrays
     * Instead, we use pointers to point to the last available element.
     */
    Reading *subfeature_pos;
    Feature *feature_pos;

    sensors_init(NULL);

    chipno = 0;
    chip_name = sensors_get_detected_chips(NULL, &chipno);
    do {
        sensors_snprintf_chip_name(Chips[chipno - 1].name,
                                   STRING_SIZE,
                                   chip_name);

        feature_pos = &Chips[chipno - 1].features[0];
        featureno = 0;
        feature = sensors_get_features(chip_name, &featureno);
        do {
            strcpy((*feature_pos).name, feature->name);

            subfeature_pos = &(*feature_pos).readings[0];
            subfeatureno = 0;
            subfeature =
                sensors_get_all_subfeatures(chip_name,
                                            feature,
                                            &subfeatureno);
            do {
                (*subfeature_pos) =
                    create_reading(chip_name, feature, subfeature);

                subfeature =
                    sensors_get_all_subfeatures(chip_name,
                                                feature,
                                                &subfeatureno);
                subfeature_pos ++;
            } while (subfeature != NULL);

            feature = sensors_get_features(chip_name, &featureno);
            feature_pos ++;
        } while (feature != NULL);

        chip_name = sensors_get_detected_chips(NULL, &chipno);
    } while (chip_name != NULL);

    sensors_cleanup();
}
