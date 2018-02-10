#define STRING_SIZE 50

typedef struct
{
    const sensors_chip_name *chip;
    const sensors_feature *feature;
    const sensors_subfeature *subfeature;

    char chip_name[STRING_SIZE];
    char feature_name[STRING_SIZE];
    char subfeature_name[STRING_SIZE];

    double value;
} Reading;

typedef struct
{
    char name[STRING_SIZE];
    Reading readings[100];
} Feature;

typedef struct
{
    char name[STRING_SIZE];
    Feature features[50];
} Chip;

/* This will contain all chips after init is called */
extern Chip Chips[10];

/* Initializes the Chip[] */
extern void init(void);
void update_subfeature(Reading *);
Reading create_reading(const sensors_chip_name *,
                       const sensors_feature *,
                       const sensors_subfeature *);
