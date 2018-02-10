/* Temporary beacause I am too lazy to do proper array extension
 * ARRAY_SIZE needs to be at least as large as the number of subfeatures
 * STRING_SIZE needs to be at least as long as the longest string
 */
#define STRING_SIZE 50
#define ARRAY_SIZE 200

/* The internal datastructure for readings in c, converted to a lua
 * representation at lua_init() and used to update these later.
 */
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

/* The initializing function for lua, loads a global table called Chips
 * into lua and stores all necessary information in it.
 *
 * lua name is sensors_init()
 */
static int lua_init(lua_State *);
/* The updating function for lua, takes a table of readings from the stack and
 * returns the updated readings (they need to be reinserted into the global
 * variable if required), it is assumed that a new private structure is made.
 *
 * lua name is sensors_update(table)
 */
static int lua_update_reading(lua_State *);
/* Used to clean libsensors when ending program */
static int lua_cleanup(lua_State *);
/* Helper function to create readings */
Reading create_reading(const sensors_chip_name *,
                       const sensors_feature *,
                       const sensors_subfeature *);
/* Helper function to update readings */
void update_reading(Reading *reading);
