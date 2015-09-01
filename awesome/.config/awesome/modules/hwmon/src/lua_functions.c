#include <lua.h>
#include <lauxlib.h>
#include <lualib.h>
#include <string.h>
#include <sensors/sensors.h>
#include "../include/lua_functions.h"

int luaopen_read_sensors(lua_State *);
void create_lua_reading(lua_State *, Reading, int number);
void create_lua_feature(lua_State *, const sensors_feature *);
void create_lua_chip(lua_State *, const sensors_chip_name *);

/* An array to hold all readings, used to update the lua-side readings */
Reading readings[200];

/* The lua function interface */
int luaopen_sensors(lua_State *L)
{
    lua_register(L, "sensors_init", lua_init);
    lua_register(L, "sensors_update", lua_update_reading);
    lua_register(L, "sensors_cleanup", lua_cleanup);

    return 0;
}

/* Create a table from a reading struct within lua WARNING, OPEN TABLE */
void create_lua_reading(lua_State *L, Reading reading, int number)
{
    lua_newtable(L);
    lua_pushstring(L, reading.chip_name);
    lua_setfield(L, -2, "chip");
    lua_pushstring(L, reading.feature_name);
    lua_setfield(L, -2, "feature");
    lua_pushstring(L, reading.subfeature_name);
    lua_setfield(L, -2, "name");
    lua_pushnumber(L, reading.value);
    lua_setfield(L, -2, "value");
    lua_pushnumber(L, number);
    lua_setfield(L, -2, "number");
}

/* Create a table from a chip name that represents a chip WARNING, OPEN TABLE */
void create_lua_chip(lua_State *L, const sensors_chip_name *chip_name)
{
    char temp[ARRAY_SIZE];

    sensors_snprintf_chip_name(temp, STRING_SIZE, chip_name);

    lua_newtable(L);
    lua_pushstring(L, temp);
    lua_setfield(L, -2, "name");
}

/* Creates a table from a feature name that represents a chip WARNING, OPEN
 * TABLE
 */
void create_lua_feature(lua_State *L, const sensors_feature *feature)
{
    char temp[STRING_SIZE];

    strcpy(temp, feature->name);

    lua_newtable(L);
    lua_pushstring(L, temp);
    lua_setfield(L, -2, "name");
}

/* Update the lua structure based on the internal structure */
static int lua_update_reading(lua_State *L)
{
    int i;
    int size;
    int number;

    Reading temp;

    luaL_checktype(L, 1, LUA_TTABLE);

    lua_len(L, 1);
    size = lua_tointeger(L, 2);

    for (i = 1; i <= size; i++) {
        /* Make sure the stack is clean at each call */
        lua_settop(L, 1);
        lua_rawgeti(L, 1, i);

        luaL_checktype(L, 2, LUA_TTABLE);
        lua_getfield(L, 2, "number");
        luaL_checktype(L, 3, LUA_TNUMBER);
        number = (int)lua_tonumber(L, 3);

        temp = readings[number];

        update_reading(&temp);
        create_lua_reading(L, temp, number);
        lua_rawseti(L, 1, i);
    }

    luaL_checktype(L, 1, LUA_TTABLE);

    /* Return a table filled with updated readings */
    return 1;
}

static int lua_init(lua_State *L)
{
    const sensors_chip_name *chip_name;
    const sensors_feature *feature;
    const sensors_subfeature *subfeature;

    /* Defining subfeatureno last results in core dump... */
    int subfeatureno;
    int chipno;
    int featureno;

    int total_readingno = 0;

    int i, j, k;

    Reading temp;

    sensors_init(NULL);

    /* Chips[] */
    lua_newtable(L);
    i = 0;
    chipno = 0;
    chip_name = sensors_get_detected_chips(NULL, &chipno);
    do {
        /* chip (+ chip.name) */
        create_lua_chip(L, chip_name);

        /* chip.features[] */
        lua_newtable(L);
        j = 0;
        featureno = 0;
        feature = sensors_get_features(chip_name, &featureno);
        do {
            create_lua_feature(L, feature);

            /* feature.readings[] */
            lua_newtable(L);
            k = 0;
            subfeatureno = 0;
            subfeature = sensors_get_all_subfeatures(chip_name,
                                                     feature,
                                                     &subfeatureno);
            do {
                temp = create_reading(chip_name, feature, subfeature);
                create_lua_reading(L, temp, total_readingno);

                readings[total_readingno] = temp;
                total_readingno++;

                /* feature.readings[k] = reading */
                lua_rawseti(L, -2, k++);
                subfeature =
                    sensors_get_all_subfeatures(chip_name,
                                                feature,
                                                &subfeatureno);
            } while (subfeature != NULL);

            lua_setfield(L, -2, "readings");

            /* chip.features[j] = feature */
            lua_rawseti(L, -2, j++);
            feature = sensors_get_features(chip_name, &featureno);
        } while (feature != NULL);

        lua_setfield(L, -2, "features");

        /* Chips[i] = chip */
        lua_rawseti(L, -2, i++);
        chip_name = sensors_get_detected_chips(NULL, &chipno);
    } while (chip_name != NULL);

    luaL_checktype(L, 1, LUA_TTABLE);
    lua_settop(L, 1);

    return 1;
}

/* Libsensors cleanup */
static int lua_cleanup(lua_State *L)
{
    sensors_cleanup();

    return 0;
}
