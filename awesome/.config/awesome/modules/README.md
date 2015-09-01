# Awesome wm dotfiles

Any modules in WIP are to be treated as a proof of concept or experiments with
APIs, none of them contain any working code. Other modules are very much WIP and
can be improved as well, but they are at least far enough to be useable.

Feel free to use any, most these modules can be loaded into awesome wm using

```lua
        require('modules/<modulename>')
```

assuming the file structure has the rc.lua in a folder that contains a folder
called "modules".

This repo will grow as I improve my centralized file problem...


## Widget modules

Widget modules require a call to

```lua
        -- More code until widget declaration

        require("modules/<modulename>")

        -- More code until widget alignment

        (right|left)_layout:add(<modulename>.widget)
```

## Hwmon module

To run the hwmon module run make in the hwmon folder and copy the resulting
sensors.so file into your lua library.

Then a call to

```lua
        require("sensors")

        -- More code until widget declaration

        require("modules/hwmon")

        -- More code until widget alignment

        (right|left)_layout:add(hwmon.widget)
```

should correctly import the widget. Change the code that the widget loads to
match your sensor data, the default settings are likely to result in a crash.

If you have any trouble running make leave an issue, it is to be expected.
