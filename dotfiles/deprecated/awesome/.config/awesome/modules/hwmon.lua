-- Awesome wm notification library
local naughty = require("naughty")
-- Awesome wm widget library
local wibox = require("wibox")
-- Awesome wm os management library
local awful = require("awful")

-- Grab the environment
local table = table
local pairs = pairs
local string = string
local math = math

-- Awesome wm specific environment
local timer = timer
local mouse = mouse

-- libsensors library
local sensors_init = sensors_init
local sensors_update = sensors_update


-- Module to create a hardware monitoring widget
module("hwmon")

-- Create Chips[]
Chips = sensors_init()
-- This is the array I want to store the relevant readings in; these will be
-- displayed.
readings = {}

-- I know that my CPU temp sensors are in Chips[0]. To figure stuff like that
-- out, simply try outputting everything and see what it shows.
for key, feature in pairs(Chips[1].features) do
   for key, reading in pairs(feature.readings) do

      -- I want to filter out readings that contain the string "input",
      -- since these are the current temps on my chip
      if string.find(reading.name, "input") then
         table.insert(readings, reading)
      end

   end
end

-- readings[] now contains all sensor data I want.
-- I now create a widget
widget = wibox.widget.textbox("Coretemp: 0°C")

-- On my computer reading 7 in the array contains an overall value for the
-- CPU temp
-- I want this to be my widget text
function widget:update ()
   sensors_update({ readings[1] })
   widget:set_text("Coretemp: " .. math.floor(readings[1].value) .. "°C")
end

-- Update the text every 5 seconds
widget.timer = timer({ timeout = 5 })
widget.timer:connect_signal("timeout",
                            function ()
                               widget:update()
                            end
)
widget.timer:start()
widget:update()

-- This is the widget tooltip, it shows all individual cores
widget.tooltip = awful.tooltip({ objects = { widget },
                                 timer_function = function ()
                                    return widget.tooltip:update()
                                 end
                              })

function widget.tooltip:update ()
   sensors_update(readings)

   return "Overall: " .. math.floor(readings[2].value) .. "°C\n\n" ..
      "Core 1: " .. math.floor(readings[1].value) .. "°C\n" ..
      "Core 2: " .. math.floor(readings[3].value) .. "°C"
end

-- Call this to end the c function. After this call sensors_init() needs to be
-- called again to update, but the application can end safely.
-- This really only needs to be called if you are on an archaic OS that doesn't
-- properly free memory after a program exits (you most likely aren't).
--
-- I don't think it is possible to use this properly in awesome wm, unless you
-- want to free up memory manually, but I wanted to note that it exists.
--
-- sensors_cleanup()
