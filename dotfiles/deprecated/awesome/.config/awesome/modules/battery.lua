----------------------------------------------------------------------------
-- @author Tristan Maat &lt;tm@tlater.net&gt;
-- @copyright 2015 Tristan Maat
-- @release v1.0.0
----------------------------------------------------------------------------
-- Import using require("modules/battery")
--
-- Use set_icon_folder(path) to set the folder from which icons are loaded.
-- The folder must be relative to rc.lua and defaults to modules/battery
--
-- To make the widget work, a call to
--
-- (right|left)_layout:add(battery.widget)
--
-- is required, the layouts being wiboxes dedicated to containing widgets,
-- as they are in the default rc.lua
--
-- @module orgmode
----------------------------------------------------------------------------

-- Awesome wm notification library
local naughty = require("naughty")
-- Awesome wm widget library
local wibox = require("wibox")
-- Awesome wm os management library
local awful = require("awful")

-- Grab the environment
local io = io
local math = math

-- Awesome wm specific environment
local timer = timer
local mouse = mouse


-- Module to create a battery widget
module("battery")

-- The path in which the icons are stored, relative to rc.lua. Needs a traling
-- slash
local ICON_PATH = "modules/battery/"

-- The total charge. Does (probably) not change when running, saves a bit of io
local full_charge

-- Helper method to read an entire file
local read_file = function (file)
   local f = io.input(file)
   file_content = f:read("*all")
   f:close()

   return file_content
end

full_charge = read_file("/sys/class/power_supply/BAT1/charge_full")

-- Sets the folder to be used for the battery icons relative to rc.lia. Default
-- is modules/battery
set_icon_folder = function (folder)
   ICON_PATH = folder
end

-- Test if we currently are plugged in (charging) or not
get_charging = function ()
   local status = read_file("/sys/class/power_supply/BAT1/status")

   -- If we are not discharging, we are charging
   status = status ~= "Discharging\n"

   return status
end

-- Get the current remaining battery charge
get_charge_ratio = function ()
   local charge = read_file("/sys/class/power_supply/BAT1/charge_now")

   -- Calculate the ratio in %
   local ratio = charge / full_charge * 100

   return ratio
end

-- Get the image that should currently be mapped to the widget
get_battery_image = function()
   local image

   -- To calculate which image should be displayed
   local ratio = get_charge_ratio()
   local is_charging = get_charging()
   -- We have 46 images, map the ratio (0-100) to that set
   local image_number = 46 - math.floor(ratio * 0.46)

   -- If our battery is (nearly) full
   if ratio > 96 then
      image = "full" .. (is_charging and "_charged" or "") .. ".png"

   -- If our battery is (nearly) empty
   elseif ratio == 0 then
      image = "empty" .. (is_charging and "_charged" or "") .. ".png"

   else
      image = image_number .. (is_charging and "_charged" or "") .. ".png"
   end

   return awful.util.getdir("config") .. "/" .. ICON_PATH .. image
end


-- Create the widget
widget = wibox.widget.imagebox()
widget:set_image(get_battery_image())
widget:fit(45, 45)

-- Hovering should show the current charge in %
widget:connect_signal("mouse::enter",
                      function ()
                         local battery_level =
                            math.floor(get_charge_ratio()) .. "%"

                         options = { text = "Battery level: " .. battery_level,
                                     timeout = 0, hover_timeout = 0.5,
                                     screen = mouse.screen }

                         widget.hover = naughty.notify(options)
                      end
)

widget:connect_signal("mouse::leave",
                      function ()
                         naughty.destroy(widget.hover)
                      end
)

-- Update the widget every 3 seconds
battery_timer = timer({ timeout = 3 })
battery_timer:connect_signal("timeout",
                             function ()
                                widget:set_image(get_battery_image())
                             end
)
battery_timer:start()

-- This stuff still needs to go somewhere
   -- if rat > 20 then
   --    low = true
   -- end

   -- if rat < 20 and not low then
   --    naughty.notify({ text = "Battery level under 20%", timeout = 6, fg="#f5b900"})
   --    low = true
   -- end

   -- if rat > 10 then
   --    critical = true
   -- end

   -- if rat < 10 and not critical then
   --    naughty.notify({ text = "Battery level under 10%", timeout = 6, fg="#f53700"})
   --    critical = true
   -- end
