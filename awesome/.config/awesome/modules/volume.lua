----------------------------------------------------------------------------
-- @author Tristan Maat &lt;tm@tlater.net&gt;
-- @copyright 2015 Tristan Maat
-- @release v1.0.0
----------------------------------------------------------------------------
-- Import using require("modules/volume")
--
-- Depends on amixer
--
-- relative_change(amount) changes the current volume by amount %
-- negative values reduce volume, positive increase
--
-- toggle_mute() toggles mute state
--
-- Bind these to something in rc.lua or other configuration file to use the
-- module (e.g. volume.toggle_mute()).
--
-- @module volume
----------------------------------------------------------------------------

-- Awesome wm notification library
local naughty = require("naughty")

-- Grab the environment
local os = os
local io = io
local table = table
local tonumber = tonumber
local tostring = tostring


module("volume")

-- The state the soundserver is currently in
-- state["volume"] and state["mute"]
-- relate to the current volume and the mute state, respectively
state = {}

-- Scrape the current volume levels and mute state from amixer
read_state = function ()
   -- Issue command and read output
   local f = io.popen("amixer sget Master")
   local f_content = f:read("*all")
   f:close()

   -- Amixer output will contain a line similar to this:
   --
   -- Mono: Playback 50 [57%] [-27.75dB] [off]
   --
   -- This is what we are scraping
   -- The indexes of the brackets in the amixer output
   local bracket1 = 0
   local bracket2 = 0
   -- The values between the brackets
   local bracket_values = {}

   -- Search for all brackets and output the contents to a list
   while true do
      bracket1, bracket2 = f_content:find("%b[]", bracket2 + 1)
      if bracket1 == nil then break end

      local bracket_state = f_content:sub(bracket1 + 1, bracket2 - 1)
      table.insert(bracket_values, bracket_state)
   end

   -- Interpret the contents in a machine readable format
   -- [xx%] -> integer
   state["volume"] =
      tonumber(bracket_values[1]:sub(1, bracket_values[1]:len() - 1))

   -- [on|off] -> boolean
   state["muted"] = bracket_values[3] == "off"

end

-- Toggles the mute state
toggle_mute = function()
   read_state()

   -- If we are currently already muted
   if state["muted"] then
      os.execute("amixer set Master unmute")
   else
      os.execute("amixer set Master mute")
   end

   notification:show_mute()
end

-- Alters the current volume level
relative_change = function(amount)
   if (amount > 0) then
      os.execute("amixer set Master " .. tostring(amount) .. "%+")
   else
      -- Avoid printing a negative string
      amount = amount * -1
      os.execute("amixer set Master " .. tostring(amount) .. "%-")
   end

   notification:show_volume()
end

-- The volume notification (awesome wm specific)
notification = {}
notification.id = nil
function notification:show (msg)
   local options = { text = msg, timeout = 6, replaces_id = self.id}
   self.id = naughty.notify(options).id
end
function notification:show_volume ()
   read_state()
   self:show("Volume: " .. state["volume"] .. "%")
end
function notification:show_mute ()
   local message
   read_state()
   if state["muted"] then
      message = "Muted"
   else
      message = "Unmuted"
   end
   self:show(message)
end
