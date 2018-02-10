----------------------------------------------------------------------------
-- @author Tristan Maat &lt;tm@tlater.net&gt;
-- @copyright 2015 Tristan Maat
-- @release v1.0.0
----------------------------------------------------------------------------
-- Import using require("modules/orgmode")
--
-- Requires a running emacs server, org-mode with a t and an n setting and
-- xterm.
--
-- To make the widget work, a call to
--
-- (right|left)_layout:add(orgmode.widget)
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
-- Awesome wm window management library
local awful = require("awful")

-- Grab the environment
local io = io
local os = os

-- Awesome wm specific environment
local timer = timer
local mouse = mouse
awful.rules = awful.rules


-- Module to read standard emacs org-mode files using emacs as a processor
module("orgmode")

-- Produce the current todo strings and count the number
get_todos = function ()
   local todos
   local number_of_todos = 0

   -- The query to access
   local emacs_query = '(org-agenda nil "t")'
   local todos = request(emacs_query)

   -- Remove the UI junk (2 leading lines)
   local todo_index = todos:find("\n", 0)
   todo_index = todos:find("\n", todo_index + 1)
   todos = todos:sub(todo_index)
   -- Remove the spaces before each individual item
   todos = todos:gsub("\n%s+", "\n")
   -- Remove the trailing and leading newline
   todos = todos:gsub("^%s", ""):gsub("%s$", "")

   -- Now count the newlines (= number of todos)
   local current = -1
   repeat
      current = todos:find("\n", current + 1)
      number_of_todos = number_of_todos + 1
   until current == nil

   return todos, number_of_todos

end

-- A standardized org-agenda querying function that uses emacs to read the DB
request = function (query)
   -- The settings emacs needs to do any queries
   local org_settings =
      "(setq org-agenda-files '\"'\"'" ..
      '("~/Documents/notes"))' ..
      '(setq org-default-notes-file "~/Documents/notes/life.org")'
   -- The functions that output the created buffer to STDOUT
   local emacs_output =
      '(find-file "/tmp/emacs_script_output")' ..
      '(insert-buffer "*Org Agenda*")' ..
      '(save-buffer))'

   -- Build the full query from the set standard blocks and the
   -- called query
   local full_query = org_settings .. query .. emacs_output
   -- Start the process
   local process =
      io.popen("emacs --batch --eval='(progn " .. full_query .. ")'")

   -- Since emacs uses buffered output, we cannot read it from stdout.
   -- Instead, we instruct emacs to save it to a file and keep polling that
   -- until we can read it.
   local output_file = nil
   repeat
      output_file = io.open("/tmp/emacs_script_output", "r")
   until output_file ~= nil

   -- Now we can read the output, however, the file may not be full at the
   -- point we reach it. So, keep reading until we have a string
   local output = ""
   while output == "" do
      output = output .. output_file:read("*all")
   end

   -- We now finally have the output string. Close the process and file and
   -- delete that file
   process:close()
   output_file:close()
   os.remove("/tmp/emacs_script_output")

   return output

end

-- Create the widget
widget = wibox.widget.textbox()
widget.todos = ""
widget:set_text(" 0 ")

-- The function that alters the values to what they should be
function widget:update ()
   todos, number = get_todos()
   self:set_text(" " .. number .. " ")
   self.todos = todos

end

-- Left mouse button should spawn an emacs window to alter todos
widget:buttons(awful.util.table.join(
                  awful.button({ }, 1,
                     function ()
                        -- Since we cannot use any asynchronous calls (damnit),
                        -- use awesome-client to center and resize the prompt
                        -- window
                        local script =
                           -- Wait just a bit so it has time to spawn
                           "sleep 0.2 && " ..
                           "echo \"local awful = require 'awful';" ..

                           -- Get the client
                           "      local emacs_client =" ..
                           "          awful.client.getmaster(mouse.screen);" ..

                           -- Set it to float
                           "      awful.client.floating.set(" ..
                           "          emacs_client, true" ..
                           "      )" ..
                           -- Center it
                           "      emacs_client:geometry({ x = 448," ..
                           "                              y = 166," ..
                           "                              width = 1024," ..
                           "                              height = 768 })" ..
                           -- Raise it
                           "      emacs_client:raise()" ..
                           -- And keep it on top
                           "      emacs_client.ontop = true\" | awesome-client"

                        -- Spawn and center an emacs client using previous code
                        local elisp = '"(progn (org-agenda 14 \\"n\\")' ..
                                      ' (delete-other-windows)' ..
                                      ' (org-agenda-fortnight-view))"'

                        awful.util.spawn("xterm -e 'emacsclient -t --eval " ..
                                          elisp .. "'")
                        awful.util.spawn_with_shell(script)

                     end
                  )
))
-- Hovering over the widget should show the current todos
widget:connect_signal("mouse::enter",
                      function ()
                         options = { text = "Todo:\n" .. widget.todos,
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

-- Update the widget every 30 seconds
agenda_timer = timer({ timeout = 30 })
agenda_timer:connect_signal("timeout",
                            function ()
                               widget:update()
                            end
)
agenda_timer:start()
widget:update()
