-- Standard awesome library
require("awful.remote")
local gears = require("gears")
local awful = require("awful")
local common = require("awful.widget.common")
awful.rules = require("awful.rules")
require("awful.autofocus")
-- Widget and layout li4brary
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")

-- Custom modules
-- Volume handler module
require("modules/volume")
-- Sensors module
require("sensors")

-- Default naughty settings
naughty.config.defaults.fg = '#f0f0f0'

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = err })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("/home/tlater/.config/awesome/themes/Cyan/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "xterm"
editor = os.getenv("EDITOR") or "emacsclient -t"
editor_cmd = terminal .. " -e " .. editor
-- Default modkey
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
layouts =
{
    awful.layout.suit.tile,
    awful.layout.suit.floating,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier
}
-- }}}

-- {{{ Wallpaper
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

--mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
--                                    { "open terminal", terminal }
--                                  }
--                        })

--mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
--                                     menu = mymainmenu })

-- Menubar configuration
function recursive_print(tab)
   local test = ""
   for i, l in pairs(tab) do
      if type(l) ~= "table" then
         test = test .. "|" .. l
      else
         test = test .. "|" .. recursive_print(l)
      end
   end
   return test
end

--menubar.menu_gen.all_menu_dirs = { "/home/tlater/.local/share/applications" }

menubar.utils.terminal = terminal
menubar.show_categories = false
-- }}}

-- {{{ Wibox
-- Create a textclock widget
mytextclock = awful.widget.textclock()
-- Custom widgets
--require("modules/orgmode")
require("modules/hwmon")
require("modules/battery")

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s,
                                        awful.widget.taglist.filter.all,
                                        mytaglist.buttons,
                                        nil,
                                        nil,
                                        wibox.layout.fixed.horizontal())
--                                        wibox.layout.fixed.vertical())

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", screen = s })

    -- Widgets that are aligned to the left
    local left_layout = wibox.layout.fixed.horizontal()
--    left_layout:add(mylauncher)
    left_layout:add(mytaglist[s])
    left_layout:add(mypromptbox[s])

    -- Widgets that are aligned to the right
    local right_layout = wibox.layout.fixed.horizontal()
    if s == 1 then right_layout:add(wibox.widget.systray()) end
    right_layout:add(mytextclock)
    right_layout:add(hwmon.widget)
    --right_layout:add(orgmode.widget)
    right_layout:add(battery.widget)
    right_layout:add(mylayoutbox[s])

    -- Now bring it all together (with the tasklist in the middle)
    local layout = wibox.layout.align.horizontal()
    layout:set_left(left_layout)
    layout:set_middle(mytasklist[s])
    layout:set_right(right_layout)

    mywibox[s]:set_widget(layout)
end
-- }}}

-- Keybindings
-- Mouse buttons
root.buttons(awful.util.table.join(
             awful.button({}, 4, awful.tag.viewnext),
             awful.button({}, 5, awful.tag.viewprev)
))

-- Keyboard buttons
globalkeys = awful.util.table.join(
   awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
   awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
   awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

   awful.key({ modkey,           }, "j",
      function ()
         awful.client.focus.byidx( 1)
         if client.focus then client.focus:raise() end
   end),
   awful.key({ modkey,           }, "k",
      function ()
         awful.client.focus.byidx(-1)
         if client.focus then client.focus:raise() end
   end),

   -- Media keys
   awful.key({}, "XF86AudioLowerVolume", function ()
         volume.relative_change(-5)
   end),
   awful.key({}, "XF86AudioRaiseVolume", function ()
         volume.relative_change(5)
   end),
   awful.key({}, "XF86AudioMute", function ()
         volume.toggle_mute()
   end),
   awful.key({}, "XF86KbdBrightnessDown", function ()
         awful.util.spawn("sudo keybacklight -")
   end),
   awful.key({}, "XF86KbdBrightnessUp", function ()
         awful.util.spawn("sudo keybacklight +")
   end),
   awful.key({}, "XF86MonBrightnessDown", function ()
         awful.util.spawn_with_shell("xbacklight -5%")
   end),
   awful.key({}, "XF86MonBrightnessUp", function ()
         awful.util.spawn_with_shell("xbacklight +5%")
   end),

   -- Layout manipulation
   awful.key({ modkey, "Shift"   }, "j", function ()
         awful.client.swap.byidx(1)
   end),
   awful.key({ modkey, "Shift"   }, "k", function ()
         awful.client.swap.byidx(-1)
   end),
   awful.key({ modkey, "Control" }, "j", function ()
         awful.screen.focus_relative(1)
   end),
   awful.key({ modkey, "Control" }, "k", function ()
         awful.screen.focus_relative(-1)
   end),

   awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
   awful.key({ modkey,           }, "Tab",
      function ()
         awful.client.focus.history.previous()
         if client.focus then
            client.focus:raise()
         end
   end),

   awful.key({ modkey,           }, "l",     function ()
         awful.tag.incmwfact(0.05)
   end),
   awful.key({ modkey,           }, "h",     function ()
         awful.tag.incmwfact(-0.05)
   end),
   awful.key({ modkey, "Shift"   }, "h",     function ()
         awful.tag.incnmaster(1)
   end),
   awful.key({ modkey, "Shift"   }, "l",     function ()
         awful.tag.incnmaster(-1)
   end),
   awful.key({ modkey, "Control" }, "h",     function ()
         awful.tag.incncol(1)
   end),
   awful.key({ modkey, "Control" }, "l",     function ()
         awful.tag.incncol(-1)
   end),
   awful.key({ modkey,           }, "space", function ()
         awful.layout.inc(layouts, 1)
   end),
   awful.key({ modkey, "Shift"   }, "space", function ()
         awful.layout.inc(layouts, -1)
   end),

   awful.key({ modkey, "Control" }, "n", awful.client.restore),

   -- Application bindings
   awful.key({ modkey,          }, "Return", function ()
         awful.util.spawn(terminal)
   end),
   awful.key({ modkey, "Control" }, "r", awesome.restart),
   awful.key({ modkey, "Shift"   }, "q", awesome.quit),

   -- Prompts
   awful.key({ modkey },            "r",     function ()
         mypromptbox[mouse.screen]:run()
   end),

   awful.key({ modkey }, "g",
      function()
         local function google (term)

            local replacements = {
               [" "] = "+",
               ["&"] = "%26",
               ["#"] = "%23",
               ["+"] = "%2B",
               ["'"] = "%27",
               ['"'] = "%22",
               ['\\'] = "%5c"
            }

            term = term:gsub(".", replacements)
            search_string =
               "http://www.google.com/search?q=" .. term

            awful.util.spawn("chromium --windows-8-search " .. search_string)
         end

         awful.prompt.run({ prompt = "Google: " },
            mypromptbox[mouse.screen].widget,
            google,
            nil
         )
   end),

   awful.key({ modkey }, "x",
      function ()
         awful.prompt.run({ prompt = "Run Lua code: " },
            mypromptbox[mouse.screen].widget,
            awful.util.eval, nil,
            awful.util.getdir("cache") .. "/history_eval")
   end),

   awful.key({ modkey }, "p", function() menubar.show() end),

   awful.key({ modkey }, "s", function()
         awful.util.spawn('/home/tlater/bin/scrotMenu')
   end)
)

-- Client manipulation
clientkeys = awful.util.table.join(
   awful.key({ modkey,           }, "f",      function (c)
         c.fullscreen = not c.fullscreen
   end),
   awful.key({ modkey, "Shift"   }, "c",      function (c)
         c:kill()
   end),
   awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle),

   awful.key({ modkey, "Control" }, "Return", function (c)
         c:swap(awful.client.getmaster())
   end),
   awful.key({ modkey,           }, "o",      awful.client.movetoscreen),
   awful.key({ modkey,           }, "t",      function (c)
         c.ontop = not c.ontop
   end),
   awful.key({ modkey,           }, "n",
      function (c)
         -- The client currently has the input focus, so it cannot be
         -- minimized, since minimized clients can't have the focus.
         c.minimized = true
   end),
   awful.key({ modkey,           }, "m",
      function (c)
         c.maximized_horizontal = not c.maximized_horizontal
         c.maximized_vertical   = not c.maximized_vertical
   end)
)

-- Bind all key numbers to tags.
for i = 1, 9 do
   globalkeys = awful.util.table.join(globalkeys,

      -- View tag only.
      awful.key({ modkey }, "#" .. i + 9,
         function ()
            local screen = mouse.screen
            local tag = awful.tag.gettags(screen)[i]
            if tag then
               awful.tag.viewonly(tag)
            end
      end),

      -- Toggle tag.
      awful.key({ modkey, "Control" }, "#" .. i + 9,
         function ()
            local screen = mouse.screen
            local tag = awful.tag.gettags(screen)[i]
            if tag then
               awful.tag.viewtoggle(tag)
            end
      end),

      -- Move client to tag.
      awful.key({ modkey, "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = awful.tag.gettags(client.focus.screen)[i]
               if tag then
                  awful.client.movetotag(tag)
               end
            end
      end),

      -- Toggle tag.
      awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
         function ()
            if client.focus then
               local tag = awful.tag.gettags(client.focus.screen)[i]
               if tag then
                  awful.client.toggletag(tag)
               end
            end
   end))
end

clientbuttons = awful.util.table.join(
   awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
   awful.button({ modkey }, 1, awful.mouse.client.move),
   awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,

                     -- THIS IS VERY IMPORTANT AS IT GETS RID OF THE SPACES
                     -- BETWEEN WINDOWS!
                     size_hints_honor = false },

      --callback =
      --   function(c)
      --      c:connect_signal("focus",
      --                       function(c)
      --                          if awful.client.focus.filter(c) then
      --                             awful.util.spawn(
      --                                'roccatryosmkcontrol -a 1')
      --                          end
      --                       end
      --      )
      --   end
    },
    { rule = { class = "gimp" },
      properties = { floating = true }
    },
    { rule = { class = "XTerm" },
      properties = { opacity = 0.7 },
      --callback =
      --   function(c)
      --      awful.util.spawn('roccatryosmkcontrol -a 2')
      --      c:disconnect_signal("focus", function(c) end)
      --      c:connect_signal("focus",
      --                       function(c)
      --                          if awful.client.focus.filter(c) then
      --                             awful.util.spawn('roccatryosmkcontrol -a 2')
      --                          end
      --                       end
      --      )
      --      c:connect_signal("unfocus",
      --                       function(c)
      --                          if awful.client.focus.filter(c) then
      --                             awful.util.spawn('roccatryosmkcontrol -a 1')
      --                          end
      --                       end
      --      )
      --   end
    },
    { rule = { name = "Chrome editor" },
      properties = { floating = true },
      callback = function(c)
         awful.placement.centered(c,nil)
      end
    },
    { rule = { name = "finch" },
      properties = { tag = tags[1][9] }
    },
    { rule = { role = "bubble" },
      properties = { floating = true, ontop = true }
    }
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = c
        end
    end)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        -- awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    local titlebars_enabled = false
    if titlebars_enabled and (c.type == "normal" or c.type == "dialog") then
        -- buttons for the titlebar
        local buttons = awful.util.table.join(
                awful.button({ }, 1, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.move(c)
                end),
                awful.button({ }, 3, function()
                    client.focus = c
                    c:raise()
                    awful.mouse.client.resize(c)
                end)
               )

        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))
        left_layout:buttons(buttons)

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local middle_layout = wibox.layout.flex.horizontal()
        local title = awful.titlebar.widget.titlewidget(c)
        title:set_align("center")
        middle_layout:add(title)
        middle_layout:buttons(buttons)

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(middle_layout)

        awful.titlebar(c):set_widget(layout)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- Autostart
function start (application)
   local pgrep = awful.util.pread("pgrep " .. application)
   if pgrep == "" then
      awful.util.spawn(application)
   end
end

--awful.util.spawn_with_shell("xterm -T finch -e /usr/bin/finch")
start("thunderbird")
--start("apulse32 skype")
--awful.util.spawn_with_shell("unclutter -root -idle 2 &")
--awful.util.spawn_with_shell("lua Projects/lua/stable/tests.lua")
--awful.util.spawn_with_shell("xcompmgr")
start("systemctl --user restart text-aid-too.service")
