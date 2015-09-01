-- Mouse buttons
root.buttons(awful.util.table.join(
                awful.button({}, 3, function ()
                      mymainmenu:toggle()
                end),
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
   awful.key({}, "XF86HomePage", function ()
         awful.util.spawn("chromium")
   end),
   awful.key({}, "XF86Explorer", function ()
         awful.util.spawn_with_shell("sudo shutdown -h now")
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

   -- Prompts
   awful.key({ modkey },            "r",     function ()
         Mypromptbox[Mouse.Screen]:Run()
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

            repNotify:notify("Google: " .. term)
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
