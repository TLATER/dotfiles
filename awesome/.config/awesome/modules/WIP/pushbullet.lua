-- Required modules
local https = require("ssl.https")
local decode = require("json.decode")
local ltn12 = require("ltn12")

-- Debug dependencies
local print = print
local write = io.write
local pairs = pairs
local type = type
local tostring = tostring


module("pushbullet")

-- The pusbullet API access token
local ACCESS_TOKEN = ""
-- Whether we want debug output
local DEBUG = true

-- Set the access token (get from https://www.pushbullet.com/account)
set_token = function (token)
   ACCESS_TOKEN = token
end

-- Make a generic API call
api_call = function (headers, url, method, body)
   -- Check if the access token has been set
   if ACCESS_TOKEN == nil then
      print("Please set the variable ACCESS_TOKEN first")
      return
   end

   -- Set the authorization header
   local headers = headers or {}
   headers["authorization"] = "Bearer " .. ACCESS_TOKEN

   -- If we have a body, prepare the headers to send one
   if body ~= nil then
      headers["content-length"] = tostring(body:len())
   end

   -- A table to be filled with the server response
   local response = {}

   -- Make the request
   local success, status, headers, info = https.request{
      url = url or "https://api.pushbullet.com/v2/users/me",
      method = method or "GET",
      headers = headers,
      source = ltn12.source.string(body),
      sink = ltn12.sink.table(response)
   }

   print("test")

   -- If we successfully get a response
   if success ~= nil then
      -- Decode the JSON response
      response = decode(response[1])

      if DEBUG then
         -- DEBUG
         print("\nAPI call returned: " .. status .. " (" .. info .. ")")
         recursive_print(response)
         recursive_print(headers)
      end
   end

   return response
end

-- Creates a new device for this application
register_device = function ()
   local body = "nickname=Awesome%20WM&type=stream"
   api_call(headers, "https://api.pushbullet.com/v2/devices", "POST", body)
end

-- Debug function to recursively print tables
recursive_print = function (input)
   for k, v in pairs(input) do
      write(k .. ": ")
      if type(v) ~= "table" then
         print(v)
      else
         recursive_print(v)
      end
   end
end
