require("sensors")

chips = sensors_init()
relevant = {}

--for key, chip in pairs(chips) do
   for key, feature in pairs(chips[1].features) do
      for key, reading in pairs(feature.readings) do
         if (string.find(reading.name, "input")) then
            print(reading.name .. "\t" .. reading.value)
         end
      end
   end
--end

sensors_cleanup()
