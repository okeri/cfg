cutils = require ("common-utils")
log = Log.open_topic ("s-boot-volume")

PINS = {
  ["alsa_input.platform-sound.HiFi__Mic__source"] =
      { direction = "Input",  volume = 0.015625 },  -- 25%
  ["alsa_output.platform-sound.HiFi__Speaker__sink"] =
      { direction = "Output", volume = 0.008 },     -- 20%
}
done = {}

SimpleEventHook {
  name = "boot-volume-pins",
  interests = {
    EventInterest {
      Constraint { "event.type", "=", "node-added" },
      Constraint { "node.name", "matches", "alsa_*.platform-sound.HiFi__*" },
    },
  },
  execute = function (event)
    local node = event:get_subject ()
    local name = node.properties ["node.name"]
    local pin = PINS [name]
    if not pin or done [name] then return end
    local source = event:get_source ()
    local dev_id = node.properties ["device.id"]
    local cpd = tonumber (node.properties ["card.profile.device"])
    local device_om = source:call ("get-object-manager", "device")
    for device in device_om:iterate () do
      if tostring (device ["bound-id"]) == tostring (dev_id) then
        for p in device:iterate_params ("Route") do
          local route = cutils.parseParam (p, "Route")
          if route and route.direction == pin.direction
                   and route.device == cpd then
            device:set_param ("Route", Pod.Object {
              "Spa:Pod:Object:Param:Route", "Route",
              index = route.index,
              device = route.device,
              props = Pod.Object {
                "Spa:Pod:Object:Param:Props", "Route",
                volume = pin.volume,
              },
              save = false,
            })
            log:info (device, name .. " volume pinned for this session")
            done [name] = true
          end
        end
      end
    end
  end,
}:register ()
