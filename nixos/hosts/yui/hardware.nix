/**
  Additional hardware configuraiton for yui.

  TODO: Upstream this.
*/
{
  # Used for IPMI (remote maintenance thing), but is unsupported
  # by motherboard.
  boot.blacklistedKernelModules = [ "sp5100_tco" ];

  services.udev.rules."98-b550i-suspend.rules" = ''
    # Fix broken suspend on b550i motherboard
    #
    # The rule is a bit overzealous, as it disables wake from *either*
    # NVME drive, but I don't see why anyone would want to wake from
    # NVME drives anyway.
    #
    # At least I *think* that's what the GPP bridge maps to. In
    # either case, this fixes the immediate resume from suspend on
    # my board.
    ACTION=="add", SUBSYSTEM=="pci", ATTR{vendor}=="0x1022", ATTR{device}=="0x1483", ATTR{power/wakeup}="disabled"
  '';
}
