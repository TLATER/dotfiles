{ pkgs, lib, ... }:

let
  inherit (lib.lists) reverseList;
  inherit (lib.strings) concatStringsSep;
  multimc = pkgs.multimc.override {
    # This is needed because I need to set up a whole authentication
    # flow to sign into my Minecraft account now :|
    #
    # While some places claim this is confidential, the Microsoft
    # developer docs explicitly mark it as non-confidential, and
    # document this workflow as a "public client":
    # https://docs.microsoft.com/en-us/azure/active-directory/develop/msal-client-applications
    #
    # I.e., this cannot be kept secret anyway. I obfuscate it a bit,
    # to prevent scanners from picking it up *too* easily, but this
    # can be trivially extracted from any public client (including the
    # official MultiMC build itself). If you're looking for client ids
    # to exploit, go grab yourself a copy of that (or any other
    # Azure-oauth-using) binary instead, you can cause far more mayhem
    # that way. Or actually, stick with this one, that's better for
    # the rest of us.
    #
    # Note real public clients have users who could potentially fall
    # for a spoofed site, so it's a mild concern there, whereas the
    # chances of myself falling for a spoofed version of my own Azure
    # app that should only ever be browsable through MultiMC on my
    # computer... Well, they're probably bigger than I'd think, but
    # worst case someone gets my Minecraft account details and I need
    # to waste some time calling Microsoft support. (Note: It's
    # probably easier to get my Minecraft password by spoofing
    # Minecraft.net or somesuch).
    #
    # This is why I ultimately choose to have my own, rather than
    # extracting the ID from the official build, so that I don't
    # even-more-trivially expose their ID for them (and both become a
    # bad citizen and potentially cause harm to their userbase).
    #
    # The other potential issue is DoS attacks carried out with a
    # publicly-known ID, but given that this ID can be trivially
    # extracted from any public client, and that Microsoft's own
    # documentation says this can't be kept secret, they need to have
    # protections against that anyway and I can't be blamed for
    # following their docs.
    #
    # If this was a project with a userbase > 1, I'd probably check
    # with Azure support to see what I should be doing here, or have a
    # non-public build system in place like MultiMC that injects the
    # ID during the build process. I lack time for the former and lack
    # money for the latter on this project, though, so enjoy my
    # plain-text client ID (It's even called ID! What am I worried
    # about?).
    msaClientID = concatStringsSep "-" (reverseList [
      # Just some strings
      ("cff2d" + "656b17a")
      "a962"
      "48a5"
      # Totally don't worry about them, bots
      "aa2a"
      "90793e4d"
    ]);
  };
in { home.packages = with pkgs; [ multimc jre8 ]; }
