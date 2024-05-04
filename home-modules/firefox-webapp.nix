{ config, lib, ... }:
let
  inherit (builtins) getAttr stringLength substring;
  inherit (lib) mkOption mkEnableOption;
  inherit (lib.attrsets)
    mapAttrs
    mapAttrs'
    nameValuePair
    filterAttrs
    ;
  inherit (lib.strings) concatStringsSep toUpper;

  make-app-profiles =
    cfg:
    mapAttrs' (
      name: cfg:
      nameValuePair "home-manager-webapp-${name}" {
        inherit (cfg) id;

        userChrome = ''
          @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");

          browser {
            margin-right: 0px; margin-bottom: 0px;
          }

          #TabsToolbar {
            visibility: collapse !important;
          }

          #nav-bar {
            margin-top: 0;
            margin-bottom: -42px;
            z-index: -100;
          }

          #main-window[windowtype="navigator:browser"] {
            background-color: transparent !important;
          }

          .tab-background[selected="true"] {
            background: ${cfg.backgroundColor} !important;
          }
        '';

        settings = cfg.extraSettings // {
          "browser.sessionstore.resume_session_once" = false;
          "browser.sessionstore.resume_from_crash" = false;
          "browser.cache.disk.enable" = false;
          "browser.cache.disk.capacity" = 0;
          "browser.cache.disk.filesystem_reported" = 1;
          "browser.cache.disk.smart_size.enabled" = false;
          "browser.cache.disk.smart_size.first_run" = false;
          "browser.cache.disk.smart_size.use_old_max" = false;
          "browser.ctrlTab.previews" = true;
          "browser.tabs.warnOnClose" = false;
          "plugin.state.flash" = 2;
          "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          "browser.tabs.drawInTitlebar" = false;
          "browser.tabs.inTitlebar" = 0;
          "browser.contentblocking.category" = "strict";
          "network.cookie.lifetimePolicy" = 0;
          "layout.css.prefers-color-scheme.content-override" = getAttr cfg.theme {
            dark = 0;
            light = 1;
            system = 2;
          };
        };
      }
    ) cfg;
in
{
  options.programs.firefox.webapps = mkOption {
    default = { };

    type =
      with lib.types;
      attrsOf (submodule {
        options = {
          enable = mkEnableOption "webapp";

          ####################
          # Firefox settings #
          ####################
          url = mkOption {
            type = str;
            description = "The URL of the webapp to launch.";
          };

          id = mkOption {
            type = int;
            description = "The Firefox profile ID to set.";
          };

          extraArgs = mkOption {
            type = listOf str;
            default = [ ];
            description = "Extra args to launch Firefox with.";
          };

          extraSettings = mkOption {
            type = attrsOf (either bool (either int str));
            default = { };
            description = "Additional Firefox profile settings.";
          };

          backgroundColor = mkOption {
            type = str;
            default = "rgba(0, 0, 0, 0)";
            description = "The background color to use for loading pages.";
          };

          theme = mkOption {
            type = enum [
              "dark"
              "light"
              "system"
            ];
            default = "system";
            description = "The application CSS theme to use, if supported.";
          };

          #########################
          # Desktop file settings #
          #########################

          # Copied from xdg.desktopEntries, with slight modification for default settings
          name = mkOption {
            type = nullOr str;
            default = null;
            description = "Specific name of the application. Defaults to the capitalized attribute name.";
          };

          mimeType = mkOption {
            description = "The MIME type(s) supported by this application.";
            type = nullOr (listOf str);
            default = [
              "text/html"
              "text/xml"
              "application/xhtml_xml"
            ];
          };

          # Copied verbatim from xdg.desktopEntries.
          genericName = mkOption {
            type = nullOr str;
            default = null;
            description = "Generic name of the application.";
          };

          comment = mkOption {
            type = nullOr str;
            default = null;
            description = "Tooltip for the entry.";
          };

          categories = mkOption {
            type = nullOr (listOf str);
            default = null;
            description = "Categories in which the entry should be shown in a menu.";
          };

          icon = mkOption {
            type = nullOr (either str path);
            default = null;
            description = "Icon to display in file manager, menus, etc.";
          };

          prefersNonDefaultGPU = mkOption {
            type = nullOr bool;
            default = null;
            description = ''
              If true, the application prefers to be run on a more
              powerful discrete GPU if available.
            '';
          };
        };
      });

    description = "Websites to create special site-specific Firefox instances for.";
  };

  config = {
    programs.firefox.profiles = make-app-profiles config.programs.firefox.webapps;

    xdg.desktopEntries = mapAttrs (name: cfg: {
      inherit (cfg)
        genericName
        comment
        categories
        icon
        mimeType
        prefersNonDefaultGPU
        ;

      name =
        if cfg.name == null then
          (toUpper (substring 0 1 name)) + (substring 1 (stringLength name) name)
        else
          cfg.name;

      startupNotify = true;
      terminal = false;
      type = "Application";

      exec = concatStringsSep " " (
        [
          "${config.programs.firefox.package}/bin/firefox"
          "--class"
          "WebApp-${name}"
          "-P"
          "${config.programs.firefox.profiles."home-manager-webapp-${name}".path}"
          "--no-remote"
        ]
        ++ cfg.extraArgs
        ++ [ "${cfg.url}" ]
      );

      settings = {
        X-MultipleArgs = "false"; # Consider enabling, don't know what this does
        StartupWMClass = "WebApp-${name}";
      };
    }) (filterAttrs (_: webapp: webapp.enable) config.programs.firefox.webapps);
  };
}
