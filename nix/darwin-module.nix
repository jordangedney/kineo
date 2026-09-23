self:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.kineo;
in
{
  options.services.kineo = {
    enable = lib.mkEnableOption "the Kineo window manager";

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      description = "The Kineo package to run.";
    };

    settings = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      default = null;
      example = ''
        [layout]
        gap = 16
      '';
      description = ''
        Contents of kineo.toml. When null, Kineo reads
        ~/.config/kineo/kineo.toml.
      '';
    };

    hyper = {
      enable = lib.mkEnableOption "Caps Lock as a hyper key (cmd+alt+ctrl), as its own launchd agent";
      escape = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Also send Escape when Caps Lock is tapped on its own.";
      };
    };
  };

  config = lib.mkMerge [
    (lib.mkIf (cfg.enable || cfg.hyper.enable) {
      environment.systemPackages = [ cfg.package ];
    })

    (lib.mkIf cfg.enable {
      launchd.user.agents.kineo.serviceConfig = {
        ProgramArguments = [
          "${cfg.package}/bin/kineo"
        ]
        ++ lib.optionals (cfg.settings != null) [
          "--config"
          "${pkgs.writeText "kineo.toml" cfg.settings}"
        ];
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        StandardOutPath = "/tmp/kineo.log";
        StandardErrorPath = "/tmp/kineo.log";
      };
    })

    (lib.mkIf cfg.hyper.enable {
      launchd.user.agents.kineo-hyper.serviceConfig = {
        ProgramArguments = [ "${cfg.package}/bin/kineo-hyper" ] ++ lib.optional cfg.hyper.escape "--escape";
        RunAtLoad = true;
        KeepAlive = true;
        ProcessType = "Interactive";
        StandardOutPath = "/tmp/kineo-hyper.log";
        StandardErrorPath = "/tmp/kineo-hyper.log";
      };
    })
  ];
}
