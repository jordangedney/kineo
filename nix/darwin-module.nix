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
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

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
  };
}
