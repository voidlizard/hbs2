self: {
  config,
  lib,
  pkgs,
  ... }: let
    inherit (pkgs.stdenv.hostPlatform) system;

    package = self.packages.${system}.default;

    cfg = config.services.hbs2;
    hbs2 = cfg.package;
    hbs2-peer = "${hbs2}/bin/hbs2-peer";
    hbs2-git-dashboard = "${hbs2}/bin/hbs2-git-dashboard";
in {
  options = {
    services.hbs2 = {
      enable = lib.mkEnableOption "hbs2-peer daemon";
      package = lib.mkOption {
        type = lib.types.package;
        description = "Package with all HBS2 basic binaries";
        default = package;
      };
      git-dashboard.enable = lib.mkEnableOption "hbs2-git-dashboard daemon";
    };
  };
  config = lib.mkIf cfg.enable {

    home.packages = [ cfg.package ];

    systemd.user.services.hbs2-peer = {
      Unit = {
        Description = "HBS2 peer daemon";
        After = [ "network.target" ];
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecPreStart = "${hbs2-peer} init";
        ExecStart = "${hbs2-peer} run";
        Restart = "always";
        RuntimeMaxSec = "1d";
      };
    };

    systemd.user.services.hbs2-git-dashboard = lib.mkIf cfg.git-dashboard.enable {
      Unit = {
        Description = "HBS2 git dashboard daemon";
        After = [ "hbs2-peer.service" "network.target" ];
        BindsTo = [ "hbs2-peer.service" ];
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
      Service = {
        ExecPreStart = "sleep 5";
        ExecStart = "${hbs2-git-dashboard} web";
        Environment = [ "PATH=${hbs2}/bin:${pkgs.git}/bin:$PATH" ]; # detectRPC require hbs2-peer to be in scope
        Restart = "always";
        RuntimeMaxSec = "1d";
      };
    };
  };
}
