# See how this flake is used in ./usage.sh
{
  description = "hbs2-container";

  inputs = {
    extra-container.url = "github:erikarvstedt/extra-container";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.11";
    hbs2.url = "github:voidlizard/hbs2";
    hbs2.inputs.nixpkgs.follows = "nixpkgs";
  };


  outputs = { extra-container, ... }@inputs:
    extra-container.inputs.flake-utils.lib.eachSystem extra-container.lib.supportedSystems (system: {
      packages.default = extra-container.lib.buildContainers {
        # The system of the container host
        inherit system;

        # Only set this if the `system.stateVersion` of your container
        # host is < 22.05
        # legacyInstallDirs = true;

        # Optional: Set nixpkgs.
        # If unset, the nixpkgs input of extra-container flake is used
        nixpkgs = inputs.nixpkgs;

        # Set this to disable `nix run` support
        # addRunner = false;

        config = {
          containers.hbs2-test = {
            extra.addressPrefix = "10.250.0";

            config = { pkgs, ... }: {
              systemd.services.hello = {
                wantedBy = [ "multi-user.target" ];
                script = ''
                  while true; do
                    echo hello | ${pkgs.netcat}/bin/nc -lN 50
                  done
                '';
              };

              networking.firewall.enable = false;

              environment.systemPackages = with pkgs; [
                inputs.hbs2.packages.${pkgs.system}.default
                screen
                tshark
              ];

              # modules = [
              #   inputs.hbs2.packages.${pkgs.system}.default
              # ];


            };
          };
        };
      };
    });
}
