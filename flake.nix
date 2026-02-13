{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/release-25.11";

  outputs =
    { self, nixpkgs }:
    let
      systems = nixpkgs.lib.systems.flakeExposed;
      forAllSystems = nixpkgs.lib.genAttrs systems;
      define = f: forAllSystems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = define (pkgs: {
        backend = import ./backend { inherit pkgs; };
        frontend = import ./frontend {
          inherit pkgs;
          rev = if self ? rev then builtins.substring 0 6 self.rev else "dirty tree";
        };
      });

      devShells = define (pkgs: {
        default = import ./shell.nix { inherit pkgs; };
      });

      nixosModules.cabinet =
        { config, pkgs, ... }:
        {
          networking.firewall.allowedTCPPorts = [ 80 ];

          systemd.services.cabinet-frontend = {
            wantedBy = [ "multi-user.target" ];
            after = [
              "network.target"
              "cabinet-backend.target"
            ];
            description = "Cabinet's frontend component.";
            serviceConfig = {
              ExecStart = "${pkgs.nodejs}/bin/node ${self.packages.x86_64-linux.frontend}/lib/cabinet-frontend";
            };

            environment.PORT = "6445";
          };

          systemd.services.cabinet-backend = {
            wantedBy = [ "multi-user.target" ];
            after = [ "network.target" ];
            description = "file storage web server";
            serviceConfig = {
              ExecStart = "${self.packages.x86_64-linux.backend}/bin/cabinet-srv";
            };

            environment.CABINET_BACKEND_PORT = "6446";
          };
        };
    };
}
