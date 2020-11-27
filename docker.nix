{ obelisk ? import ./.obelisk/impl {
  system = builtins.currentSystem;
  iosSdkVersion = "10.2";
} }:

with obelisk.nixpkgs;

let
  project = import ./. { inherit obelisk; };
  exe = project.exe;

in dockerTools.buildLayeredImage {
  name = "asmJsonCpp-ui";
  tag = "latest";
  config = {
    WorkingDir = "${exe}";
    Cmd = [ "${exe}/backend" "-p 8080" ];
    ExposedPorts = { "8080" = { }; };
  };
}
