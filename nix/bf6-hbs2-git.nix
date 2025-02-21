{
  lib,
  writeTextFile,
  stdenv,
  suckless-conf,
}:
stdenv.mkDerivation {
  pname = "bf6-git-hbs2";
  version = "1.0";

  dontUnpack = true;
  dontConfigure = true;
  doCheck = false;

  installPhase = let
    name = "git-hbs2";
    withLines = f: s: lib.strings.concatStringsSep "\n" (f (lib.strings.splitString "\n" s));
    p = writeTextFile {
      inherit name;
      text = lib.strings.concatStringsSep "\n" [
        ''#! ${suckless-conf}/bin/bf6 file''
        (withLines (lib.lists.drop 1) (builtins.readFile ../hbs2-git3/bf6/git-hbs2))
      ];
      executable = true;
      destination = "/bin/${name}";
    };
  in ''
    mkdir -p $out/bin
    cp ${p}/bin/${name} $out/bin
    '';



}
