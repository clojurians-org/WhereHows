with import <nixpkgs> {} ;
let
  inline_java_git = fetchFromGitHub {
      owner = "tweag" ;
      repo = "inline-java" ;
      rev = "a897d32df99e4ed19314d2a7e245785152e9099d" ;
      sha256 = "00pk19j9g0mm9sknj3aklz01zv1dy234s3vnzg6daq1dmwd4hb68" ;
  } ; 
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      jni = addBuildDepend (self.callCabal2nix "jni"  (inline_java_git + /jni) {}) pkgs.jdk ;
      inline-java = addBuildDepend (self.callCabal2nix "inline-java" inline_java_git {}) pkgs.jdk ;
    } ;
  };

in
mkShell {
  buildInputs = [
    pkgs.jdk
    (haskellPackages.ghcWithPackages ( p: 
      [ p.bytestring p.string-conversions
        p.exceptions 
        p.inline-java
      ]
    ))
  ];
}
