{ mkDerivation, base, containers, mtl, stdenv, transformers, xhb
, xhb-monad
}:
mkDerivation {
  pname = "xhb-event-queue";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base containers mtl transformers xhb xhb-monad
  ];
  license = stdenv.lib.licenses.mit;
}
