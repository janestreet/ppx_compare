#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"ppx_compare"
  [ oasis_lib "ppx_compare"
  ; oasis_lib "ppx_compare_expander"
  ; file "META" ~section:"lib"
  ]
