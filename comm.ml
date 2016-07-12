open Types;;

type comm =
| Quit
| Extract of ty * ty
| OKextract of ty * ty
| KOextract of ty * ty
| Join of (int list) * ty * map * int 
| OKjoin of (int list) * ty * map * int * ty ;;
