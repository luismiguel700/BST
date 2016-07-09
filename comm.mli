open Types;;

type comm =
| Quit
| Extract of ty * ty
| Join of (int list) * ty * map * int ;;
