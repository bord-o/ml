structure Util =
struct

  infix 1 |>
  fun x |> f = f x (* Left pipe *)

  infixr 1 <|
  fun f <| y = f y (* Right pipe *)

  fun id x = x

  structure Dict :>
  sig
    type ('a, 'b) dict
    val empty : ('a -> 'a -> bool) -> ('a, 'b) dict
    val insert : 'a -> 'b -> ('a, 'b) dict -> ('a, 'b) dict
    val find : 'a -> ('a, 'b) dict -> 'b option
    val find_exn : 'a -> ('a, 'b) dict -> 'b
    (*format keys and values given a formatter for keys, and formatter for values*)
    val pp_dict : ('a -> string) -> ('b -> string) -> ('a, 'b) dict -> string
  end
   
  =
  struct
    type ('a, 'b) dict = ('a -> 'a -> bool) * ('a * 'b) list
    fun eqf (a, _) = a
    fun data (_, b) = b
    fun empty equality = (equality, [])
    fun insert (k: 'a) (v: 'b) (d: ('a, 'b) dict) =
      let
        val equality = eqf d
        val data' = data d
      in
        case data' of
          [] => (equality, [(k, v)])
        | (k', v') :: xs =>
            if equality k k' then (equality, (k', v) :: xs)
            else (equality, (k', v') :: (data (insert k v (equality, xs))))
      end
    fun find (k: 'a) (d: ('a, 'b) dict) =
      let
        val equality = eqf d
        val data' = data d
      in
        case data' of
          [] => NONE
        | (k', v') :: xs =>
            if equality k k' then SOME v' else find k (equality, xs)
      end
    exception NotFound
    fun find_exn (k: 'a) (d: ('a, 'b) dict) =
      case find k d of
        SOME v => v
      | NONE => raise NotFound

    fun pp_dict kf vf d = 
      let 
        val data' = data d
      in
        foldl (fn ((key, value), state) => state ^ "KEY: " ^ (kf key) ^  ", VALUE: " ^ (vf value) ^ "\n") "" data'
      end


      
    fun streq (l: string) (r: string) : bool = l = r
    fun test1 () =
      let
        val d =
          empty streq |> insert "K1" "V1" |> insert "K3" "V2"
          |> insert "K3" "V3"
        val (_, items) = d
      in
        print ((Int.toString o List.length) items)
      end
    fun test2 () =
      let
        val d = empty streq |> insert "K1" "YES" |> insert "K2" "NO"
        val ans = find_exn "K2" d
      in
        print ans
      end
  end

end
