val test = let val y = 9 in y end
val test2 = 
    let
        val x = 1
        val y = 2
        val z = 3
    in
        let 
            val residual = x * y * z + 23
        in
            let
                val rec testfun = fn x =>  x / 2
            in
                testfun residual
            end
        end

    end
