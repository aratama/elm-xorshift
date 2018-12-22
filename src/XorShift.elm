module XorShift exposing (Generator(..), defaultSeed, random, randomIntList, randomList, randomint, seed)

import Bitwise exposing (and, or, shiftLeftBy, shiftRightBy, shiftRightZfBy)


type Generator
    = Generator
        { state0U : Int
        , state0L : Int
        , state1U : Int
        , state1L : Int
        }


defaultSeed : Int
defaultSeed =
    88675123


seed : Int -> Generator
seed s =
    Generator
        { state0U = 123456789
        , state0L = 362436069
        , state1U = 521288629
        , state1L = s
        }


randomIntList : Int -> Generator -> ( List ( Int, Int ), Generator )
randomIntList n gen =
    if n <= 0 then
        ( [], gen )

    else
        let
            ( r, gen_ ) =
                randomint gen

            ( rs, gen__ ) =
                randomIntList (n - 1) gen_
        in
        ( r :: rs, gen__ )


randomList : Int -> Generator -> ( List Float, Generator )
randomList n gen =
    if n <= 0 then
        ( [], gen )

    else
        let
            ( r, gen_ ) =
                random gen

            ( rs, gen__ ) =
                randomList (n - 1) gen_
        in
        ( r :: rs, gen__ )


randomint : Generator -> ( ( Int, Int ), Generator )
randomint (Generator v) =
    let
        -- uint64_t s1 = s[0]
        -- var s1U = this._state0U, s1L = this._state0L;
        s1U =
            v.state0U

        s1L =
            v.state0L

        -- uint64_t s0 = s[1]
        -- var s0U = this._state1U, s0L = this._state1L;
        s0U =
            v.state1U

        s0L =
            v.state1L

        -- result = s0 + s1
        -- var sumL = (s0L >>> 0) + (s1L >>> 0);
        -- resU = (s0U + s1U + (sumL / 2 >>> 31)) >>> 0;
        -- resL = sumL >>> 0;
        sumL =
            shiftRightZfBy 0 s0L + shiftRightZfBy 0 s1L

        resU =
            shiftRightZfBy 0 (s0U + s1U + shiftRightZfBy 31 (sumL // 2))

        resL =
            shiftRightZfBy 0 sumL

        -- s[0] = s0
        --this._state0U = s0U;
        --this._state0L = s0L;
        state0U =
            s0U

        state0L =
            s0L

        -- - t1 = [0, 0]
        --var t1U = 0, t1L = 0;
        t1U =
            0

        t1L =
            0

        -- - t2 = [0, 0]
        -- var t2U = 0, t2L = 0;
        t2U =
            0

        t2L =
            0

        -- s1 ^= s1 << 23;
        -- :: t1 = s1 << 23
        -- var a1 = 23;
        a1 =
            23

        -- var m1 = 0xFFFFFFFF << (32 - a1);
        m1 =
            shiftLeftBy (32 - a1) 0xFFFFFFFF

        -- t1U = (s1U << a1) | ((s1L & m1) >>> (32 - a1));
        t1U_ =
            or (shiftLeftBy a1 s1U) (shiftRightZfBy (32 - a1) (and s1L m1))

        -- t1L = s1L << a1;
        t1L_ =
            shiftLeftBy a1 s1L

        -- :: s1 = s1 ^ t1
        -- s1U = s1U ^ t1U;
        -- s1L = s1L ^ t1L;
        s1U_ =
            Bitwise.xor s1U t1U_

        s1L_ =
            Bitwise.xor s1L t1L_

        -- t1 = ( s1 ^ s0 ^ ( s1 >> 17 ) ^ ( s0 >> 26 ) )
        -- :: t1 = s1 ^ s0
        --t1U = s1U ^ s0U;
        --t1L = s1L ^ s0L;
        t1U__ =
            Bitwise.xor s1U_ s0U

        t1L__ =
            Bitwise.xor s1L_ s0L

        -- :: t2 = s1 >> 18
        -- var a2 = 18;
        -- var m2 = 0xFFFFFFFF >>> (32 - a2);
        -- t2U = s1U >>> a2;
        -- t2L = (s1L >>> a2) | ((s1U & m2) << (32 - a2));
        a2 =
            18

        m2 =
            shiftRightZfBy (32 - a2) 0xFFFFFFFF

        t2U_ =
            shiftRightZfBy a2 s1U_

        t2L_ =
            or (shiftRightZfBy a2 s1L_) (shiftLeftBy (32 - a2) (and s1U_ m2))

        -- :: t1 = t1 ^ t2
        -- t1U = t1U ^ t2U;
        -- t1L = t1L ^ t2L;
        t1U___ =
            Bitwise.xor t1U__ t2U_

        t1L___ =
            Bitwise.xor t1L__ t2L_

        -- :: t2 = s0 >> 5
        -- var a3 = 5;
        -- var m3 = 0xFFFFFFFF >>> (32 - a3);
        -- t2U = s0U >>> a3;
        -- t2L = (s0L >>> a3) | ((s0U & m3) << (32 - a3));
        a3 =
            5

        m3 =
            shiftRightZfBy (32 - a3) 0xFFFFFFFF

        t2U__ =
            shiftRightZfBy a3 s0U

        t2L__ =
            or (shiftRightZfBy a3 s0L) (shiftLeftBy (32 - a3) (and s0U m3))

        -- :: t1 = t1 ^ t2
        -- t1U = t1U ^ t2U;
        -- t1L = t1L ^ t2L;
        t1U____ =
            Bitwise.xor t1U___ t2U__

        t1L____ =
            Bitwise.xor t1L___ t2L__
    in
    -- return result
    -- return [resU, resL];
    ( ( resU, resL )
    , Generator
      -- s[1] = t1
      -- this._state1U = t1U;
      -- this._state1L = t1L;
      <|
        { v
            | state0U = state0U
            , state0L = state0L
            , state1U = t1U____
            , state1L = t1L____
        }
    )



{-
   Returns a random number normalized [0, 1), just like Math.random()
   @return {number}
-}


random : Generator -> ( Float, Generator )
random gen =
    -- var t2 = this.randomint();
    let
        ( ( t2U, t2L ), next ) =
            randomint gen

        -- // Math.pow(2, -32) = 2.3283064365386963e-10
        -- // Math.pow(2, -52) = 2.220446049250313e-16
    in
    -- return t2[0] * 2.3283064365386963e-10 + (t2[1] >>> 12) * 2.220446049250313e-16;
    ( toFloat t2U * 2.3283064365386963e-10 + toFloat (shiftRightZfBy 12 t2L) * 2.220446049250313e-16, next )
