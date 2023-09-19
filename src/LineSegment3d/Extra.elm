module LineSegment3d.Extra exposing (nearestPoints)

import LineSegment3d exposing (LineSegment3d)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)


type alias Point =
    ( Float, Float, Float )


isInvalid : Point -> Bool
isInvalid ( x, y, z ) =
    isNaN x || isInfinite x || isNaN y || isInfinite y || isNaN z || isInfinite z


add : Point -> Point -> Point
add ( x0, y0, z0 ) ( x1, y1, z1 ) =
    ( x0 + x1, y0 + y1, z0 + z1 )


sub : Point -> Point -> Point
sub ( x0, y0, z0 ) ( x1, y1, z1 ) =
    ( x0 - x1, y0 - y1, z0 - z1 )


scale : Float -> Point -> Point
scale k ( x, y, z ) =
    ( k * x, k * y, k * z )


dot : Point -> Point -> Float
dot ( x0, y0, z0 ) ( x1, y1, z1 ) =
    x0 * x1 + y0 * y1 + z0 * z1


getClampedRoot : Float -> Float -> Float -> Float
getClampedRoot slope h0 h1 =
    if h0 < 0 then
        if h1 > 0 then
            let
                r =
                    -h0 / slope
            in
            if r > 1 then
                0.5

            else
                r

        else
            1

    else
        0


classify : Float -> Float
classify n =
    if n <= 0 then
        -1

    else if n >= 1 then
        1

    else
        0


computeMinimumParameters :
    { edge0 : Float
    , edge1 : Float
    , end00 : Float
    , end01 : Float
    , end10 : Float
    , end11 : Float
    , b : Float
    , c : Float
    , e : Float
    , g00 : Float
    , g10 : Float
    , g01 : Float
    , g11 : Float
    }
    ->
        { param0 : Float
        , param1 : Float
        }
computeMinimumParameters cfg =
    let
        ( x0, y0 ) =
            ( cfg.end00, cfg.end01 )

        ( x1, y1 ) =
            ( cfg.end10, cfg.end11 )

        delta =
            y1 - y0

        h0 =
            delta * (-cfg.b * x0 + cfg.c * y0 - cfg.e)
    in
    if h0 >= 0 then
        if cfg.edge0 == 0 then
            { param0 = 0
            , param1 = getClampedRoot cfg.c cfg.g00 cfg.g01
            }

        else if cfg.edge0 == 1 then
            { param0 = 1
            , param1 = getClampedRoot cfg.c cfg.g10 cfg.g11
            }

        else
            { param0 = x0
            , param1 = y0
            }

    else
        let
            h1 =
                delta * (-cfg.b * x1 + cfg.c * y1 - cfg.e)
        in
        if h1 <= 0 then
            if cfg.edge1 == 0 then
                { param0 = 0
                , param1 = getClampedRoot cfg.c cfg.g00 cfg.g01
                }

            else if cfg.edge1 == 1 then
                { param0 = 1
                , param1 = getClampedRoot cfg.c cfg.g10 cfg.g11
                }

            else
                { param0 = x1
                , param1 = y1
                }

        else
            let
                z =
                    min (max (h0 / (h0 - h1)) 0) 1

                omz =
                    1 - z
            in
            { param0 = omz * x0 + z * x1
            , param1 = omz * y0 + z * y1
            }


midIfOutOfBounds : Float -> Float
midIfOutOfBounds n =
    if n < 0 || n > 1 then
        0.5

    else
        n


computeIntersection :
    { sValue0 : Float
    , sValue1 : Float
    , classify0 : Float
    , classify1 : Float
    , b : Float
    , f00 : Float
    , f10 : Float
    }
    ->
        { edge0 : Float
        , edge1 : Float
        , end00 : Float
        , end01 : Float
        , end10 : Float
        , end11 : Float
        }
computeIntersection cfg =
    if cfg.classify0 < 0 then
        let
            ( edge1, end10, end11 ) =
                if cfg.classify1 == 0 then
                    ( 3, cfg.sValue1, 1 )

                else
                    ( 1, 1, midIfOutOfBounds (cfg.f10 / cfg.b) )
        in
        { edge0 = 0
        , edge1 = edge1
        , end00 = 0
        , end01 = midIfOutOfBounds (cfg.f00 / cfg.b)
        , end10 = end10
        , end11 = end11
        }

    else if cfg.classify0 == 0 then
        let
            ( edge1, end10, end11 ) =
                if cfg.classify1 < 0 then
                    ( 0, 0, midIfOutOfBounds (cfg.f00 / cfg.b) )

                else if cfg.classify1 == 0 then
                    ( 3, cfg.sValue1, 1 )

                else
                    ( 1, 1, midIfOutOfBounds (cfg.f10 / cfg.b) )
        in
        { edge0 = 2
        , edge1 = edge1
        , end00 = cfg.sValue0
        , end01 = 0
        , end10 = end10
        , end11 = end11
        }

    else
        let
            ( edge1, end10, end11 ) =
                if cfg.classify1 == 0 then
                    ( 3, cfg.sValue1, 1 )

                else
                    ( 0, 0, midIfOutOfBounds (cfg.f00 / cfg.b) )
        in
        { edge0 = 1
        , edge1 = edge1
        , end00 = 1
        , end01 = midIfOutOfBounds (cfg.f10 / cfg.b)
        , end10 = end10
        , end11 = end11
        }


{-| Find the nearest points between two line segments. Returns `Nothing` if NaN or infinity would otherwise come up in the result.
-}
nearestPoints :
    LineSegment3d u c
    -> LineSegment3d u c
    -> Maybe { nearestOnFirstLine : Point3d u c, nearestOnSecondLine : Point3d u c }
nearestPoints p q =
    -- Ported from: <https://www.geometrictools.com/Documentation/DistanceLine3Line3.pdf>
    let
        p0 =
            LineSegment3d.startPoint p |> Point3d.toTuple Quantity.unwrap

        p1 =
            LineSegment3d.endPoint p |> Point3d.toTuple Quantity.unwrap

        q0 =
            LineSegment3d.startPoint q |> Point3d.toTuple Quantity.unwrap

        q1 =
            LineSegment3d.endPoint q |> Point3d.toTuple Quantity.unwrap

        p1mp0 =
            sub p1 p0

        q1mq0 =
            sub q1 q0

        p0mq0 =
            sub p0 q0

        a =
            dot p1mp0 p1mp0

        b =
            dot p1mp0 q1mq0

        c =
            dot q1mq0 q1mq0

        d =
            dot p1mp0 p0mq0

        e =
            dot q1mq0 p0mq0

        f00 =
            d

        f10 =
            f00 + a

        f01 =
            f00 - b

        f11 =
            f10 - b

        g00 =
            -e

        g10 =
            g00 - b

        g01 =
            g00 + c

        g11 =
            g10 + c

        { param0, param1 } =
            if a > 0 && c > 0 then
                let
                    sValue0 =
                        getClampedRoot a f00 f10

                    sValue1 =
                        getClampedRoot a f01 f11

                    classify0 =
                        classify sValue0

                    classify1 =
                        classify sValue1
                in
                if classify0 == -1 && classify1 == -1 then
                    { param0 = 0
                    , param1 = getClampedRoot c g00 g01
                    }

                else if classify0 == 1 && classify1 == 1 then
                    { param0 = 1
                    , param1 = getClampedRoot c g10 g11
                    }

                else
                    let
                        i =
                            computeIntersection
                                { sValue0 = sValue0
                                , sValue1 = sValue1
                                , classify0 = classify0
                                , classify1 = classify1
                                , b = b
                                , f00 = f00
                                , f10 = f10
                                }

                        min =
                            computeMinimumParameters
                                { edge0 = i.edge0
                                , edge1 = i.edge1
                                , end00 = i.end00
                                , end01 = i.end01
                                , end10 = i.end10
                                , end11 = i.end11
                                , b = b
                                , c = c
                                , e = e
                                , g00 = g00
                                , g10 = g10
                                , g01 = g01
                                , g11 = g11
                                }
                    in
                    { param0 = min.param0
                    , param1 = min.param1
                    }

            else if a > 0 then
                { param0 = getClampedRoot a f00 f10
                , param1 = 0
                }

            else if c > 0 then
                { param0 = 0
                , param1 = getClampedRoot c g00 g01
                }

            else
                { param0 = 0
                , param1 = 0
                }

        p2 =
            add (scale (1 - param0) p0) (scale param0 p1)

        q2 =
            add (scale (1 - param1) q0) (scale param1 q1)
    in
    if isInvalid p2 || isInvalid q2 then
        Nothing

    else
        Just
            { nearestOnFirstLine = Point3d.fromTuple Quantity.unsafe p2
            , nearestOnSecondLine = Point3d.fromTuple Quantity.unsafe q2
            }
