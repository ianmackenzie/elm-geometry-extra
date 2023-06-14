module QuadraticSpline2d.Extra exposing (nearestPoint)

{-| Extra functionality for `QuadraticSpline2d` values.

@docs nearestPoint

-}

import Point2d exposing (Point2d)
import QuadraticSpline2d exposing (QuadraticSpline2d)


{-| Find the parameter value on a spline that is closest to a given point. You can combine this with
`pointOn` to find the nearest point on the spline.

    QuadraticSpline2d.nearestPoint spline p0
        |> QuadraticSpline2d.pointOn spline

-}
nearestPoint :
    QuadraticSpline2d units coordinates
    -> Point2d units coordinates
    -> Float
nearestPoint spline point =
    -- Original code found here http://www.gludion.com/blog/bezier_distance.zip on this blog post https://blog.gludion.com/2009/08/distance-to-quadratic-bezier-curve.html
    let
        { x, y } =
            Point2d.unwrap point

        p0 =
            QuadraticSpline2d.firstControlPoint spline |> Point2d.unwrap

        p1 =
            QuadraticSpline2d.secondControlPoint spline |> Point2d.unwrap

        p2 =
            QuadraticSpline2d.thirdControlPoint spline |> Point2d.unwrap

        aCap =
            { x = p1.x - p0.x
            , y = p1.y - p0.y
            }

        bCap =
            { x = p0.x - 2 * p1.x + p2.x
            , y = p0.y - 2 * p1.y + p2.y
            }

        pos =
            { x = p0.x - x, y = p0.y - y }

        a =
            bCap.x * bCap.x + bCap.y * bCap.y

        b =
            3 * (aCap.x * bCap.x + aCap.y * bCap.y)

        c =
            2 * (aCap.x * aCap.x + aCap.y * aCap.y) + pos.x * bCap.x + pos.y * bCap.y

        d =
            pos.x * aCap.x + pos.y * aCap.y

        sol =
            thirdDegreeEquation a b c d

        d0 =
            getDist x y p0.x p0.y

        d2 =
            getDist x y p2.x p2.y

        maybeResult =
            List.filterMap
                (\t ->
                    if t >= 0 && t <= 1 then
                        let
                            pos2 =
                                getPos p0 p1 p2 t

                            dist =
                                getDist x y pos2.x pos2.y
                        in
                        if dist < d0 && dist < d2 then
                            { tMin = t
                            , distMin = dist
                            , posMin = pos2
                            }
                                |> Just

                        else
                            Nothing

                    else
                        Nothing
                )
                sol
                |> minimumBy .distMin
    in
    case maybeResult of
        Just { tMin, distMin, posMin } ->
            tMin

        Nothing ->
            if d0 < d2 then
                0

            else
                1


{-| Find the first minimum element in a list using a comparable transformation
-}
minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy f ls =
    -- Copied from elm-community/list-extra
    let
        minBy x ( y, fy ) =
            let
                fx =
                    f x
            in
            if fx < fy then
                ( x, fx )

            else
                ( y, fy )
    in
    case ls of
        [ l_ ] ->
            Just l_

        l_ :: ls_ ->
            Just <| Tuple.first <| List.foldl minBy ( l_, f l_ ) ls_

        _ ->
            Nothing


getDist : Float -> Float -> Float -> Float -> Float
getDist x1 y1 x2 y2 =
    sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))


getPos : { x : Float, y : Float } -> { x : Float, y : Float } -> { x : Float, y : Float } -> Float -> { x : Float, y : Float }
getPos p0 p1 p2 t =
    let
        a =
            (1 - t) * (1 - t)

        b =
            2 * t * (1 - t)

        c =
            t * t
    in
    { x = a * p0.x + b * p1.x + c * p2.x
    , y = a * p0.y + b * p1.y + c * p2.y
    }


zeroMax : Float
zeroMax =
    0.0000001


thirdDegreeEquation : Float -> Float -> Float -> Float -> List Float
thirdDegreeEquation a b c d =
    if abs a > zeroMax then
        let
            a2 =
                b / a

            b2 =
                c / a

            c2 =
                d / a

            p =
                b2 - a2 * a2 / 3

            q =
                a2 * (2 * a2 * a2 - 9 * b2) / 27 + c2

            p3 =
                p * p * p

            dCap =
                q * q + 4 * p3 / 27

            offset =
                -a2 / 3
        in
        if dCap > zeroMax then
            let
                z =
                    sqrt dCap

                u =
                    (-q + z) / 2

                v =
                    (-q - z) / 2

                u2 =
                    if u >= 0 then
                        u ^ (1 / 3)

                    else
                        -(-u ^ (1 / 3))

                v2 =
                    if v >= 0 then
                        v ^ (1 / 3)

                    else
                        -(-v ^ (1 / 3))
            in
            [ u2 + v2 + offset ]

        else if dCap < -zeroMax then
            let
                u =
                    2 * sqrt (-p / 3)

                v =
                    acos -(sqrt (-27 / p3) * q / 2) / 3
            in
            [ u * cos v + offset
            , u * cos (v + 2 * pi / 3) + offset
            , u * cos (v + 4 * pi / 3) + offset
            ]

        else
            let
                u =
                    if q < 0 then
                        (-q / 2) ^ (1 / 3)

                    else
                        -((q / 2) ^ (1 / 3))
            in
            [ 2 * u + offset, -u + offset ]

    else
        let
            a2 =
                b

            b2 =
                c

            c2 =
                d
        in
        if abs a2 <= zeroMax then
            if abs b2 <= zeroMax then
                []

            else
                [ -c2 / b2 ]

        else
            let
                dCap =
                    b2 * b2 - 4 * a2 * c2
            in
            if dCap <= -zeroMax then
                []

            else if dCap > zeroMax then
                let
                    dCap2 =
                        sqrt dCap
                in
                [ (-b2 - dCap2) / (2 * a2)
                , (-b2 + dCap2) / (2 * a2)
                ]

            else if dCap < -zeroMax then
                []

            else
                [ -b2 / (2 * a2) ]
