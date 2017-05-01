module Stack exposing (..)


type Stack a
    = Stack (List a)



-- Constructors


empty : Stack a
empty =
    Stack []


fromList : List a -> Stack a
fromList =
    Stack



-- Basic Operations


push : a -> Stack a -> ( Maybe a, Stack a )
push item (Stack state) =
    ( Nothing, Stack <| item :: state )


pop : Stack a -> ( Maybe a, Stack a )
pop (Stack state) =
    case state of
        [] ->
            ( Nothing, Stack [] )

        head :: tail ->
            ( Just head, Stack tail )


top : Stack a -> ( Maybe a, Stack a )
top ((Stack state) as stack) =
    ( List.head state, stack )



-- Quering


isEmpty : Stack a -> Bool
isEmpty (Stack state) =
    List.isEmpty state


length : Stack a -> Int
length (Stack state) =
    List.length state



-- Transformations


toList : Stack a -> List a
toList (Stack list) =
    list



-- Functor


map : (a -> b) -> Stack a -> Stack b
map fc (Stack state) =
    Stack <| List.map fc state


map2 : (a -> b -> c) -> Stack a -> Stack b -> Stack c
map2 fc (Stack state1) (Stack state2) =
    Stack <| List.map2 fc state1 state2


filter : (a -> Bool) -> Stack a -> Stack a
filter fc (Stack state) =
    Stack <| List.filter fc state



-- Applicative


singleton : a -> Stack a
singleton a =
    Stack [ a ]


andMap : Stack a -> Stack (a -> b) -> Stack b
andMap s fs =
    map2 (<|) fs s



-- Monad


flatten : Stack (Stack a) -> Stack a
flatten (Stack state) =
    Stack <| List.concatMap toList state


andThen : (a -> Stack b) -> Stack a -> Stack b
andThen fc =
    flatten << map fc
