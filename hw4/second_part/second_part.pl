classify(SepalLength, SepalWidth, PetalLength, PetalWidth, Class) :-
    (PetalWidth =< 0.8 -> Class = 'Iris-setosa';
    PetalWidth > 0.8 -> 
        (PetalWidth =< 1.75 -> 
            (PetalLength =< 4.95 -> 
                (PetalWidth =< 1.65 -> Class = 'Iris-versicolor'; Class = 'Iris-virginica')
            ;
            PetalLength > 4.95 -> 
                (PetalWidth =< 1.55 -> Class = 'Iris-virginica';
                PetalWidth > 1.55 -> 
                    (PetalLength =< 5.45 -> Class = 'Iris-versicolor'; Class = 'Iris-virginica')
                )
            )
        ;
        PetalWidth > 1.75 -> Class = 'Iris-virginica'
        )
    ).
