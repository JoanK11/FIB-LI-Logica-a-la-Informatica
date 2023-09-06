solucio( SOL ) :-
    SOL = [ [1, C1, Pr1, A1, B1, Pa1], % numcasa, color, profressió, animal, bebida, pais
            [2, C2, Pr2, A2, B2, Pa2],
            [3, C3, Pr3, A3, B3, Pa3],
            [4, C4, Pr4, A4, B4, Pa4],
            [5, C5, Pr5, A5, B5, Pa5] ],

            member([_, vermell, _, _, _, peru], SOL),   % 1 - El que vive en la casa roja es de Perú
            member([_, _, _, perro, _, frances], SOL),  % 2 - Al francés le gusta el perro
            member([_, _, pintor, _, _, japones], SOL), % 3 - El pintor es japonés
            member([_, _, _, _, ron, chino], SOL),      % 4 - Al chino le gusta el ron
            member([1, _, _, _, _, hungaro], SOL),      % 5 - El húngaro vive en la primera casa
            member([_, verde, _, _, coñac, _], SOL),    % 6 - Al de la casa verde le gusta el coñac

            % 7 - La casa verde está justo a la izquierda de la blanca
            member([PosVerde, verde, _, _, _, _], SOL), PosBlanca is PosVerde + 1,
            member([PosBlanca, blanco, _, _, _, _], SOL),

            member([_, _, escultor, caracoles, _, _], SOL), % 8 - El escultor crı́a caracoles
            member([_, amarillo, actor, _, _, _], SOL), % 9 - El de la casa amarilla es actor
            member([3, _, _, _, cava, _], SOL), % 10 - El de la tercera casa bebe cava

            % 11 - El que vive al lado del actor tiene un caballo
            member([PosActor, _, actor, _, _, _], SOL),
            PosActorL is PosActor - 1, PosActorR is PosActor + 1,
            (member([PosActorL, _, _, caballo, _, _], SOL) ; member([PosActorR, _, _, caballo, _, _], SOL)),

            % 12 - El húngaro vive al lado de la casa azul
            member([PosHungaro, _, _, _, _, hungaro], SOL),
            PosHungaroL is PosHungaro - 1, PosHungaroR is PosHungaro + 1,
            (member([PosHungaroL, azul, _, _, _, _], SOL) ; member([PosHungaroR, azul, _, _, _, _], SOL)),

            member([_, _, notario, _, whisky, _], SOL), % 13 - Al notario la gusta el whisky

            % 14 - El que vive al lado del médico tiene un ardilla
            member([PosMedico, _, medico, _, _, _], SOL),
            PosMedicoL is PosMedico - 1, PosMedicoR is PosMedico + 1,
            (member([PosMedicoL, _, _, ardilla, _, _], SOL) ; member([PosMedicoR, _, _, ardilla, _, _], SOL)).
