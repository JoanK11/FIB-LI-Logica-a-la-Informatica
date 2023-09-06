programa(['begin'|P]):- append(I, ['end'], P), instrucciones(I), write('yes'), !.
programa(_):- write('no').

instrucciones(I):- instruccion(I).
instrucciones([I,';'|LI]):- instruccion(I), instrucciones(LI).

instruccion([V1, '=', V2, '+', V3]):- variable(V1), variable(V2), variable(V3).
instruccion(['if', V1, '=', V2, 'then', LI1, 'else', LI2, 'endif']):- variable(V1), variable(V2), instrucciones(LI1), instrucciones(LI2).

variable('x').
variable('y').
variable('z').
