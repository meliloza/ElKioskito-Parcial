
%turno(Persona,Dia,HorarioI, HorarioF).
turno(dodain,lunes,9, 15).
turno(dodain,miercoles,9, 15).
turno(dodain,viernes,9, 15).

turno(juanC,sabados,18,22).
turno(juanC,domingos,18,22).

turno(lucas,martes,10,20).

turno(juanFDs,jueves,10,20).
turno(juanFDs,viernes,12,20).

turno(leoC,lunes,14,18).
turno(leoC,miercoles,14,18).

turno(martu,miercoles,23,24).


turno(vale,Dia,HorarioInicio,HorarioFinal):-
    turno(dodain,Dia,HorarioInicio,HorarioFinal).

turno(vale,Dia,HorarioInicio,HorarioFinal):-
    turno(juanC,Dia,HorarioInicio,HorarioFinal).


% "nadie hace el mismo horario que leoC"
%Por principio del universo cerrado, no es necesario agregar a la base de conocimientos cosas que no aportan nada

%maiu está pensando si hace el horario de 0 a 8 los martes y miércoles
%Por principio del universo cerrado, vemos que todo lo que es desconocido, se toma como falso.


atiendeElKiosko(Persona,Dia,Horario):-
    turno(Persona,Dia,HorarioInicio,HorarioFinal),
    between(HorarioInicio, HorarioFinal, Horario).
    

estaForeverAlone(Persona,Dia,Horario):-
    atiendeElKiosko(Persona,Dia,Horario),
    not(noEstaSolo(Persona,Dia,Horario)).


noEstaSolo(Persona,Dia,Horario):-
    atiendeElKiosko(Persona,Dia,Horario),
    findall(Kioskero, atiendeElKiosko(Kioskero,Dia,Horario), ListaKioskeros),
    length(ListaKioskeros, Cantidad), 
    Cantidad > 1.
    

%ESTO ESTA MAL PLANTEADO 
%atencion(Persona,Dia,_):-
%    turno(Persona,Dia,_,_),
%    forall(turno(Persona,Dia,_,_), atiendeElKiosko(Persona,Dia,_)).
    
    
%   venta(Persona,Fecha(dia,mes),Productos)

venta(dodain,fecha(10,8),[golosina(1200),cigarrillo([hockey]),golosina(50)]).
venta(dodain, fecha(12,8), [bebida(alcoholica,8),bebida(noAlcoholica,1),golosina(10)]).
venta(martu, fecha(12,8),[golosina(1000),cigarrillo([chesterfield,colorado,parisiennes])]).
venta(lucas, fecha(11,8),[golosina(600)]).
venta(lucas, fecha(18,8), [bebida(noAlcoholica,2),cigarrillo([derby])]).

% Queremos saber si una persona vendedora es suertuda, esto ocurre si para todos los días en los que vendió,
% la primera venta que hizo fue importante. Una venta es importante:
% - en el caso de las golosinas, si supera los $ 100.
% - en el caso de los cigarrillos, si tiene más de dos marcas.
% - en el caso de las bebidas, si son alcohólicas o son más de 5.


esSuertudo(Persona):-
    vendedora(Persona),
    forall(venta(Persona,_,[Producto|_]), ventaImportante(Producto)).

vendedora(Persona):-venta(Persona,_,_).

ventaImportante(golosina(precio)):- precio > 100.
ventaImportante(cigarrillo(Marcas)):-length(Marcas, Cantidad), Cantidad > 2. 
ventaImportante(bebida(Tipo,_)):- Tipo is alcoholica. 
ventaImportante(bebida(_,Cantidad)):- Cantidad >5. 


 













/*  ESTO ESTA MAL!! porque no puedo contemplar los dos a la vez para vale, 
    primero porque el dia de dodain no va a coincidir con el dia de juanC para asignarseo a el dia de vale

turno(vale,Dia,HorarioInicio,HorarioFinal):-
    turno(dodain,Dia,HorarioInicio,HorarioFinal),
    turno(juanC,Dia,HorarioInicio,HorarioFinal).

*/





































